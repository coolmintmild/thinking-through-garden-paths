# 06_stats_reliability.R
# ===========================================================================
# Inter-coder reliability for the trace coding (Methods 3.4).
#
# Coders, all stored as columns of results/trials.xlsx:
#   primary coder  trace_A ... trace_D        (Gemini 3.6 Flash; A deterministic)
#   second coder   trace_A_coder2 ... _coder2 (GPT-5.6 Terra, same prompts)
#   human coder    trace_A_human ... _human   (513-trace validation sample, 5%)
#
# Universe: high-thinking traces passing the 0.15 visibility filter
# (trace_included = 1, N = 10,225). The human sample is a random 5 per cent
# of that universe. Values outside the valid code range (B/C = 3, D = 2,
# blanks) are undecidable and excluded pairwise per comparison.
#
# Reported: raw percent agreement, unweighted Cohen's kappa, and nominal
# Krippendorff's alpha, for the raw codes and the derived analysis contrasts
# (B detection, B' resolution, C pursuit, C' persistence). Code A is
# deterministic pattern matching, identical for both LLM pipelines, so it is
# reported only for the human validation.
# Software: R 4.3.3, readxl.
# ===========================================================================

suppressMessages({library(readxl)})
TRIALS <- "../results/trials.xlsx"
raw <- as.data.frame(read_excel(TRIALS, sheet = "trials"))
stopifnot(nrow(raw) == 23040)

u <- raw[raw$trace_included %in% c(1, TRUE, "TRUE", "True"), ]
stopifnot(nrow(u) == 10225)
hsub <- u[!is.na(u$trace_A_human), ]
stopifnot(nrow(hsub) == 513)

clean <- function(x, valid) ifelse(x %in% valid, as.integer(x), NA_integer_)
bin <- function(x, one, zero) ifelse(x %in% one, 1L, ifelse(x %in% zero, 0L, NA_integer_))

prepare_pair <- function(df, left, right) {
  g <- function(side, code) df[[paste0("trace_", code, side)]]
  d <- data.frame(
    A1 = clean(g(left, "A"), 0:1), A2 = clean(g(right, "A"), 0:1),
    B1 = clean(g(left, "B"), 0:2), B2 = clean(g(right, "B"), 0:2),
    C1 = clean(g(left, "C"), 0:2), C2 = clean(g(right, "C"), 0:2),
    D1 = clean(g(left, "D"), 0:1), D2 = clean(g(right, "D"), 0:1))
  d$Bdet1 <- bin(d$B1, c(1, 2), 0); d$Bdet2 <- bin(d$B2, c(1, 2), 0)
  d$Bp1   <- bin(d$B1, 1, 2);       d$Bp2   <- bin(d$B2, 1, 2)
  d$Cpur1 <- bin(d$C1, c(1, 2), 0); d$Cpur2 <- bin(d$C2, c(1, 2), 0)
  d$Cp1   <- bin(d$C1, 2, 1);       d$Cp2   <- bin(d$C2, 2, 1)
  d
}

rel <- function(a, b) {
  ok <- !is.na(a) & !is.na(b); a <- a[ok]; b <- b[ok]
  if (!length(a)) return(data.frame(n = 0L, agree_pct = NA, kappa = NA, kripp_a = NA))
  categories <- sort(unique(c(a, b)))
  po <- mean(a == b)
  ml <- table(factor(a, levels = categories)) / length(a)
  mr <- table(factor(b, levels = categories)) / length(b)
  pe <- sum(ml * mr)
  kappa <- if (pe == 1) NA_real_ else (po - pe) / (1 - pe)
  pooled <- table(factor(c(a, b), levels = categories)); tot <- 2 * length(a)
  pea <- sum(pooled * (pooled - 1)) / (tot * (tot - 1))
  alpha <- if (pea == 1) NA_real_ else 1 - (1 - po) / (1 - pea)
  data.frame(n = length(a), agree_pct = round(100 * po, 1),
             kappa = round(kappa, 3), kripp_a = round(alpha, 3))
}

COMPARISONS <- list(
  "A (binary)"               = c("A1", "A2"),
  "B (tertiary 0/1/2)"       = c("B1", "B2"),
  "C (tertiary 0/1/2)"       = c("C1", "C2"),
  "D (binary)"               = c("D1", "D2"),
  "B detection (1-2 vs 0)"   = c("Bdet1", "Bdet2"),
  "B' resolution (detected)" = c("Bp1", "Bp2"),
  "C pursuit (1-2 vs 0)"     = c("Cpur1", "Cpur2"),
  "C' persistence (pursued)" = c("Cp1", "Cp2"))

run_comparisons <- function(d, label, include_a) {
  defs <- if (include_a) COMPARISONS else COMPARISONS[-1]
  out <- do.call(rbind, lapply(names(defs), function(code) {
    v <- defs[[code]]
    cbind(code = code, rel(d[[v[1]]], d[[v[2]]]), stringsAsFactors = FALSE)
  }))
  cat("\n", label, "\n", sep = "")
  print(out, row.names = FALSE)
  invisible(out)
}

confusions <- function(d, left_label, right_label, codes) {
  cat("\nConfusion matrices (rows = ", left_label, ", cols = ", right_label, ")\n", sep = "")
  for (code in codes) {
    cat("\n-- ", code, " --\n", sep = "")
    print(table(d[[paste0(code, "1")]], d[[paste0(code, "2")]],
                dnn = c(left_label, right_label), useNA = "no"))
  }
}

cat("Inter-coder reliability: LLM coders and the human-validation sample\n")
cat("Raw agreement, unweighted Cohen's kappa, and nominal Krippendorff's alpha.\n")
cat("Undecidable, blank, and out-of-range values are excluded pairwise.\n")
cat("B' and C' are conditioned on both coders detecting/pursuing, respectively.\n\n")
cat("Eligible universe (visibility-filtered traces): N =", nrow(u), "\n")
cat("Human-coded validation sample: N =", nrow(hsub),
    sprintf("(%.2f%% of the universe)\n", 100 * nrow(hsub) / nrow(u)))
inval <- function(df, side) sprintf("A=%d, B=%d, C=%d, D=%d",
  sum(!(df[[paste0("trace_A", side)]] %in% 0:1)),
  sum(!(df[[paste0("trace_B", side)]] %in% 0:2)),
  sum(!(df[[paste0("trace_C", side)]] %in% 0:2)),
  sum(!(df[[paste0("trace_D", side)]] %in% 0:1)))
cat("Undecidable/invalid values, human coder on the sample:", inval(hsub, "_human"), "\n")
cat("Undecidable/invalid values, primary coder on the sample:", inval(hsub, ""), "\n")
cat("Undecidable/invalid values, second coder on the sample:", inval(hsub, "_coder2"), "\n")

d_model <- prepare_pair(u, "", "_coder2")
d_hg    <- prepare_pair(hsub, "_human", "")
d_hp    <- prepare_pair(hsub, "_human", "_coder2")

run_comparisons(d_model, "PRIMARY (GEMINI 3.6 FLASH) VS SECOND CODER (GPT-5.6 TERRA)", include_a = FALSE)
confusions(d_model, "Primary", "Second", c("B", "C", "D"))
run_comparisons(d_hg, "HUMAN CODER VS PRIMARY CODER", include_a = TRUE)
confusions(d_hg, "Human", "Primary", c("A", "B", "C", "D"))
run_comparisons(d_hp, "HUMAN CODER VS SECOND CODER", include_a = TRUE)
confusions(d_hp, "Human", "Second", c("A", "B", "C", "D"))
