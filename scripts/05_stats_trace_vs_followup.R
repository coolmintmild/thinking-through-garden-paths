# 05_stats_trace_vs_followup.R
# ===========================================================================
# Q1 trace vs Q2 follow-up comparison (Results section
# "Comparing the Trace with the Follow-Up Explanation"; Supplemental File S1, Section S4).
# The stages run in order; each later part uses the fits of Part 3.
#   Part 0  Derive the two analysis tables from results/trials.xlsx
#   Part 1  Cell-level similarity: Wilcoxon signed-rank, Lin's CCC
#   Part 2  Cell counts and sparse flags
#   Part 3  Mixed models at the retained rung of the fallback ladder
#   Part 4  Likelihood-ratio tests on every source-containing term
#   Part 5  Post hoc contrasts at the highest significant order
# Notes. The retained rung per code (RETAINED below) was established by the
# original ladder run: fit the full four-way model, and descend (drop the
# four-way, then the three-ways) until the largest fixed-effect SE is <= 5.
# The mixed-model fits are the slow part (hours in total on a laptop).
# Software: R 4.3.3, lme4 1.1-35.5, emmeans 1.11.2.
# ===========================================================================

suppressMessages({library(lme4); library(emmeans); library(readxl)})

# ---------------------------------------------------------------------------
# Part 0: derive the analysis tables
# ---------------------------------------------------------------------------
TRIALS <- "../results/trials.xlsx"
raw <- as.data.frame(read_excel(TRIALS, sheet = "trials"))
stopifnot(nrow(raw) == 23040)

bin01 <- function(x, one, zero) ifelse(x %in% one, 1, ifelse(x %in% zero, 0, NA))
# six binary codes from raw code columns (same rules as 04_stats_trace_codes.R)
six <- function(A, B, C, D) data.frame(
  A     = as.numeric(A),
  B     = bin01(B, c(1,2), 0),
  Bres  = bin01(B, 1, 2),
  C     = bin01(C, c(1,2), 0),
  Cpers = bin01(C, 2, 1),
  D     = bin01(D, 1, 0))

key <- c("model","itemno","condition","trial")
hi  <- raw[raw$thinking_condition == "high", ]

# Q1trace side: high-thinking trials passing the trace filter, Yes/No answers
tr  <- hi[hi$trace_included %in% c(1, TRUE, "TRUE", "True") & hi$q1_answer_norm %in% c("Yes","No"), ]
q1t <- cbind(tr[key], q1_answer = tr$q1_answer_norm, gp3 = tr$condition,
             gp2 = ifelse(tr$condition == "GP", "GP", "NGP"), verb = tr$verb_type,
             six(tr$trace_A, tr$trace_B_coder1, tr$trace_C_coder1, tr$trace_D_coder1), source = "Q1trace")
stopifnot(nrow(q1t) == 10216)

# Q2high side: follow-up codes of all high-thinking trials with a normalised
# Yes/No answer (no trace filter)
hi  <- hi[hi$q1_answer_norm %in% c("Yes","No"), ]
q2h <- cbind(hi[key], q1_answer = hi$q1_answer_norm, gp3 = hi$condition,
             gp2 = ifelse(hi$condition == "GP", "GP", "NGP"), verb = hi$verb_type,
             six(hi$followup_A, hi$followup_B, hi$followup_C, hi$followup_D), source = "Q2high")
stopifnot(nrow(q2h) == 11511)

TRIALS_LONG <- rbind(q1t, q2h)
TRIALS_LONG$itemno <- as.integer(TRIALS_LONG$itemno)
TRIALS_LONG$source <- factor(TRIALS_LONG$source, levels = c("Q1trace", "Q2high"))
TRIALS_LONG$answer <- factor(TRIALS_LONG$q1_answer, levels = c("No", "Yes"))
TRIALS_LONG$gp2    <- factor(TRIALS_LONG$gp2,  levels = c("GP", "NGP"))
TRIALS_LONG$verb   <- factor(TRIALS_LONG$verb, levels = c("OT", "RAT"))
TRIALS_LONG$item   <- factor(TRIALS_LONG$itemno)
TRIALS_LONG$model  <- factor(TRIALS_LONG$model)
for (v in c("source", "answer", "gp2", "verb"))
  contrasts(TRIALS_LONG[[v]]) <- contr.sum(2) / 2      # sum coding +/- 0.5

# Q2base side, for the cell-level table: follow-up codes of the baseline trial
# paired row-wise to each high trial by trial number
ba  <- raw[raw$thinking_condition == "no" & raw$q1_answer_norm %in% c("Yes","No"), ]
q2b <- cbind(ba[key], six(ba$followup_A, ba$followup_B, ba$followup_C, ba$followup_D))
pair <- merge(hi[key], q2b, by = key, all.x = TRUE, sort = FALSE)

# cell-level rates: one row per item x model x condition (24 x 16 x 3 = 1152)
rate_tab <- function(df, src) {
  ag <- aggregate(df[c("A","B","Bres","C","Cpers","D")],
                  by = df[c("model","itemno","condition")],
                  FUN = function(x) mean(x, na.rm = TRUE))
  nn <- aggregate(df[c("A","B","Bres","C","Cpers","D")],
                  by = df[c("model","itemno","condition")],
                  FUN = function(x) sum(!is.na(x)))
  names(ag)[4:9] <- paste0(names(ag)[4:9], "_", src, "_rate")
  names(nn)[4:9] <- paste0(names(nn)[4:9], "_", src, "_n")
  merge(ag, nn, by = c("model","itemno","condition"))
}
CELLS <- Reduce(function(a, b) merge(a, b, by = c("model","itemno","condition"), all = TRUE),
                list(rate_tab(q1t, "Q1trace"), rate_tab(q2h, "Q2high"), rate_tab(pair, "Q2base")))
CELLS[sapply(CELLS, is.nan)] <- NA
stopifnot(nrow(CELLS) == 1152)

CODES <- c("A", "B", "Bres", "C", "Cpers", "D")
SE_THRESHOLD <- 5
SPARSE_MIN   <- 40
code_data <- function(d, cd) { s <- d[!is.na(d[[cd]]), ]; s$y <- s[[cd]]; s }
max_se <- function(m) max(summary(m)$coefficients[, "Std. Error"])
FORMULAS <- list(
  full = y ~ source * answer * gp2 * verb + (1 | item) + (1 | model),
  no4  = y ~ (source + answer + gp2 + verb)^3 + (1 | item) + (1 | model),
  no3  = y ~ (source + answer + gp2 + verb)^2 + (1 | item) + (1 | model))
RUNG_LABEL <- c(full = "full four-way", no4 = "four-way dropped", no3 = "three-ways dropped")
RETAINED <- c(A = "no4", B = "full", Bres = "no3", C = "no3", Cpers = "no4", D = "no4")

# ---------------------------------------------------------------------------
# Part 1: cell-level similarity (Wilcoxon, Lin's CCC)
# ---------------------------------------------------------------------------
d <- CELLS
codes <- c("A", "B", "Bres", "C", "Cpers", "D")

cat("=== Question 1 ===\n")
cat("Unit: item x model x condition cells derived from results/trials.xlsx\n")
cat("Rows (cells):", nrow(d), "\n")
cat("Unique itemno x model x gp3:",
    nrow(unique(d[, c("itemno", "model", "gp3")])), "\n")
cat("No split by condition, verb or answer.\n\n")

# Lin's concordance correlation coefficient
ccc <- function(x, y) {
  ok <- !is.na(x) & !is.na(y)
  x <- x[ok]; y <- y[ok]
  n <- length(x)
  if (n < 3) return(NA_real_)
  mx <- mean(x); my <- mean(y)
  # population (1/n) moments, as in Lin (1989)
  vx <- var(x) * (n - 1) / n
  vy <- var(y) * (n - 1) / n
  sxy <- cov(x, y) * (n - 1) / n
  den <- vx + vy + (mx - my)^2
  if (den == 0) return(NA_real_)
  2 * sxy / den
}

res <- data.frame()

for (cd in codes) {
  r1 <- d[[paste0(cd, "_Q1trace_rate")]]
  rh <- d[[paste0(cd, "_Q2high_rate")]]
  rb <- d[[paste0(cd, "_Q2base_rate")]]

  d1 <- abs(r1 - rh)
  d2 <- abs(rb - rh)

  ok <- !is.na(d1) & !is.na(d2)
  n_cells <- sum(ok)
  D1 <- d1[ok]; D2 <- d2[ok]

  ties <- sum(D1 == D2)
  n_contrib <- n_cells - ties

  w <- suppressWarnings(wilcox.test(D1, D2, paired = TRUE, exact = FALSE))

  pct_d2_lt_d1 <- 100 * mean(D2 < D1)

  c1 <- ccc(r1, rh)
  c2 <- ccc(rb, rh)

  closer <- if (mean(D1) < mean(D2)) "Q1trace" else if (mean(D1) > mean(D2)) "Q2base" else "tie"

  cat("---------------- code", cd, "----------------\n")
  cat("cells with both distances defined:", n_cells, "  (of", nrow(d), ")\n")
  cat("tied cells (d1 == d2):", ties, "   contributing cells:", n_contrib, "\n")
  cat("mean   d1 =", round(mean(D1), 4), "   mean   d2 =", round(mean(D2), 4), "\n")
  cat("median d1 =", round(median(D1), 4), "   median d2 =", round(median(D2), 4), "\n")
  cat("sd     d1 =", round(sd(D1), 4), "   sd     d2 =", round(sd(D2), 4), "\n")
  cat("% cells with d2 < d1:", round(pct_d2_lt_d1, 1), "\n")
  cat("% cells with d1 < d2:", round(100 * mean(D1 < D2), 1), "\n")
  cat("Wilcoxon signed-rank (paired, d1 vs d2): V =", unname(w$statistic),
      "  p =", format.pval(w$p.value, digits = 4, eps = 1e-300), "\n")
  cat("CCC(Q1trace, Q2high) =", round(c1, 4), "\n")
  cat("CCC(Q2base,  Q2high) =", round(c2, 4), "\n")
  cat("closer to Q2high (by mean distance):", closer, "\n\n")

  res <- rbind(res, data.frame(
    code = cd, cells = n_cells, contributing = n_contrib,
    mean_d1 = round(mean(D1), 4), mean_d2 = round(mean(D2), 4),
    median_d1 = round(median(D1), 4), median_d2 = round(median(D2), 4),
    pct_d2_lt_d1 = round(pct_d2_lt_d1, 1),
    wilcoxon_p = w$p.value,
    ccc_trace_high = round(c1, 4), ccc_base_high = round(c2, 4),
    closer = closer, stringsAsFactors = FALSE))
}

cat("=== summary table ===\n")
print(res, row.names = FALSE)
invisible(res)


# ---------------------------------------------------------------------------
# Part 2: cell counts before any model is fitted
# ---------------------------------------------------------------------------
# Cell counts before any model is fitted.
# Counts of y=0 and y=1 in every source x answer x gp2 x verb cell (16 cells),
# per code, with a sparse flag for cells holding fewer than 40 of either outcome.

d <- TRIALS_LONG

cat("=== Cell counts, source x answer x gp2 x verb (16 cells per code) ===\n")
cat("sparse flag: fewer than", SPARSE_MIN, "of either outcome\n\n")

all_tab <- data.frame()

for (cd in CODES) {
  s <- code_data(d, cd)
  cat("---------------- code", cd, "----------------\n")
  cat("N defined:", nrow(s),
      "  (Q1trace", sum(s$source == "Q1trace"),
      "/ Q2high", sum(s$source == "Q2high"), ")\n")
  cat("overall 1s:", sum(s$y == 1), " (",
      round(100 * mean(s$y == 1), 1), "% )\n\n", sep = "")

  tab <- aggregate(list(n0 = s$y == 0, n1 = s$y == 1),
                   by = list(source = s$source, answer = s$answer,
                             gp2 = s$gp2, verb = s$verb),
                   FUN = sum)
  tab$n   <- tab$n0 + tab$n1
  tab$pct <- round(100 * tab$n1 / tab$n, 1)
  tab$sparse <- ifelse(pmin(tab$n0, tab$n1) < SPARSE_MIN, "SPARSE", "")
  tab <- tab[order(tab$answer, tab$gp2, tab$verb, tab$source), ]
  print(tab, row.names = FALSE)

  ns <- sum(tab$sparse == "SPARSE")
  cat("\nsparse cells:", ns, "of", nrow(tab), "\n")
  cat("cells with a zero count:", sum(pmin(tab$n0, tab$n1) == 0), "\n\n")

  tab$code <- cd
  all_tab <- rbind(all_tab, tab)
}

invisible(all_tab)


# ---------------------------------------------------------------------------
# Part 3: mixed models at the retained rung
# ---------------------------------------------------------------------------
fits <- list()
for (cd in CODES) {
  s <- code_data(TRIALS_LONG, cd)
  rung <- RETAINED[[cd]]
  cat("\n=== fit", cd, "at rung:", RUNG_LABEL[[rung]], " N =", nrow(s), "===\n")
  m <- glmer(FORMULAS[[rung]], data = s, family = binomial,
             control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))
  cat("singular:", isSingular(m), " max fixed-effect SE:", round(max_se(m), 3),
      " (retained if <=", SE_THRESHOLD, ")\n")
  print(round(summary(m)$coefficients, 4))
  fits[[cd]] <- m
}

# ---------------------------------------------------------------------------
# Part 4: likelihood-ratio tests on every source-containing term
# ---------------------------------------------------------------------------
# Each term containing `source` is dropped INDIVIDUALLY from the retained rung,
# df = 1 each, leaving all other terms in place; plus one omnibus LRT dropping
# every source term at once. To beat R's marginality rule, the design matrix is
# built once and the single column of the target term is removed (every factor
# is two-level and sum-coded, so each term is exactly one column). The
# reference model is refitted on the same design-matrix columns so that all
# compared fits share one likelihood construction.
SRC_TERMS <- c("source",
               "source:answer", "source:gp2", "source:verb",
               "source:answer:gp2", "source:answer:verb", "source:gp2:verb",
               "source:answer:gp2:verb")
norm <- function(x) sapply(strsplit(x, ":"), function(p) paste(sort(p), collapse = ":"))

for (cd in CODES) {
  s <- code_data(TRIALS_LONG, cd)
  mref <- fits[[cd]]
  fixed_f  <- nobars(formula(mref))
  in_model <- attr(terms(fixed_f), "term.labels")
  present  <- SRC_TERMS[norm(SRC_TERMS) %in% norm(in_model)]

  X <- model.matrix(fixed_f, s)
  asg <- attr(X, "assign")
  safe <- make.names(colnames(X), unique = TRUE)
  dm <- as.data.frame(X[, asg != 0, drop = FALSE])
  names(dm) <- safe[asg != 0]
  dm$y <- s$y; dm$item <- s$item; dm$model <- s$model
  col_term <- in_model[asg[asg != 0]]
  col_name <- safe[asg != 0]

  fit_cols <- function(keep) {
    f <- as.formula(paste("y ~", paste(keep, collapse = " + "), "+ (1|item) + (1|model)"))
    glmer(f, data = dm, family = binomial,
          control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))
  }

  m_full <- fit_cols(col_name)
  ll_full <- as.numeric(logLik(m_full)); np_full <- attr(logLik(m_full), "df")
  cat("\n=== LRTs,", cd, "(rung:", RUNG_LABEL[[RETAINED[[cd]]]], ") reference logLik =",
      round(ll_full, 3), "===\n")

  for (tm in c(present, "OMNIBUS")) {
    drop_terms <- if (tm == "OMNIBUS") present else tm
    drop_cols  <- col_name[norm(col_term) %in% norm(drop_terms)]
    m2 <- fit_cols(setdiff(col_name, drop_cols))
    ll_red <- as.numeric(logLik(m2)); np_red <- attr(logLik(m2), "df")
    chisq <- 2 * (ll_full - ll_red); df <- np_full - np_red
    p <- pchisq(chisq, df, lower.tail = FALSE)
    flag <- if (chisq < 0) "NEGATIVE CHISQ - reduced fit beat the full fit, rerun" else ""
    cat(sprintf("%-6s %-24s chisq=%-11.4f df=%-3d p=%-12s %s\n",
                cd, tm, chisq, df, format.pval(p, digits = 4, eps = 1e-300), flag))
  }
}

# ---------------------------------------------------------------------------
# Part 5: post hoc contrasts at the highest significant order
# ---------------------------------------------------------------------------
# One p per contrast, from the model. Percentages are counted from the trial
# table and are descriptive only. All contrasts use weights = "proportional".
# Holm is applied within one family (the contrasts of one significant term for
# one code). The family list encodes the significant terms of the published
# LRT table; if the LRTs above change, revise FAMILIES accordingly.
FAMILIES <- list(
  A     = list(list(term = "source:gp2", by = c("gp2"))),
  B     = list(list(term = "source (main effect; all interactions ns)",
                    by = character(0))),
  Bres  = list(list(term = "source:answer", by = c("answer")),
               list(term = "source:verb",   by = c("verb"))),
  C     = list(list(term = "source:answer", by = c("answer")),
               list(term = "source:gp2",    by = c("gp2"))),
  Cpers = list(list(term = "source:gp2:verb", by = c("gp2", "verb")),
               list(term = "source:answer (significant, not contained in the three-way)",
                    by = c("answer"))),
  D     = list(list(term = "source:answer", by = c("answer")),
               list(term = "source:gp2",    by = c("gp2")))
)


for (cd in CODES) {
  s <- code_data(TRIALS_LONG, cd)
  m <- fits[[cd]]
  rung <- RETAINED[[cd]]
  cat("\n=== Post hoc, code", cd, " (retained rung:", RUNG_LABEL[[rung]], ") ===\n")
  cat("emmeans weights = proportional; one p per contrast; no test on percentages\n\n")
  all_out <- data.frame()

  for (fam in FAMILIES[[cd]]) {
    by <- fam$by
    marginal <- length(by) == 0
    cat("---- family:", fam$term, "----\n")
    if (marginal) {
      cat("source main effect, 1 marginal contrast",
          "(averaged over answer, gp2, verb, proportional weights)\n\n")
    } else {
      cat("source contrast within", paste(by, collapse = " x "),
          "-", prod(sapply(by, function(v) nlevels(s[[v]]))), "cells\n\n")
    }

    # ---- model side: OR, CI, p ----
    emm <- emmeans(m,
                   if (marginal) ~ source else
                     as.formula(paste("~ source |", paste(by, collapse = "*"))),
                   weights = "proportional")
    ct <- as.data.frame(pairs(emm, adjust = "none"))
    ci <- as.data.frame(confint(pairs(emm, adjust = "none"), level = 0.95))
    ct$lcl <- ci$asymp.LCL; ct$ucl <- ci$asymp.UCL

    # ---- data side: observed counts and percentages, descriptive only ----
    rows <- list()
    for (i in seq_len(nrow(ct))) {
      sel <- rep(TRUE, nrow(s))
      for (v in by) sel <- sel & s[[v]] == ct[[v]][i]

      a <- s[sel & s$source == "Q1trace", "y"]
      b <- s[sel & s$source == "Q2high",  "y"]
      p1 <- 100 * mean(a == 1); p2 <- 100 * mean(b == 1)

      rows[[i]] <- data.frame(
        cell = if (marginal) "all rows" else
          paste(sapply(by, function(v) paste0(v, "=", ct[[v]][i])), collapse = ", "),
        n_Q1trace = length(a), k_Q1trace = sum(a == 1), pct_Q1trace = round(p1, 1),
        n_Q2high  = length(b), k_Q2high  = sum(b == 1), pct_Q2high  = round(p2, 1),
        pp_diff = round(p1 - p2, 1),          # descriptive size only, not tested
        OR = exp(ct$estimate[i]),
        OR_lcl = exp(ct$lcl[i]), OR_ucl = exp(ct$ucl[i]),
        p = ct$p.value[i],
        stringsAsFactors = FALSE)
    }
    tab <- do.call(rbind, rows)
    tab$p_holm <- p.adjust(tab$p, method = "holm")

    pr <- tab
    for (cc in c("OR", "OR_lcl", "OR_ucl")) pr[[cc]] <- round(pr[[cc]], 3)
    for (cc in c("p", "p_holm")) pr[[cc]] <- format.pval(pr[[cc]], digits = 3, eps = 1e-300)
    print(pr, row.names = FALSE)
    cat("\n")

    tab$code <- cd; tab$family <- fam$term
    all_out <- rbind(all_out, tab)
  }
}
