# 04_stats_trace_codes.R
# ===========================================================================
# Trace-code analyses of the paper, combined into one script.
#   Part 1  Code prevalence by condition                (Section 4.5, App. B)
#   Part 2  Error by code within condition; joint models (Section 4.6, App. C)
#   Part 3  A x B_res interaction on detected GP traces  (Section 4.6)
#   Part 4  Strongest vs remaining models (cohort)       (Section 4.8)
#   Part 5  Analyses within the four strongest only      (Section 4.8)
# The analysis table is derived below from results/trials.xlsx.
# Software: R 4.3.3, lme4 1.1-35.5, emmeans 1.11.2, dplyr.
# ===========================================================================

# ---------------------------------------------------------------------------
# PREP: rebuild the analysis table from results/trials.xlsx
# ---------------------------------------------------------------------------
# The single data file of this repository is results/trials.xlsx (sheet
# "trials"; column definitions in sheet "codebook"). This block reconstructs
# the trial-level analysis frame with the variable names the models below use.
# Run from the scripts/ directory (or set TRIALS to the file location).
# Expected dimensions are asserted so a silent mismatch cannot pass unnoticed.
suppressMessages({library(readxl)})
TRIALS <- "../results/trials.xlsx"
raw <- as.data.frame(read_excel(TRIALS, sheet = "trials"))
stopifnot(nrow(raw) == 23040)

DF_ALL <- data.frame(
  model          = raw$model,
  flagship       = raw$strongest4,
  itemno         = raw$itemno,
  verb           = raw$verb_type,
  gp3            = raw$condition,
  gp2            = raw$gp,
  trial          = raw$trial,
  thinking_level = raw$thinking_level,
  think2         = raw$thinking_condition,   # "no" = the model's baseline setting, "high" = high thinking
  q1_answer      = raw$q1_answer_norm,
  error          = raw$error,
  q1_input_tokens = raw$q1_input_tokens,
  q1_total_tokens = raw$q1_total_tokens,
  gen_tokens     = raw$generated_tokens,
  stringsAsFactors = FALSE)

# Derive the six binary analysis codes from the raw trace codes (Methods):
# high-thinking trials passing the 0.15 trace-visibility filter, Yes/No answers.
#   A      = trace_A (0/1)
#   B_det  = 1 if trace_B in {1,2}, 0 if trace_B == 0        (difficulty detected)
#   B_res  = 1 if trace_B == 1, 0 if trace_B == 2, else NA   (resolved | detected)
#   C_purs = 1 if trace_C in {1,2}, 0 if trace_C == 0        (object pursued)
#   C_pers = 1 if trace_C == 2, 0 if trace_C == 1, else NA   (persisted | pursued)
#   D      = trace_D if in {0,1}, else NA                    (pragmatic override)
# Cannot-decide values (B/C = 3, D = 2) become NA for that code only.
STRONG <- c("claude-opus-4-6", "gemini-3-pro-preview", "gpt-5-2025-08-07", "glm-5")
h <- raw[raw$thinking_condition == "high" & raw$trace_included %in% c(1, TRUE, "TRUE", "True") &
         raw$q1_answer_norm %in% c("Yes","No"), ]
bin01 <- function(x, one, zero) ifelse(x %in% one, 1, ifelse(x %in% zero, 0, NA))
D_V2 <- data.frame(
  item  = h$itemno, gp2 = ifelse(h$condition == "GP", "GP", "NGP"), gp3 = h$condition,
  verb  = h$verb_type, model = h$model,
  cohort = ifelse(h$model %in% STRONG, "Strong4", "Others"),
  trial = h$trial, error = h$error,
  A      = as.integer(h$trace_A),
  B_det  = bin01(h$trace_B, c(1,2), 0),
  B_res  = bin01(h$trace_B, 1, 2),
  C_purs = bin01(h$trace_C, c(1,2), 0),
  C_pers = bin01(h$trace_C, 2, 1),
  D      = bin01(h$trace_D, 1, 0),
  stringsAsFactors = FALSE)
stopifnot(nrow(D_V2) == 10216)   # matches the published analysis set

# ===========================================================================
# Part 1: code prevalence by GP x verb (Section 4.5)
# ===========================================================================
# Code prevalence by GP x verb (Section 4.5).
# Six models: code ~ gp2 * verb + (1|item) + (1|model), binomial.
# LRTs for interaction & mains; emmeans cell rates; Holm within-verb GP contrasts.
# Plus A x B: (a) misuse descriptives among A=1 GP traces; (b) B_res ~ A on GP detected.

suppressMessages({library(lme4); library(emmeans); library(dplyr); library(readr)})
options(width = 110)
d <- D_V2
d$gp2  <- factor(d$gp2, c("NGP","GP")); d$verb <- factor(d$verb, c("RAT","OT"))
contrasts(d$gp2) <- contr.sum(2)/-2   # +/-0.5 sum coding, +0.5 = GP
contrasts(d$verb) <- contr.sum(2)/-2  # +0.5 = OT
codes <- c("A","B_det","B_res","C_purs","C_pers","D")

fit_one <- function(code){
  cat("\n=============================", code, "=============================\n")
  dd <- d[!is.na(d[[code]]), ]
  dd$y <- dd[[code]]
  cat("N =", nrow(dd), "\n")
  m  <- glmer(y ~ gp2*verb + (1|item) + (1|model), dd, family=binomial,
              control=glmerControl(optimizer="bobyqa"))
  cat("singular:", isSingular(m), "\n")
  print(summary(m)$coefficients)
  co <- exp(cbind(OR=fixef(m), confint(m, method="Wald")[-(1:2),,drop=FALSE]))
  print(round(co,3))
  # LRTs
  m_no3 <- update(m, . ~ . - gp2:verb)
  cat("\nLRT gp x verb: "); print(anova(m_no3, m)[2, c("Chisq","Df","Pr(>Chisq)")])
  m_nog <- update(m_no3, . ~ . - gp2)
  cat("LRT gp: ");        print(anova(m_nog, m_no3)[2, c("Chisq","Df","Pr(>Chisq)")])
  m_nov <- update(m_no3, . ~ . - verb)
  cat("LRT verb: ");      print(anova(m_nov, m_no3)[2, c("Chisq","Df","Pr(>Chisq)")])
  # cells + within-verb GP contrasts (Holm)
  em <- emmeans(m, ~ gp2 | verb, type="response")
  print(em)
  print(summary(contrast(em, "revpairwise", adjust="holm"), infer=TRUE))
  invisible(m)
}
for (code in codes) fit_one(code)

# ---------- A x B ----------
cat("\n\n================ A x B: misuse & resolution ================\n")
gp <- d %>% filter(gp2=="GP")
cat("\nAmong A=1 GP traces, distribution of B (raw 3-level via B_det/B_res):\n")
gp %>% mutate(Blev = ifelse(is.na(B_det), NA,
                ifelse(B_det==0, "B0_nodiff", ifelse(B_res==1,"B1_resolved","B2_ungram")))) %>%
  filter(!is.na(Blev)) %>% count(A, Blev) %>% group_by(A) %>%
  mutate(pct = round(100*n/sum(n),1)) %>% print(n=20)

cat("\nB_res ~ A on GP detected subset:\n")
db <- gp %>% filter(!is.na(B_res))
mb <- glmer(B_res ~ A + (1|item) + (1|model), db, family=binomial,
            control=glmerControl(optimizer="bobyqa"))
cat("N =", nrow(db), " singular:", isSingular(mb), "\n")
print(summary(mb)$coefficients)
print(round(exp(cbind(OR=fixef(mb), confint(mb, method="Wald")[-1,,drop=FALSE])),3))

cat("\nError by A x B level (GP, descriptive):\n")
gp %>% mutate(Blev = ifelse(is.na(B_det), NA,
                ifelse(B_det==0,"B0", ifelse(B_res==1,"B1","B2")))) %>%
  filter(!is.na(Blev)) %>% group_by(A, Blev) %>%
  summarise(n=n(), err=round(100*mean(error),1), .groups="drop") %>% print(n=20)


# ===========================================================================
# Part 2: error by code within condition (Section 4.6)
# ===========================================================================
# Error ~ code x GP x verb per code (Section 4.6),
# then adjusted joint models and the C' x D analysis on pursued NGP-OT.

suppressMessages({library(lme4); library(emmeans); library(dplyr); library(readr)})
options(width = 110)
d <- D_V2
d$gp2  <- factor(d$gp2, c("NGP","GP")); d$verb <- factor(d$verb, c("RAT","OT"))
contrasts(d$gp2) <- contr.sum(2)/-2; contrasts(d$verb) <- contr.sum(2)/-2
codes <- c("A","B_det","B_res","C_purs","C_pers","D")

fit_err <- function(code){
  cat("\n=============================", code, "=============================\n")
  dd <- d[!is.na(d[[code]]), ]; dd$x <- dd[[code]]
  # drop cells with no variance in x (report them)
  tab <- dd %>% group_by(gp2, verb) %>% summarise(nx0=sum(x==0), nx1=sum(x==1), .groups="drop")
  print(tab)
  form <- error ~ x*gp2*verb + (1|item) + (1|model)
  m <- tryCatch(glmer(form, dd, family=binomial, control=glmerControl(optimizer="bobyqa")),
                error=function(e){cat("3-way fit failed:", conditionMessage(e), "\n"); NULL})
  if (is.null(m) || isSingular(m)) {
    cat("--> falling back to two-way (x*gp2 + x*verb + gp2*verb)\n")
    m <- glmer(error ~ x*gp2 + x*verb + gp2*verb + (1|item) + (1|model), dd,
               family=binomial, control=glmerControl(optimizer="bobyqa"))
  }
  cat("singular:", isSingular(m), "\n")
  print(summary(m)$coefficients)
  # LRT for any code involvement
  m0 <- glmer(error ~ gp2*verb + (1|item) + (1|model), dd, family=binomial,
              control=glmerControl(optimizer="bobyqa"))
  cat("\nLRT all x terms: "); print(anova(m0, m)[2, c("Chisq","Df","Pr(>Chisq)")])
  # simple effect of code within each cell, Holm across the 4 cells
  em <- emmeans(m, ~ x | gp2*verb, type="response")
  print(em)
  print(summary(contrast(em, "revpairwise", by=c("gp2","verb"), adjust="none"),
        infer=TRUE, adjust="holm"))
  invisible(m)
}
for (code in codes) fit_err(code)

# ---------- adjusted joint models (secondary) ----------
cat("\n\n============ adjusted: error ~ A + B_det, GP subset ============\n")
dg <- d %>% filter(gp2=="GP", !is.na(A), !is.na(B_det))
m1 <- glmer(error ~ A + B_det + (1|item) + (1|model), dg, family=binomial,
            control=glmerControl(optimizer="bobyqa"))
cat("N =", nrow(dg), " singular:", isSingular(m1), "\n")
print(summary(m1)$coefficients)
print(round(exp(cbind(OR=fixef(m1), confint(m1, method="Wald")[-1,,drop=FALSE])),4))

cat("\n============ adjusted: error ~ C_pers + D, pursued NGP-OT ============\n")
dn <- d %>% filter(gp2=="NGP", verb=="OT", !is.na(C_pers), !is.na(D))
m2 <- glmer(error ~ C_pers + D + (1|item) + (1|model), dn, family=binomial,
            control=glmerControl(optimizer="bobyqa"))
cat("N =", nrow(dn), " singular:", isSingular(m2), "\n")
print(summary(m2)$coefficients)
print(round(exp(cbind(OR=fixef(m2), confint(m2, method="Wald")[-1,,drop=FALSE])),4))

# ---------- C' x D (Option 1) ----------
cat("\n\n================= C' x D on pursued NGP-OT =================\n")
cat("\nP(D=1 | C_pers):\n")
dn %>% group_by(C_pers) %>% summarise(n=n(), D_rate=round(100*mean(D),1)) %>% print()
cat("\nError by C_pers x D cells:\n")
dn %>% group_by(C_pers, D) %>%
  summarise(n=n(), err=round(100*mean(error),1), .groups="drop") %>% print()
cat("\nerror ~ C_pers * D, pursued NGP-OT:\n")
m3 <- glmer(error ~ C_pers*D + (1|item) + (1|model), dn, family=binomial,
            control=glmerControl(optimizer="bobyqa"))
cat("singular:", isSingular(m3), "\n")
print(summary(m3)$coefficients)
m3a <- update(m3, . ~ . - C_pers:D)
cat("LRT C_pers x D: "); print(anova(m3a, m3)[2, c("Chisq","Df","Pr(>Chisq)")])

# persistence generality: error ~ C_pers * gp2 on pursued OT trials
cat("\n\n========== error ~ C_pers * gp2, pursued OT trials ==========\n")
dot <- d %>% filter(verb=="OT", !is.na(C_pers))
m4 <- glmer(error ~ C_pers*gp2 + (1|item) + (1|model), dot, family=binomial,
            control=glmerControl(optimizer="bobyqa"))
cat("N =", nrow(dot), " singular:", isSingular(m4), "\n")
print(summary(m4)$coefficients)
em4 <- emmeans(m4, ~ C_pers | gp2, type="response")
print(em4)
print(summary(contrast(em4, "revpairwise", adjust="holm"), infer=TRUE))


# ===========================================================================
# Part 2b: fixes (D two-way fallback, clean CIs, cell Ns)
# ===========================================================================
# Refinements to the error-by-code models:
# (1) D error model refit without three-way (convergence failure in full model)
# (2) adjusted joint models: clean Wald CIs (parm="beta_")
# (3) B_res model: cell Ns for caution notes

suppressMessages({library(lme4); library(emmeans); library(dplyr); library(readr)})
options(width = 110)
d <- D_V2
d$gp2  <- factor(d$gp2, c("NGP","GP")); d$verb <- factor(d$verb, c("RAT","OT"))
contrasts(d$gp2) <- contr.sum(2)/-2; contrasts(d$verb) <- contr.sum(2)/-2

orci <- function(m) round(exp(cbind(OR=fixef(m),
                confint(m, parm="beta_", method="Wald"))),4)

cat("========== D error model, two-way fallback ==========\n")
dd <- d[!is.na(d$D),]; dd$x <- dd$D
m <- glmer(error ~ x*gp2 + x*verb + gp2*verb + (1|item) + (1|model), dd,
           family=binomial, control=glmerControl(optimizer="bobyqa"))
cat("singular:", isSingular(m), " converged:",
    length(m@optinfo$conv$lme4$messages)==0, "\n")
print(summary(m)$coefficients); print(orci(m))
m0 <- glmer(error ~ gp2*verb + (1|item) + (1|model), dd, family=binomial,
            control=glmerControl(optimizer="bobyqa"))
cat("LRT all x terms: "); print(anova(m0, m)[2, c("Chisq","Df","Pr(>Chisq)")])
em <- emmeans(m, ~ x | gp2*verb, type="response")
print(em)
print(summary(contrast(em, "revpairwise", by=c("gp2","verb"), adjust="none"),
      infer=TRUE, adjust="holm"))

cat("\n========== adjusted error ~ A + B_det (GP), clean CIs ==========\n")
dg <- d %>% filter(gp2=="GP", !is.na(A), !is.na(B_det))
m1 <- glmer(error ~ A + B_det + (1|item) + (1|model), dg, family=binomial,
            control=glmerControl(optimizer="bobyqa"))
cat("N =", nrow(dg), " singular:", isSingular(m1), "\n")
print(summary(m1)$coefficients); print(orci(m1))

cat("\n========== adjusted error ~ C_pers + D (pursued NGP-OT), clean CIs ==========\n")
dn <- d %>% filter(gp2=="NGP", verb=="OT", !is.na(C_pers), !is.na(D))
m2 <- glmer(error ~ C_pers + D + (1|item) + (1|model), dn, family=binomial,
            control=glmerControl(optimizer="bobyqa"))
cat("N =", nrow(dn), " singular:", isSingular(m2), "\n")
print(summary(m2)$coefficients); print(orci(m2))

cat("\n========== B_res: raw cell Ns and error rates ==========\n")
d %>% filter(!is.na(B_res)) %>% group_by(gp2, verb, B_res) %>%
  summarise(n=n(), err=round(100*mean(error),1), .groups="drop") %>% print(n=16)


# ===========================================================================
# A x B_res on detected GP traces (pooled, and by cohort) -- Section 4.6
# ===========================================================================
cat("\n\n========== A x B_res, detected GP traces (pooled) ==========\n")
dg <- D_V2[D_V2$gp2 == "GP" & !is.na(D_V2$A) & !is.na(D_V2$B_res), ]
dg %>% group_by(A) %>% summarise(n = n(), res = round(100*mean(B_res),1)) %>%
  {cat("B-prime rate by A (detected GP traces):\n"); print(.)}
mAB <- glmer(error ~ A*B_res + (1|item) + (1|model), dg, family = binomial,
             control = glmerControl(optimizer = "bobyqa"))
cat("N =", nrow(dg), " singular:", isSingular(mAB), "\n")
print(summary(mAB)$coefficients)
mAB0 <- update(mAB, . ~ . - A:B_res)
cat("LRT A x Bres: "); print(anova(mAB0, mAB)[2, c("Chisq","Df","Pr(>Chisq)")])
cat("main-effects model:\n"); print(summary(mAB0)$coefficients)
print(round(exp(cbind(OR = fixef(mAB0), confint(mAB0, parm = "beta_", method = "Wald"))), 4))
em <- emmeans(mAB, ~ A | B_res, type = "response"); print(em)
cat("A effect within B_res levels:\n")
print(summary(contrast(em, "revpairwise", by = "B_res", adjust = "none"), infer = TRUE, adjust = "holm"))
em2 <- emmeans(mAB, ~ B_res | A, type = "response")
cat("B_res effect within A levels:\n")
print(summary(contrast(em2, "revpairwise", by = "A", adjust = "none"), infer = TRUE, adjust = "holm"))
cat("cells:\n")
dg %>% group_by(A, B_res) %>% summarise(n = n(), err = round(100*mean(error),1), .groups = "drop") %>% print()

cat("\n\n========== A x B_res by cohort ==========\n")
for (coh in c("Strong4","Others")) {
  cat("\n---", coh, "---\n")
  dc <- dg[dg$cohort == coh, ]
  mc <- glmer(error ~ A*B_res + (1|item) + (1|model), dc, family = binomial,
              control = glmerControl(optimizer = "bobyqa"))
  cat("N =", nrow(dc), " singular:", isSingular(mc), "\n")
  print(summary(mc)$coefficients)
  mc0 <- update(mc, . ~ . - A:B_res)
  cat("LRT A x Bres: "); print(anova(mc0, mc)[2, c("Chisq","Df","Pr(>Chisq)")])
  dc %>% group_by(A, B_res) %>% summarise(n = n(), err = round(100*mean(error),1), .groups = "drop") %>% print()
}

# ===========================================================================
# Part 4: Strong4 vs Others cohort models (Section 4.8)
# ===========================================================================
# Strong4 vs Others cohort models (Section 4.8).
# (1) code ~ cohort * gp2 * verb per code, cohort contrasts per cell (Holm).
# (2) Strong4-only within-OT D contrast (GP-OT vs NGP-OT), same for C_pers.
# (3) error-by-code descriptives per cohort; adjusted NGP-OT model with cohort.

suppressMessages({library(lme4); library(emmeans); library(dplyr); library(readr)})
options(width = 110)
d <- D_V2
d$gp2  <- factor(d$gp2, c("NGP","GP")); d$verb <- factor(d$verb, c("RAT","OT"))
d$cohort <- factor(d$cohort, c("Others","Strong4"))
contrasts(d$gp2) <- contr.sum(2)/-2; contrasts(d$verb) <- contr.sum(2)/-2
contrasts(d$cohort) <- contr.sum(2)/-2   # +0.5 = Strong4
codes <- c("A","B_det","B_res","C_purs","C_pers","D")

fit_coh <- function(code){
  cat("\n=============================", code, "=============================\n")
  dd <- d[!is.na(d[[code]]), ]; dd$y <- dd[[code]]
  m <- tryCatch(glmer(y ~ cohort*gp2*verb + (1|item) + (1|model), dd, family=binomial,
                      control=glmerControl(optimizer="bobyqa")),
                error=function(e){cat("fit failed:", conditionMessage(e), "\n"); NULL})
  if (is.null(m) || isSingular(m)) {
    cat("--> fallback: drop three-way\n")
    m <- glmer(y ~ (cohort+gp2+verb)^2 + (1|item) + (1|model), dd, family=binomial,
               control=glmerControl(optimizer="bobyqa"))
  }
  cat("N =", nrow(dd), " singular:", isSingular(m), "\n")
  print(summary(m)$coefficients)
  m0 <- glmer(y ~ gp2*verb + (1|item) + (1|model), dd, family=binomial,
              control=glmerControl(optimizer="bobyqa"))
  cat("\nLRT all cohort terms: "); print(anova(m0, m)[2, c("Chisq","Df","Pr(>Chisq)")])
  em <- emmeans(m, ~ cohort | gp2*verb, type="response")
  print(em)
  print(summary(contrast(em, "revpairwise", by=c("gp2","verb"), adjust="none"),
        infer=TRUE, adjust="holm"))
  invisible(m)
}
for (code in codes) fit_coh(code)

# ---------- Strong4-only within-OT contrasts for C_pers and D ----------
cat("\n\n========= Strong4 only: gp2 effect within OT (C_pers, D) =========\n")
for (code in c("C_pers","D")) {
  cat("\n---", code, "(Strong4, OT trials) ---\n")
  ds <- d %>% filter(cohort=="Strong4", verb=="OT") ; ds <- ds[!is.na(ds[[code]]),]
  ds$y <- ds[[code]]
  m <- glmer(y ~ gp2 + (1|item) + (1|model), ds, family=binomial,
             control=glmerControl(optimizer="bobyqa"))
  cat("N =", nrow(ds), " singular:", isSingular(m), "\n")
  print(summary(m)$coefficients)
  em <- emmeans(m, ~ gp2, type="response"); print(em)
  print(summary(contrast(em, "revpairwise"), infer=TRUE))
}

# ---------- error by code within cohort (descriptives) ----------
cat("\n\n========= error by code x cell x cohort (descriptive) =========\n")
for (code in codes) {
  cat("\n---", code, "---\n")
  dd <- d[!is.na(d[[code]]), ]; dd$x <- dd[[code]]
  dd %>% group_by(cohort, gp2, verb, x) %>%
    summarise(n=n(), err=round(100*mean(error),1), .groups="drop") %>%
    print(n=40)
}

# ---------- adjusted NGP-OT error model with cohort ----------
cat("\n\n===== error ~ (C_pers + D) * cohort, pursued NGP-OT =====\n")
dn <- d %>% filter(gp2=="NGP", verb=="OT", !is.na(C_pers), !is.na(D))
m5 <- glmer(error ~ (C_pers + D)*cohort + (1|item) + (1|model), dn, family=binomial,
            control=glmerControl(optimizer="bobyqa"))
cat("N =", nrow(dn), " singular:", isSingular(m5), "\n")
print(summary(m5)$coefficients)

# Strong4 GP-OT: persistence-without-override cell table
cat("\n===== Strong4 OT cells: C_pers x D joint rates =====\n")
d %>% filter(cohort=="Strong4", verb=="OT", !is.na(C_pers), !is.na(D)) %>%
  group_by(gp3) %>%
  summarise(n=n(), pers=round(100*mean(C_pers),1), D=round(100*mean(D),1),
            pers_and_D=round(100*mean(C_pers==1 & D==1),1),
            pers_no_D=round(100*mean(C_pers==1 & D==0),1),
            err=round(100*mean(error),1), .groups="drop") %>% print()
d %>% filter(cohort=="Others", verb=="OT", !is.na(C_pers), !is.na(D)) %>%
  group_by(gp3) %>%
  summarise(n=n(), pers=round(100*mean(C_pers),1), D=round(100*mean(D),1),
            pers_and_D=round(100*mean(C_pers==1 & D==1),1),
            pers_no_D=round(100*mean(C_pers==1 & D==0),1),
            err=round(100*mean(error),1), .groups="drop") %>% print()


# ===========================================================================
# Part 4b: final group analysis set (Section 4.8)
# ===========================================================================
# Final group analysis set (Section 4.8).
# (1) Six prevalence models: code ~ group*gp2*verb + (1|item)+(1|model)
#     LRT for all group terms; Holm-corrected group contrasts per cell.
# (2) Accuracy: error ~ (A + B_res)*group on detected GP trials
#               error ~ (C_pers + D)*group on pursued NGP-OT trials
suppressMessages({library(lme4); library(emmeans); library(dplyr); library(readr)})
options(width = 120)
d <- D_V2
d$gp2  <- factor(d$gp2, c("NGP","GP")); d$verb <- factor(d$verb, c("RAT","OT"))
d$group <- factor(d$cohort, c("Others","Strong4"))
contrasts(d$gp2) <- contr.sum(2)/-2; contrasts(d$verb) <- contr.sum(2)/-2
contrasts(d$group) <- contr.sum(2)/-2
codes <- c("A","B_det","B_res","C_purs","C_pers","D")

for (code in codes) {
  cat("\n=====================", code, "=====================\n")
  dd <- d[!is.na(d[[code]]), ]; dd$y <- dd[[code]]
  m <- tryCatch(glmer(y ~ group*gp2*verb + (1|item) + (1|model), dd, family=binomial,
                      control=glmerControl(optimizer="bobyqa")),
                error=function(e){cat("FIT FAILED:", conditionMessage(e), "\n"); NULL})
  if (is.null(m)) next
  conv_msgs <- length(m@optinfo$conv$lme4$messages)
  cat("N =", nrow(dd), "| singular:", isSingular(m), "| convergence messages:", conv_msgs, "\n")
  m0 <- glmer(y ~ gp2*verb + (1|item) + (1|model), dd, family=binomial,
              control=glmerControl(optimizer="bobyqa"))
  lrt <- anova(m0, m)
  cat("LRT all group terms: chi2(", lrt$Df[2], ") =", round(lrt$Chisq[2],2),
      ", p =", signif(lrt$`Pr(>Chisq)`[2],3), "\n")
  em <- emmeans(m, ~ group | gp2*verb, type="response")
  print(em)
  print(summary(contrast(em, "revpairwise", by=c("gp2","verb"), adjust="none"),
        infer=TRUE, adjust="holm"))
}

cat("\n\n===================== accuracy: GP detected, (A + B_res)*group =====================\n")
dg <- d %>% filter(gp2=="GP", !is.na(A), !is.na(B_res))
cat("cells:\n")
dg %>% group_by(group, A, B_res) %>% summarise(n=n(), err=round(100*mean(error),1), .groups="drop") %>% print(n=10)
mg <- tryCatch(glmer(error ~ (A + B_res)*group + (1|item) + (1|model), dg, family=binomial,
                     control=glmerControl(optimizer="bobyqa")),
               error=function(e){cat("FIT FAILED:", conditionMessage(e), "\n"); NULL})
if (!is.null(mg)) {
  cat("N =", nrow(dg), "| singular:", isSingular(mg),
      "| convergence messages:", length(mg@optinfo$conv$lme4$messages), "\n")
  print(summary(mg)$coefficients)
  mg0 <- glmer(error ~ A + B_res + group + (1|item) + (1|model), dg, family=binomial,
               control=glmerControl(optimizer="bobyqa"))
  lrt <- anova(mg0, mg)
  cat("LRT group interactions: chi2(", lrt$Df[2], ") =", round(lrt$Chisq[2],2),
      ", p =", signif(lrt$`Pr(>Chisq)`[2],3), "\n")
}

cat("\n\n===================== accuracy: pursued NGP-OT, (C_pers + D)*group =====================\n")
dn <- d %>% filter(gp2=="NGP", verb=="OT", !is.na(C_pers), !is.na(D))
cat("cells:\n")
dn %>% group_by(group, C_pers, D) %>% summarise(n=n(), err=round(100*mean(error),1), .groups="drop") %>% print(n=10)
mn <- tryCatch(glmer(error ~ (C_pers + D)*group + (1|item) + (1|model), dn, family=binomial,
                     control=glmerControl(optimizer="bobyqa")),
               error=function(e){cat("FIT FAILED:", conditionMessage(e), "\n"); NULL})
if (!is.null(mn)) {
  cat("N =", nrow(dn), "| singular:", isSingular(mn),
      "| convergence messages:", length(mn@optinfo$conv$lme4$messages), "\n")
  print(summary(mn)$coefficients)
  mn0 <- glmer(error ~ C_pers + D + group + (1|item) + (1|model), dn, family=binomial,
               control=glmerControl(optimizer="bobyqa"))
  lrt <- anova(mn0, mn)
  cat("LRT group interactions: chi2(", lrt$Df[2], ") =", round(lrt$Chisq[2],2),
      ", p =", signif(lrt$`Pr(>Chisq)`[2],3), "\n")
}


# ===========================================================================
# Part 5: within the four strongest models only (Section 4.8)
# ===========================================================================
# All analyses within the four strongest models only (Section 4.8).
# (1) Six prevalence models: code ~ gp2*verb + (1|item) + (1|model), Strong4 trials.
#     LRTs (gp, verb, gp x verb) + Holm within-verb GP contrasts.
# (2) Accuracy GP: error ~ A*B_res on detected GP trials + cells.
# (3) Accuracy NGP-OT: error ~ C_pers + D on pursued trials + C' x D cells + P(D|C').
suppressMessages({library(lme4); library(emmeans); library(dplyr); library(readr)})
options(width = 120)
d <- D_V2 %>% filter(cohort=="Strong4")
d$gp2  <- factor(d$gp2, c("NGP","GP")); d$verb <- factor(d$verb, c("RAT","OT"))
contrasts(d$gp2) <- contr.sum(2)/-2; contrasts(d$verb) <- contr.sum(2)/-2
codes <- c("A","B_det","B_res","C_purs","C_pers","D")

cat("Raw code rates (%), Strong4, by condition:\n")
for (code in codes) {
  r <- d[!is.na(d[[code]]),] %>% group_by(gp3, verb) %>%
    summarise(v=round(100*mean(.data[[code]]),1), n=n(), .groups="drop")
  cat(code, ": "); print(tidyr::pivot_wider(r %>% select(-n), names_from=c(gp3,verb), values_from=v))
}

for (code in codes) {
  cat("\n=====================", code, "(Strong4) =====================\n")
  dd <- d[!is.na(d[[code]]), ]; dd$y <- dd[[code]]
  m <- tryCatch(glmer(y ~ gp2*verb + (1|item) + (1|model), dd, family=binomial,
                      control=glmerControl(optimizer="bobyqa")),
                error=function(e){cat("FIT FAILED:", conditionMessage(e), "\n"); NULL})
  if (is.null(m)) next
  cat("N =", nrow(dd), "| singular:", isSingular(m),
      "| convergence messages:", length(m@optinfo$conv$lme4$messages), "\n")
  m_no3 <- update(m, . ~ . - gp2:verb)
  m_nog <- update(m_no3, . ~ . - gp2)
  m_nov <- update(m_no3, . ~ . - verb)
  l3 <- anova(m_no3, m); lg <- anova(m_nog, m_no3); lv <- anova(m_nov, m_no3)
  cat("LRT gp:      chi2(1) =", round(lg$Chisq[2],2), ", p =", signif(lg$`Pr(>Chisq)`[2],3), "\n")
  cat("LRT verb:    chi2(1) =", round(lv$Chisq[2],2), ", p =", signif(lv$`Pr(>Chisq)`[2],3), "\n")
  cat("LRT gpXverb: chi2(1) =", round(l3$Chisq[2],2), ", p =", signif(l3$`Pr(>Chisq)`[2],3), "\n")
  em <- emmeans(m, ~ gp2 | verb, type="response")
  print(em)
  print(summary(contrast(em, "revpairwise", by="verb", adjust="none"), infer=TRUE, adjust="holm"))
}

cat("\n\n============ Strong4 accuracy GP: error ~ A*B_res, detected GP trials ============\n")
dg <- d %>% filter(gp2=="GP", !is.na(A), !is.na(B_res))
cat("B_res rate by A: "); dg %>% group_by(A) %>% summarise(n=n(), res=round(100*mean(B_res),1)) %>% print()
cat("cells:\n"); dg %>% group_by(A, B_res) %>% summarise(n=n(), err=round(100*mean(error),1), .groups="drop") %>% print()
mg <- glmer(error ~ A*B_res + (1|item) + (1|model), dg, family=binomial,
            control=glmerControl(optimizer="bobyqa"))
cat("N =", nrow(dg), "| singular:", isSingular(mg),
    "| convergence messages:", length(mg@optinfo$conv$lme4$messages), "\n")
print(summary(mg)$coefficients)
mg0 <- update(mg, . ~ . - A:B_res)
l <- anova(mg0, mg); cat("LRT A x B_res: chi2(1) =", round(l$Chisq[2],2), ", p =", signif(l$`Pr(>Chisq)`[2],3), "\n")
cat("main-effects model:\n"); print(summary(mg0)$coefficients)
print(round(exp(cbind(OR=fixef(mg0), confint(mg0, parm="beta_", method="Wald"))),4))
em <- emmeans(mg, ~ A | B_res, type="response")
cat("A within B_res:\n"); print(summary(contrast(em, "revpairwise", by="B_res", adjust="none"), infer=TRUE, adjust="holm"))
em2 <- emmeans(mg, ~ B_res | A, type="response")
cat("B_res within A:\n"); print(summary(contrast(em2, "revpairwise", by="A", adjust="none"), infer=TRUE, adjust="holm"))

cat("\n\n============ Strong4 accuracy NGP-OT: error ~ C_pers + D, pursued trials ============\n")
dn <- d %>% filter(gp2=="NGP", verb=="OT", !is.na(C_pers), !is.na(D))
cat("P(D=1|C_pers): "); dn %>% group_by(C_pers) %>% summarise(n=n(), D=round(100*mean(D),1)) %>% print()
cat("C' x D cells:\n"); dn %>% group_by(C_pers, D) %>% summarise(n=n(), err=round(100*mean(error),1), .groups="drop") %>% print()
mn <- glmer(error ~ C_pers + D + (1|item) + (1|model), dn, family=binomial,
            control=glmerControl(optimizer="bobyqa"))
cat("N =", nrow(dn), "| singular:", isSingular(mn),
    "| convergence messages:", length(mn@optinfo$conv$lme4$messages), "\n")
print(summary(mn)$coefficients)
print(round(exp(cbind(OR=fixef(mn), confint(mn, parm="beta_", method="Wald"))),4))

