# 03_stats_accuracy.R
# ===========================================================================
# Accuracy analyses of the paper, combined into one script.
#   Part 1  Core error model: error ~ GP x verb x thinking
#   Part 2  Reasoning length: difficulty -> length -> accuracy
#   Part 3  The four strongest models: reverse GP profile
# Paper sections (Results) and Supplemental File S1:
#   Part 1  "Overall Error Patterns: Lower Rates, Same Good-Enough Profile"
#           and "Thinking Improves Accuracy, Especially Where Reanalysis Matters";
#           S1 Section S1.1
#   Part 2  "Reasoning Length Tracks Difficulty and Predicts Accuracy"; S1 Section S1.2
#   Part 3  "The Strongest Models Reveal a Reverse Garden-Path Profile"; S1 Section S1.3
# All parts read results/trials.xlsx.
# Software: R 4.3.3, lme4 1.1-35.5, emmeans 1.11.2.
# ===========================================================================

# ---------------------------------------------------------------------------
# PREP: rebuild the analysis table from results/trials.xlsx
# ---------------------------------------------------------------------------
# The single data file of this repository is results/trials.xlsx (sheet
# "trials"; column definitions in sheet "codebook"). This block reconstructs
# the trial-level analysis frame with the variable names the models below use.
# Run from the scripts/ directory (or set TRIALS to the file location).
# Expected dimensions are asserted so a silent mismatch cannot pass unnoticed.
suppressMessages({library(readxl); library(lme4); library(emmeans)})
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


# ===========================================================================
# Part 1: core error model (no-thinking baseline vs high thinking)
# ===========================================================================
df <- subset(DF_ALL, think2 %in% c("no","high") & !is.na(error))
stopifnot(nrow(df) == 23029)
df$item <- factor(df$itemno); df$model <- factor(df$model)
# factor version (for emmeans cell means)
df$thinking <- factor(df$think2, levels=c("no","high"))
df$gp   <- factor(df$gp2,  levels=c("NGP","GP"))
df$verb <- factor(df$verb, levels=c("RAT","OT"))
# +/-0.5 sum-coded numeric predictors (for the coefficient/OR table)
df$gp_c    <- ifelse(df$gp2=="GP",    0.5, -0.5)   # GP vs NGP
df$verb_c  <- ifelse(df$verb=="OT",   0.5, -0.5)   # OT vs RAT
df$think_c <- ifelse(df$think2=="high",0.5, -0.5)  # high vs no
cat("N:",nrow(df)," items:",nlevels(df$item)," models:",nlevels(df$model),"\n")

ctrl <- glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=3e5))

# ---- Sum-coded model: coefficient/OR table (averaged main effects) ----
cat("\n======== SUM-CODED (+/-0.5) MODEL: full factorial ========\n")
m_sum <- glmer(error ~ gp_c*verb_c*think_c + (1+think_c|item) + (1+think_c|model),
               data=df, family=binomial, control=ctrl)
cat("singular:", isSingular(m_sum), "\n")
fe<-fixef(m_sum); se<-sqrt(diag(vcov(m_sum))); co<-summary(m_sum)$coefficients
or<-data.frame(logodds=round(fe,3), OR=round(exp(fe),3),
               CI_low=round(exp(fe-1.96*se),3), CI_high=round(exp(fe+1.96*se),3),
               z=round(co[,3],2), p=signif(co[,4],3))
rownames(or)<-c("(Intercept)","GP (main)","verbOT (main)","thinking (main)",
                "GP x verb","GP x thinking","verb x thinking","GP x verb x thinking")
print(or)

# ---- Likelihood-ratio tests (coding-invariant) ----
cat("\n======== LIKELIHOOD-RATIO TESTS ========\n")
cat("\n-- overall thinking (all 4 thinking terms) --\n")
m_noT <- update(m_sum, .~. - gp_c:verb_c:think_c - gp_c:think_c - verb_c:think_c - think_c)
print(anova(m_noT, m_sum))
cat("\n-- three-way GP x verb x thinking --\n")
m_no3 <- update(m_sum, .~. - gp_c:verb_c:think_c)
print(anova(m_no3, m_sum))
cat("\n-- GP x thinking (from no-3way) --\n")
print(anova(update(m_no3, .~. - gp_c:think_c), m_no3))
cat("\n-- verb x thinking (from no-3way) --\n")
print(anova(update(m_no3, .~. - verb_c:think_c), m_no3))
cat("\n-- GP x verb (from no-3way, thinking interactions kept) --\n")
print(anova(update(m_no3, .~. - gp_c:verb_c), m_no3))

# ---- Factor model for emmeans cell means + contrasts ----
cat("\n======== EMMEANS (factor-coded, identical fit) ========\n")
m_fac <- glmer(error ~ gp*verb*thinking + (1+thinking|item) + (1+thinking|model),
               data=df, family=binomial, control=ctrl)
cat("cell error rates (response):\n")
print(emmeans(m_fac, ~ thinking|gp*verb, type="response"))
cat("\nno vs high odds ratios:\n")
print(pairs(emmeans(m_fac, ~ thinking|gp*verb), adjust="none", type="response"))
cat("\nABSOLUTE risk differences high - no (pct pts):\n")
rg<-regrid(emmeans(m_fac, ~ thinking|gp*verb))
print(summary(contrast(rg,"revpairwise",by=c("gp","verb"),adjust="none"), infer=c(TRUE,TRUE)))



# ===========================================================================
# Part 2: reasoning length
# ===========================================================================
# Difficulty -> length, and length -> accuracy, on the high-thinking trials.
suppressMessages({library(lme4)})
options(width=200)
# build the length table used by the reasoning-length models: high-thinking
# trials; baseline_error = the proportion of errors in the model's ten baseline
# repetitions of the same item in the same condition.
d <- subset(DF_ALL, think2 == "high" & !is.na(error))
be <- aggregate(error ~ model + itemno + gp3, data = subset(DF_ALL, think2 == "no" & !is.na(error)), FUN = mean)
names(be)[names(be) == "error"] <- "baseline_error"
d <- merge(d, be, by = c("model","itemno","gp3"), sort = FALSE)
stopifnot(nrow(d) == 11511)
d$item<-factor(d$itemno); d$model<-factor(d$model); d$loglen<-log(d$gen_tokens)
d$gp<-factor(d$gp2,levels=c("NGP","GP"))
ctrl<-glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=3e5))
ortab<-function(m){fe<-fixef(m);se<-sqrt(diag(vcov(m)))
  round(data.frame(OR=exp(fe),CI_low=exp(fe-1.96*se),CI_high=exp(fe+1.96*se),p=summary(m)$coefficients[,4]),4)}
cetab<-function(m){fe<-fixef(m);se<-sqrt(diag(vcov(m)));co<-summary(m)$coefficients
  round(data.frame(Est=fe,SE=se,CI_low=fe-1.96*se,CI_high=fe+1.96*se,p=co[,ncol(co)]),4)}

run <- function(sub, label){
  cat("\n########################## ",label," (N=",nrow(sub),") ##########################\n",sep="")
  # (a) difficulty -> length
  cat("\n-- (a) log(gen_tokens) ~ baseline_error --\n")
  m_len<-lmer(loglen ~ baseline_error + (1|item)+(1|model), data=sub)
  print(cetab(m_len))
  # correlations, trial level
  ct<-cor.test(sub$baseline_error, sub$loglen)
  cs<-suppressWarnings(cor.test(sub$baseline_error, sub$loglen, method="spearman"))
  cat(sprintf("trial-level  Pearson r=%.3f [%.3f, %.3f]  Spearman rho=%.3f  (n=%d)\n",
      ct$estimate, ct$conf.int[1], ct$conf.int[2], cs$estimate, nrow(sub)))
  # aggregated to model-item-condition
  ag<-aggregate(cbind(baseline_error,loglen)~model+itemno+gp3, data=sub, FUN=mean)
  ct2<-cor.test(ag$baseline_error, ag$loglen)
  cat(sprintf("cell-level   Pearson r=%.3f [%.3f, %.3f]  (n=%d model-item-condition cells)\n",
      ct2$estimate, ct2$conf.int[1], ct2$conf.int[2], nrow(ag)))
  # (b) length -> accuracy, gp interaction
  cat("\n-- (b) error ~ scale(loglen)*gp --\n")
  sub$z<-scale(sub$loglen)
  m_acc<-glmer(error ~ z*gp + (1|item)+(1|model), data=sub, family=binomial, control=ctrl)
  cat("singular:",isSingular(m_acc),"\n"); print(ortab(m_acc))
}
run(d, "HIGH-THINKING TRIALS")

cat("\n-- n=6 condition-mean correlation (the inflated one, for reference) --\n")
cm<-aggregate(cbind(baseline_error,loglen)~gp3+verb, data=d, FUN=mean)
print(cor.test(cm$baseline_error, cm$loglen)$estimate)
cat("n =", nrow(cm), "\n")


# ===========================================================================
# Part 3: the four strongest models
# ===========================================================================
suppressMessages({library(lme4); library(emmeans)})
options(width=200)
d <- DF_ALL
flag<-c("claude-opus-4-6","gemini-3-pro-preview","glm-5","gpt-5-2025-08-07")
d<-subset(d, model %in% flag & think2=="high" & !is.na(error))
d$item<-factor(d$itemno); d$model<-factor(d$model)
d$gp<-factor(d$gp2,levels=c("NGP","GP")); d$verb<-factor(d$verb,levels=c("RAT","OT"))
cat("strongest four:", paste(sort(unique(d$model)),collapse=", "),"| N:",nrow(d),"\n")
cat("\n== error % by gp3 x verb per model ==\n")
print(aggregate(error~model+gp3+verb, d, function(x) round(100*mean(x),1)))
cat("\n== MODEL error ~ gp*verb + (1|item)+(1|model) ==\n")
m<-glmer(error ~ gp*verb + (1|item)+(1|model), data=d, family=binomial,
         control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=3e5)))
cat("singular:",isSingular(m),"\n")
fe<-fixef(m); se<-sqrt(diag(vcov(m)))
print(round(data.frame(OR=exp(fe),CI_low=exp(fe-1.96*se),CI_high=exp(fe+1.96*se),
      p=summary(m)$coefficients[,4]),4))
cat("\n== emmeans error/cell ==\n")
print(emmeans(m, ~gp|verb, type="response"))
cat("\n== GP vs NGP within each verb. NGP/GP >1 = reverse (NGP worse) ==\n")
print(pairs(emmeans(m, ~gp|verb), adjust="none"))

cat("\n== sum-coded fit (main effects averaged over the other factor) ==\n")
d$gp_c<-ifelse(d$gp2=="GP",.5,-.5); d$verb_c<-ifelse(d$verb=="OT",.5,-.5)
m2<-glmer(error~gp_c*verb_c+(1|item)+(1|model),data=d,family=binomial,
          control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=3e5)))
fe<-fixef(m2); se<-sqrt(diag(vcov(m2)))
print(round(data.frame(OR=exp(fe),CI_low=exp(fe-1.96*se),CI_high=exp(fe+1.96*se),
      p=summary(m2)$coefficients[,4]),4))

