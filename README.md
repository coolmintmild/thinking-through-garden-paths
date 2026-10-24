# Thinking Through Garden Paths: data and analysis repository

Data and code for "Thinking Through Garden Paths: Reasoning Traces and Sentence
Comprehension in Large Reasoning Models". Sixteen large reasoning
models from six families were evaluated on the garden-path materials of
Christianson et al. (2001) under no-thinking and high-thinking settings, and
the reasoning traces and follow-up explanations were coded against a scheme
derived from the psycholinguistic account of garden-path processing.

## Repository structure

```
materials/   What went into the experiment: items, prompts, model list
scripts/     How it was run and analysed: experiment, coding, statistics
results/     What came out: one trial-level data file, one statistics file
```

## materials/

- `items.csv` - the 24 item sets in their three structural variants
  (GP, NGP-comma, NGP-order; 72 sentences) with the comprehension question
  and verb type (OT / RAT) for each.
- `prompts.md` - every prompt used, verbatim: the Q1 comprehension
  instruction, the Q2 follow-up prompt, and the three LLM coding prompts
  for trace codes B, C, and D.
- `models.csv` - the sixteen models: family, API identifier, thinking
  control, the settings used for the no/low/high conditions, and the access
  provider (open-weight models were accessed through Novita AI).

## scripts/

Run order matches the numbering. The statistics scripts read
`../results/trials.xlsx` and print the outputs collected in
`results/statistics_results.txt`; each asserts the expected row counts
(23,040 trials; 23,029 in the core model; 10,216 coded traces; 11,511 in the
length analyses; 1,152 item cells; 10,225 in the reliability universe) so a
data mismatch cannot pass silently.

- `01_run_experiment.py` - the experiment runner, shown in its OpenAI
  version as a representative example. The same two-turn mechanism (Q1
  comprehension question, Q2 follow-up in the same conversation, ten
  independent repetitions per sentence) was used for every provider with its
  native thinking control. Set your API key before running.
- `02_code_traces.ipynb` - batch LLM coding of trace codes B, C, D with
  Gemini 3.6 Flash at temperature 0.1. Code A is deterministic pattern
  matching. Set your API key before running.
- `02_code_A.py` - rule-based coding of trace code A (string match on
  "garden"); recomputes trace_A and followup_A from the text and checks them
  against `results/trials.xlsx`.
- `03_stats_accuracy.R` - accuracy analyses: the core error model
  (GP x verb x thinking), reasoning-length models, and the strongest-four
  reverse-GP analysis (Results sections
  "Overall Error Patterns: Lower Rates, Same Good-Enough Profile",
  "Thinking Improves Accuracy, Especially Where Reanalysis Matters",
  "Reasoning Length Tracks Difficulty and Predicts Accuracy" and
  "The Strongest Models Reveal a Reverse Garden-Path Profile";
  Supplemental File S1, Section S1).
- `04_stats_trace_codes.R` - trace-code analyses: prevalence by condition,
  error by code, the A x B' interaction, cohort comparisons, and the
  strongest-four-only refits (Results sections
  "Trace Content Is Consistent with Typical Garden-Path Processing, with Two Exceptions",
  "Recognition and Resolution Track GP Accuracy; Pragmatic Override Tracks NGP-OT Errors"
  and "The Strongest Models Amplify the Overall Pattern into the Reverse Garden-Path Profile";
  Supplemental File S1, Sections S2-S3).
- `05_stats_trace_vs_followup.R` - the trace vs follow-up comparison:
  cell-level similarity (Wilcoxon, Lin's CCC), mixed models on the retained
  rung of the fallback ladder, likelihood-ratio tests, post hoc contrasts
  (Results section "Comparing the Trace with the Follow-Up Explanation";
  Supplemental File S1, Section S4).
- `06_stats_reliability.R` - inter-coder reliability of the trace coding:
  the primary LLM coder against the second coder on all eligible traces, and
  the human-coded validation sample against both (agreement, Cohen's kappa,
  Krippendorff's alpha, confusion matrices; Materials & Methods section
  "Trace Coding" and Table 2).

All statistics scripts read `results/trials.xlsx` only. The mixed-effects
fits in script 05 are slow (hours in total). Software: R 4.3.3 with lme4
1.1-35.5, emmeans 1.11.2, dplyr, readxl; Python 3 with pandas, openpyxl.

## results/

- `trials.xlsx` - the complete trial-level dataset in one file: 23,040 rows,
  every baseline and high-thinking trial (11,520 each). One row = one trial,
  carrying the sentence and question, the model and thinking setting, the raw
  and normalised answer with the error flag (blank for the 11 responses that
  could not be normalised), token counts, the Q1 reasoning trace text with
  its visibility ratio and the trace codes of all three coders (primary LLM,
  second LLM, and the human validation sample), and the Q2 follow-up
  explanation text with its four codes. The intermediate low-thinking runs
  are not included; no analysis reported in the paper uses them. The
  `codebook` sheet defines every column.
- `statistics_results.txt` - the full printed output of the four
  statistics scripts, in one file, with numbered section headers describing
  each statistic and naming the paper section it supports.
- `descriptive_statistics.xlsx` - the raw (unmodelled) percentages behind
  the paper's figures and supplementary tables, computed from `trials.xlsx`:
  error rates by model, condition, and thinking setting (with the human
  comparison values); code rates by model; error by code level with trial
  counts, pooled and by cohort and by model; and code rates in the trace vs
  the follow-up explanation. Sheet `notes` documents each sheet.

## Provenance and terms for the model outputs

All responses in this repository, including reasoning traces, are
AI-generated model outputs and may be inaccurate; factual assertions in them
should not be relied on without independent verification, and they do not
reflect the views of the model providers.

- Claude, Gemini, and GPT traces are provider-generated summaries of the
  underlying reasoning, returned by the respective APIs. DeepSeek, GLM, and
  Qwen traces are raw reasoning tokens, obtained via Novita AI.
- Outputs generated via the OpenAI, Anthropic, and Google APIs are subject
  to those providers' terms prohibiting use of the outputs to develop or
  train competing AI models. Downstream users of this dataset should respect
  these origin terms.
- Providers may generate identical or similar outputs for other users; no
  exclusivity is claimed over any output.
- Open-weight model licenses (DeepSeek-V3.1: MIT; GLM-4.7/GLM-5: MIT;
  Qwen3-235B-A22B / Qwen3-Next-80B-A3B: Apache 2.0) place no conditions on
  outputs.

## License

The code in this repository is released under the MIT License, and the data
(the contents of `results/` and `materials/`) under CC BY 4.0; see LICENSE.
