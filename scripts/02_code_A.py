# 02_code_A.py
# ===========================================================================
# Rule-based coding of trace code A (construction named; Materials & Methods
# section "Trace Coding").
#
# A = 1 if the text contains the string "garden" in any letter case, else 0.
# No stimulus sentence contains "garden" (materials/items.csv), so a match
# comes from the model's own text.
#
# Applied to
#   q1_trace        -> trace_A     (high-thinking trials, N = 11,520)
#   q2_explanation  -> followup_A  (all trials, N = 23,040)
#
# The script recomputes both columns from the text in results/trials.xlsx
# and checks them against the stored values.
# Software: Python 3, pandas, openpyxl.
# ===========================================================================

import pandas as pd

ROOT_PATH = ".."
TRIALS = f"{ROOT_PATH}/results/trials.xlsx"
ITEMS = f"{ROOT_PATH}/materials/items.csv"
PATTERN = "garden"


def code_A(text: pd.Series) -> pd.Series:
    return text.str.contains(PATTERN, case=False, na=False).astype(int)


items = pd.read_csv(ITEMS)
assert not items["sentence"].str.contains(PATTERN, case=False).any()

df = pd.read_excel(TRIALS, sheet_name="trials")
assert len(df) == 23040

high = df["thinking_condition"] == "high"
assert high.sum() == 11520

trace_A = code_A(df.loc[high, "q1_trace"])
followup_A = code_A(df["q2_explanation"])

n_trace = int((trace_A == df.loc[high, "trace_A"]).sum())
n_follow = int((followup_A == df["followup_A"]).sum())
print(f"trace_A    reproduced: {n_trace} / {high.sum()}")
print(f"followup_A reproduced: {n_follow} / {len(df)}")
assert n_trace == high.sum() and n_follow == len(df)
