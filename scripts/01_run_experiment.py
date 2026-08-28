# 01_run_experiment.py
# Experiment runner, shown here in its OpenAI (Responses API) version as a
# representative example. The same two-turn mechanism (Q1 comprehension
# question with the system instruction below, then Q2 follow-up in the same
# conversation) was used for every provider, with each provider's native
# thinking control (see materials/models.csv): Anthropic and Google through
# their own SDKs, and the open-weight models (DeepSeek, GLM, Qwen) through
# the Novita AI API. Each trial is an independent API call; context is reset
# between repetitions.
#
## Responses API — Q1 with n=NUM_TRIALS, then Q2 follow-ups with conversation array
import time
import pandas as pd
import openai

ROOT_PATH = "../materials"
MODEL = "gpt-5-2025-08-07"
# gpt-5-mini-2025-08-07, gpt-5-2025-08-07, gpt-5.2-pro-2025-12-11, gpt-5.2-2025-12-11
NUM_TRIALS = 10
q1_effort = "high"
q2_effort = "minimal"
MAX_RETRIES = 10
api_key = "YOUR_OPENAI_API_KEY"

client = openai.OpenAI(api_key=api_key)
df = pd.read_csv(f"{ROOT_PATH}/items.csv").rename(columns={"sentence": "sent", "question": "ques", "condition": "GP"})

Q1_INSTRUCTIONS = (
    "Read the sentence and answer the subsequent question with either 'Yes' or 'No' only. "
    "Return exactly one token: Yes or No."
)


def normalize_yesno(text: str) -> str:
    t = text.strip()
    if "Yes" in t and "No" in t:
        return t.split()[-1]
    if "Yes" in t:
        return "Yes"
    if "No" in t:
        return "No"
    return t

def extract_reasoning_summary(resp) -> str:
    parts = []
    for item in resp.output:
        if item.type == "reasoning":
            for s in item.summary or []:
                if s.type == "summary_text" and s.text:
                    parts.append(s.text.strip())
    return "\n".join(parts)


def extract_output_text(resp) -> str:
    for item in resp.output:
        if item.type == "message":
            for content in item.content:
                if content.type == "output_text":
                    return content.text
    return ""


def extract_usage(resp) -> dict:
    u = resp.usage
    details = u.output_tokens_details
    return {
        "input_tokens": u.input_tokens,
        "output_tokens": u.output_tokens,
        "reasoning_tokens": getattr(details, "reasoning_tokens", 0) or 0,
        "total_tokens": u.total_tokens,
    }



def call_with_retry(func, *args, **kwargs):
    for attempt in range(MAX_RETRIES):
        try:
            return func(*args, **kwargs)
        except openai.RateLimitError:
            print(f"    429 rate limit, sleeping 60s... (attempt {attempt+1})")
            time.sleep(60)
        except Exception as e:
            print(f"    Error ({type(e).__name__}): {e}, retrying in 10s... (attempt {attempt+1})")
            time.sleep(10)
    return None


def process_row(sent, ques):
    """Q1: NUM_TRIALS separate calls, Q2: one follow-up per trial."""

    results = []
    for t in range(NUM_TRIALS):
        # ── Q1 ────────────────────────────────────────────────────────────
        resp1 = call_with_retry(
            client.responses.create,
            model=MODEL,
            input=[
                {"role": "user", "content": f"Sentence: {sent}"},
                {"role": "user", "content": f"Question: {ques}"},
            ],
            instructions=Q1_INSTRUCTIONS,
            reasoning={"effort": q1_effort, "summary": "detailed"},
        )

        if resp1 is None:
            results.append({"q1_answer": "", "q1_thinking": "", "q2_answer": "", "error": "q1_failed"})
            continue

        q1_answer = normalize_yesno(extract_output_text(resp1))
        q1_thinking = extract_reasoning_summary(resp1)
        q1_usage = extract_usage(resp1)

        # ── Q2 ────────────────────────────────────────────────────────────
        resp2 = call_with_retry(
            client.responses.create,
            model=MODEL,
            previous_response_id=resp1.id,
            input=[
                {"role": "user", "content": f"You answered '{q1_answer}'. Please explain why you answered yes or no."},
            ],
            reasoning={"effort": q2_effort, "summary": "detailed"},
        )

        if resp2 is None:
            q2_answer = ""
            q2_usage = {"input_tokens": 0, "output_tokens": 0, "reasoning_tokens": 0, "total_tokens": 0}
        else:
            q2_answer = extract_output_text(resp2).strip()
            q2_usage = extract_usage(resp2)

        results.append({
            "q1_answer": q1_answer,
            "q1_thinking": q1_thinking,
            **{f"q1_{k}": v for k, v in q1_usage.items()},
            "q2_answer": q2_answer,
            **{f"q2_{k}": v for k, v in q2_usage.items()},
        })

        print(f"    Trial {t+1}: Q1={q1_answer} | Q2={q2_answer[:60]}...")

    return results


# ── Main loop ─────────────────────────────────────────────────────────────────

rows = []
for index, row in df.iterrows():
    itemno, sent, ques, verb_type, gp = row["itemno"], row["sent"], row["ques"], row["verb_type"], row["GP"]
    print(f"Row {index+1}/{len(df)}: {sent[:50]}...")

    trial_results = process_row(sent, ques)

    for i, r in enumerate(trial_results):
        rows.append({"itemno": itemno, "sent": sent, "ques": ques, "verb_type": verb_type, "GP": gp, "model": MODEL, "trial": i + 1, **r})

    # Save progress every 5 rows
    if (index + 1) % 5 == 0:
        print(f"  Progress saved ({index+1} rows)...")
        pd.DataFrame(rows).to_excel(f"./results_progress_{MODEL}_{q1_effort}.xlsx", index=False)

    time.sleep(0.1)

pd.DataFrame(rows).to_excel(f"./results_{MODEL}_{q1_effort}.xlsx", index=False)
print(f"Done. Exported {len(rows)} rows ({len(df)} sentences x {NUM_TRIALS} trials) to results.xlsx")
