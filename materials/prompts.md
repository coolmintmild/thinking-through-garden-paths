# Prompts

All prompts used in the experiment and in the trace coding, verbatim.

## 1. Experiment, Q1 (comprehension question)

System instruction (delivered through each provider's system-instruction channel):

```
Read the sentence and answer the subsequent question with either 'Yes' or 'No' only. Return exactly one token: Yes or No.
```

User turns (two separate user messages):

```
Sentence: {sentence}
```
```
Question: {question}
```

## 2. Experiment, Q2 (follow-up explanation)

Sent in the same conversation after the model's Q1 answer:

```
You answered '{q1_answer}'. Please explain why you answered yes or no.
```

## 3. Trace coding

Code A (explicit garden-path recognition) was coded by deterministic pattern
matching on the trace text, not by prompt. Codes B, C, and D were coded by
Gemini 3.6 Flash (gemini-3.6-flash) at temperature 0.1 through the Batch API
(scripts/02_code_traces.ipynb), with the prompt as the system instruction and
the trace as the user turn. {sentence} and, for C, {verb} were filled per
trial. Traces returning a value outside the coding range were excluded from
that code.

### Code B: structural difficulty and its resolution

```
You are a qualitative coder. Read the reasoning trace below and answer one question.

The sentence under discussion is:
"{sentence}"

Question: What is the trace's final verdict on the structure of this sentence?

Judge by where the trace ENDS UP, not by moments of confusion along the way.

- 0: No difficulty. The trace parses the sentence smoothly and never flags any
  structural problem, confusion, or ungrammaticality.
- 1: Difficulty resolved as valid. The trace notices structural difficulty,
  ambiguity, confusion, or a missing/omitted comma, but ultimately concludes the
  sentence IS a complete, valid sentence with a correct parse (e.g. "this looks
  broken at first, but it is actually a well-formed garden-path sentence").
  Noting only a missing or omitted comma, or calling the sentence merely
  "awkward" or "confusing", while still parsing it, counts as 1.
- 2: Treated as ungrammatical. The trace treats the sentence as ungrammatical, a
  fragment, broken, defective, incomplete, a run-on, an error, or as needing to
  be fixed/rewritten, AND does not retract this by concluding it is actually
  valid. Code 2 even if:
    - the claim is hedged ("possibly a fragment", "seems incomplete"), and
    - the trace still extracts an interpretation or answers the question
      ("despite being a fragment, it clearly says X").
  Extracting meaning from a sentence the trace regards as defective does NOT
  upgrade it to 1.
- 3: Cannot decide.

Notes:
- Calling the sentence "confusing", "awkward", or "a garden-path" is not by
  itself a verdict of ungrammaticality. Code 2 only if the trace treats the
  sentence itself as invalid.
- If the trace raises a defect (fragment, broken, incomplete, ungrammatical) and
  never retracts it, code 2, even if it still answers. Code 1 only when the trace
  explicitly resolves the sentence as valid/complete. A missing or omitted comma
  is not a defect for this purpose; parsing despite it is 1.

You MUST respond with ONLY a single digit: 0, 1, 2, or 3. No words, no explanation.
```

### Code C: the object of the subordinate verb

```
You are a qualitative coder. Read the reasoning trace below and answer one question.

The sentence under discussion is:
"{sentence}"

The target verb is "{verb}", the verb of the subordinate clause (the clause
introduced by "while" or "as").

Question: How does the trace treat the object of "{verb}"?

An "object expectation" is any sign that the trace treats "{verb}" as needing,
taking, or missing an object: it considers the following noun phrase as the
object, or it frames the absent object as a gap (e.g. "it doesn\'t say what he
was hunting", "the object is unstated", "hunted something unspecified").

- 0: No object expectation. The trace consistently accepts the objectless
  reading of "{verb}" (intransitive, or self-directed for verbs like "washed"
  = washed himself) and never treats the missing object as an issue.
- 1: Object expectation appears but is dropped. The trace considers the noun
  phrase as a possible object, or flags the unstated object, but ultimately
  lets this go and settles on the objectless reading.
- 2: Object expectation persists. In the trace\'s final analysis, "{verb}" still
  has an object in play: it maintains that something unstated was acted on
  (e.g. "he was hunting something, we just aren\'t told what"), keeps the
  object as implicit or missing, or attaches the noun phrase as the object.
- 3: Cannot decide.

Examples:
- "the verb is used intransitively here" / "the thief hid (himself)" and the
  object question never arises -> 0
- "at first it looks like \'the deer\' is the object, but it is actually the
  subject of the main clause; \'hunted\' is intransitive here" -> 1
- "it doesn\'t specify what he was hunting, so we can\'t say it was the deer" ->
  2 (the object is still treated as existing but unstated)
- "he was hunting, and the deer was probably what he hunted" -> 2

You MUST respond with ONLY a single digit: 0, 1, 2, or 3. No words, no explanation.
```

### Code D: pragmatic override

```
You are a qualitative coder. Read the reasoning trace below and answer one question.

The sentence under discussion is:
"{sentence}"

Question: Does the trace use pragmatic inference to fill in or override what the
syntax leaves open, in a way that supports its answer?

Code 1 ONLY if BOTH of the following hold:
(a) The trace acknowledges, explicitly or implicitly, that the syntax alone does
    not state the relevant relation (e.g. the sentence does not say what was
    hunted, or the two clauses are formally separate), AND
(b) The trace then uses context, plausibility, implication, or common sense to
    conclude that the relation holds anyway, and this inference supports the
    answer it gives (e.g. "he was hunting something, and the deer is the obvious
    candidate, so yes, he hunted the deer" or "they are separate clauses, but the
    natural implication is that the deer was what he hunted").

- 0: No. The trace answers from the syntactic parse alone. This includes traces
  that use pragmatic-sounding vocabulary ("the natural reading", "plausibly",
  "common sense") merely to describe or justify the syntactic parse itself,
  without inferring anything beyond what the syntax states.
- 1: Yes. The trace makes the bridging move described above: pragmatics supplies
  a relation the syntax does not state, and that inference feeds the answer.
- 2: Cannot decide.

Key distinction:
- "Syntactically, the deer is the subject of \'ran\', so the sentence does not say
  he hunted the deer. Answer: No." -> 0 (syntax only)
- "The most natural reading is that the deer ran into the woods." -> 0
  (pragmatic vocabulary, but describing the parse, no bridging inference)
- "The sentence doesn\'t state the object of \'hunted\', but in context the deer is
  clearly what he was hunting. Answer: Yes." -> 1

You MUST respond with ONLY a single digit: 0, 1, or 2. No words, no explanation.
```
