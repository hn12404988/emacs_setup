---
name: collect
description: Produce a clean, up-to-date summary of the current discussion — present state only, no history, no pivots, no "we used to think" narrative.
disable-model-invocation: true
user-invocable: true
argument-hint: <optional free-form message from user>
---

# /collect — Fresh Summary of Current State

## What this does

Produce a clean, present-tense summary of where the current discussion stands **right now**. Write it as if a new person just walked in and asked "what is this?"

If the user passed a message after `/collect`, treat it as scope (e.g. "just the auth part"). If no message, summarize the whole current discussion.

## The one rule

**Write only what is true now. Drop everything that has been superseded — and do not mention that you dropped it.**

If a sentence only makes sense because of a *prior* version of the discussion, delete it. Do **not** replace it with a "we no longer…" sentence. Just remove it.

The reader of your summary should not be able to tell that the discussion ever changed direction.

## Forbidden

- Phrases: "originally", "initially", "previously", "at first", "used to", "no longer", "pivoted", "evolved", "revised", "replaced", "we thought", "we considered", "we tried", "we learned", "turned out"
- Sections named: History, Journey, Pivots, What we tried, Lessons learned, Evolution
- Parentheticals like "(previously X)" or "(originally Y)"
- Justifying the current design by contrasting it with a discarded one
- Listing resolved questions as if they were still open

## Required

- Current design, decisions, and architecture — in present tense
- Open questions still unresolved **now**
- Next steps / TODOs still active
- Constraints and assumptions still in force

State the design. Don't defend it.

## Self-check before delivering

Scan your draft for the forbidden phrases above. If any appear, rewrite to remove them.

Final check: if a reader could not tell from the summary alone that the discussion ever changed direction — good. That is the goal.
