---
name: tldr
description: Guide the user to fully understand a file or the most recent discussion, layer by layer, from high-level to details. Not a filtered summary — complete coverage, progressively zoomed in. Output is always in Traditional Chinese (繁體中文), regardless of source language. Do not use this skill unless you are explicitly asked to.
---

# /tldr — Progressive, Complete Understanding

## Purpose

Help the user absorb a large amount of information **completely**, by guiding them step by step from high-level to details, top to bottom.

**This is NOT a "top N important points" filter.** The goal is full coverage — the user should end up understanding *everything* that matters in the source, just delivered in a progressively zoomed-in way so they can build a mental model layer by layer instead of being dumped with a wall of details.

Think of it like a camera zoom: first the whole landscape, then regions, then objects, then textures. Every layer is *complete at its own resolution* — nothing important is dropped, only deferred to a deeper layer.

## Two Modes

The skill takes one optional argument: `$ARGUMENTS`.

1. **File mode** — `$ARGUMENTS` is a file path:
   - Resolve relative paths against the current working directory.
   - Read the file with the Read tool. For large files, read in chunks until the whole file is covered.
   - If the file cannot be read, report the error and stop.

2. **Discussion mode** — `$ARGUMENTS` is empty:
   - The subject is the **most recent discussion only**, *not* the entire session. Sessions can be very long and span many unrelated topics; only the latest thread is in scope.
   - Concretely: scope covers the user's most recent request and all work done in response to it, up to the point `/tldr` is invoked. If the latest discussion spans multiple back-and-forth turns on one coherent topic, include all of them. If the session has clearly shifted topics earlier, exclude the older topics.
   - If the scope is ambiguous, prefer a narrower interpretation (more recent, more focused) over a wider one. Briefly state, in one line before Layer 1, what was included as "the most recent discussion" so the user can redirect if wrong.
   - Within that scope, cover: user messages, assistant replies, tool calls and their results, decisions made, code changes, corrections, and open questions.
   - Do not launch new exploration tools unless the discussion itself already referenced them. Re-read files that were central to the discussion if needed to recall exact details.

## Language Rule (Hard Requirement)

**Output is always in Traditional Chinese (繁體中文)**, regardless of the source language (English, Simplified Chinese, Japanese, etc.).

- Keep proper nouns, identifiers, commands, paths, and code verbatim (e.g. `useState`, `git rebase`, `/path/to/file`).
- Translate general technical terms when there is a well-established Chinese rendering (error → 錯誤, deprecated → 已棄用). Otherwise keep the original term.
- When quoting source text, the quote may stay in the original language, but the surrounding explanation must be in Traditional Chinese.

Note: this `SKILL.md` file itself is in English (it is instructions for the assistant). Only the *output shown to the user* is in Traditional Chinese.

## Output Structure — Layered, Top to Bottom

Produce the layers **in order**. Each layer must stand on its own: a reader who stops after Layer 2 should still have a coherent (if coarse) understanding of the entire source.

Crucially: later layers **expand** earlier ones, they do not introduce unrelated new content. Every concept that appears at a deeper layer should have a trace in the layer above it. This is how "complete guidance" differs from "summary".

### Layer 1 — One-sentence essence
One sentence in Traditional Chinese. What is this thing, at the highest possible level?
- File mode: what the file is and what role it plays.
- Discussion mode: what the user and assistant have been working on in the most recent discussion.

### Layer 2 — The outline
A bulleted outline of **every** major part of the source, in the order it appears.
- File mode: sections, top-level declarations, phases of logic — whatever the natural top-level division is.
- Discussion mode: in chronological order within the most recent discussion — what happened first, next, then, last. Include every distinct sub-topic or task within that discussion, not just the "important" ones.

This layer is the table of contents for Layer 3. If something will show up in Layer 3, it must have a bullet here.

### Layer 3 — Each part, explained
For every item in the Layer 2 outline, give a `###` subsection that explains it in a few lines.
- Use the same order as Layer 2.
- Cover *all* items from Layer 2, even ones that feel minor — completeness matters here.
- For each part, convey: what it does, why it's there, how it connects to adjacent parts.
- Keep each subsection tight (a few lines to a short paragraph), but do not drop content — push finer details into Layer 4 instead.

### Layer 4 — Details worth zooming into
For the parts where details genuinely matter, zoom in further. This layer is where specific mechanics, edge cases, exact values, and subtle behaviors live.
- Organize by referring back to Layer 3 subsections (e.g. `#### Details: <part name>`).
- Include: concrete numbers, exact identifiers, edge cases, error handling, non-obvious interactions, user-stated preferences or corrections (discussion mode), known issues, TODOs.
- It is fine for some Layer 3 parts to have no Layer 4 expansion if they are truly self-contained.
- This layer can be long. Do not compress it at the cost of completeness.

## Completeness vs. Length

- Do not artificially cap the output. The length should be whatever it takes to cover the source completely at the depth described above.
- Do compress *redundancy*: say each thing once, at the right layer. If a detail belongs in Layer 4, don't also spell it out in Layer 2.
- If the source is huge (thousands of lines, a very long session), Layers 1 and 2 stay compact, while Layers 3 and 4 grow proportionally.
- Never skip content because it seems "less important". Defer it to a deeper layer instead.

## Style

- Direct statements of fact. No meta openings like "This file discusses…" or "In this session we…".
- No personal commentary, opinions, or recommendations unless they appear in the source.
- Preserve key names verbatim: function names, variable names, file paths, commands, flags, numbers.
- No emoji unless the source uses them.
- Markdown headings as specified (`###` for Layer 3 parts, `####` for Layer 4 zoom-ins). No other structural decoration.
