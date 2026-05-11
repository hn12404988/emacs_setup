---
name: tldr
description: Tell the user a short story that helps them understand a file or the most recent discussion. Use a setup-build-turn-close narrative arc (起承轉合). Skip details on purpose — the goal is understanding, not coverage. Output is in simple English. Do not use this skill unless you are explicitly asked to.
---

# /tldr — A Short Story to Help You Understand

## Purpose

Take one complex thing — a file, or a recent discussion — and tell it back as a short story. The reader should walk away **understanding the core idea**, not holding a copy of every detail.

This is the opposite of a complete summary. Details are dropped on purpose. If the reader wants details, they can read the source. Your job is to make the *shape* of the idea click in their head.

Think of it like this: a friend asks "what is this thing about?" and you give a two-minute answer that actually makes them get it. You do not read them the table of contents.

## Two Modes

The skill takes one optional argument: `$ARGUMENTS`.

1. **File mode** — `$ARGUMENTS` is a file path:
   - Resolve relative paths against the current working directory.
   - Read the file with the Read tool. For large files, read enough to grasp the whole story — you do not need every line.
   - If the file cannot be read, report the error and stop.

2. **Discussion mode** — `$ARGUMENTS` is empty:
   - The subject is the **most recent discussion only**, not the whole session.
   - Scope: the user's latest request, and the work done in response to it, up to now. If the session shifted topics earlier, leave older topics out.
   - If scope is unclear, prefer the narrower reading (more recent, more focused). In one short line before the story, state what you treated as "the most recent discussion" so the user can correct you.
   - Cover what mattered in that discussion — decisions, turns, corrections. Skip noise.
   - Do not launch new exploration tools unless the discussion already used them. You may re-read files that were central to the discussion.

## Language Rule

**Output is always in simple English**, regardless of the source language (English, Traditional Chinese, Japanese, etc.).

- The reader is not a native English speaker. Use short sentences. Common words. Plain phrasing.
- Keep proper nouns and identifiers verbatim: `useState`, `git rebase`, file paths, function names, flags, numbers.
- Quoted source text can stay in its original language, but the words around it must be simple English.

Simple words — but do not drop ideas. If the source has three connected ideas, all three should be in the story, just told plainly. Never trade clarity for completeness, and never trade completeness for clarity.

Note: this `SKILL.md` file itself is in English because it is instructions for the assistant. The rule above is about the *output shown to the user*.

## Output Structure — A Story in Four Beats

Tell the story using the classic four-beat arc 起承轉合: **Setup → Build → Turn → Close**. Each beat is short — usually a few sentences. The four beats flow into each other.

A reader should read the whole thing top to bottom in under two minutes and walk away with a clear mental picture.

### Setup — Where this comes from
What is this thing? What world does it live in? What problem or question made it exist?
- File mode: what kind of file it is, what role it plays, what it is responding to.
- Discussion mode: what the user came in wanting, and what shape the conversation took.

Do not start with a dry definition. Start with the situation that makes the rest make sense.

### Build — The core idea
Now explain what the thing actually does, or where the discussion actually arrived. This is the main body of the story — the "what is going on here" part.

Stay at the level of *ideas and shape*, not mechanics. The reader should understand the design or the direction, not the exact lines of code.

### Turn — The twist or the key insight
Every good story has a turn. Find it:
- A tension or trade-off the file is solving.
- A non-obvious choice — why it is built *this way* and not the obvious way.
- A surprise or correction in the discussion — where the user changed direction, or where something unexpected came up.
- A subtle point that, if missed, makes the whole thing confusing.

If you cannot find a turn, the story will feel flat. Look harder. There is almost always one.

### Close — What to walk away with
End with the single takeaway. One short paragraph. What should the reader hold in their head after reading?

This is not a recap of the previous beats. It is the *point* of the story — the reason the reader is better off than before they read it.

## What to Drop on Purpose

This skill is about understanding, not coverage. Actively cut:
- Exhaustive lists of every section, every function, every step.
- Exact numbers, exact identifiers, edge cases — unless one of them *is* the turn.
- Anything the reader can easily find by skimming the source themselves.
- Anything that does not move the story forward.

If you are tempted to write "for completeness, also note that…", stop. That sentence is the thing this skill is built to avoid.

## Style

- Tell, do not list. Sentences and short paragraphs, not bullet walls.
- Simple English. Short sentences. Common words.
- No meta openings ("This file is about…", "In this session we…"). Start inside the story.
- No personal commentary or opinions unless they appear in the source.
- Keep code-like names verbatim: function names, paths, commands, flags.
- Markdown headings as specified (`###` for each of the four beats). No other decoration.
- Length target: short. If it grows past ~400 words, you are probably putting details back in. Cut.
