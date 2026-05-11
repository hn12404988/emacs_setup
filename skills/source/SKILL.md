---
name: source
description: Ground a topic, term, or name the user is confused about in concrete source locations. Given a topic as argument, search the current project (or a user-specified directory) and tell a short story about where it lives — definition, the most telling usage (the turn), and where to start reading. Every claim is anchored to a real `file:line`. Output is in simple English. Not for interpreting session/discussion content — this skill targets files on disk.
---

# /source — Ground a Topic with a Short Story of Anchors

## Purpose

The user invokes this skill when a topic feels like a floating label — a term, name, or concept they cannot map to real code. The job of `/source` is to turn that label into a small **story of grounded anchors**: where it is **defined**, the **one most telling place** it is used, and where to **start reading**.

This is not a summary of the topic and not an explanation from general knowledge. It is a grounded pointer into the filesystem, told as a short narrative so the topic clicks in the user's head. Every claim must be backed by a real `file:line` that was actually found on disk.

The shape borrows from `/tldr`: do not dump every match — pick the smallest set of anchors that makes the topic understandable. But unlike `/tldr`, the anchors themselves must stay precise. Precision is what makes this skill useful; storytelling is how you deliver it.

## Input

The skill takes one argument: `$ARGUMENTS`.

- Primary form: a single topic/term/name (e.g. `useAuthContext`, `DERP relay`, `merge freeze`).
- The user may also specify a directory to search in, either inline (`useAuthContext in /path/to/other/project`) or in surrounding prose. If so, search that directory instead of the current working directory.
- If no directory is specified, default to the current working directory.

## Language Rule

**Output is always in simple English**, regardless of the source language or identifier language.

- The reader is not a native English speaker. Use short sentences. Common words. Plain phrasing.
- Keep identifiers, file paths, commands, and code verbatim (e.g. `useState`, `src/auth/context.ts:42`, `cargo build`).
- Quoted source text may stay in its original language. The words around it must be simple English.

Simple words — but do not drop ideas. If the topic has three connected pieces, all three should be in the story, just told plainly.

Note: this `SKILL.md` is in English because it is instructions for the assistant. The rule above is about the *output shown to the user*.

## Search Scope

- Default scope: the current working directory, recursively.
- Override scope: if the user names a different directory in `$ARGUMENTS` or the recent message, use that directory instead.
- Do not search arbitrary paths outside the intended scope. Do not search the web or general knowledge. The answer must come from files on disk.
- Respect the directory's own ignore rules (e.g. `.gitignore`) when using search tools that support them.

## Search Strategy — Definition First, Then the Turn

1. **Interpret the topic.** Consider likely forms: exact identifier, camelCase/snake_case variants, quoted string, constant name, filename, config key, CLI flag. Pick the most plausible forms — do not invent extras.
2. **Find the definition site(s) first.** Use `grep`/`rg` and file reads to locate where the topic is declared. Read enough surrounding code to understand what it is.
3. **Look for the turn.** Skim usage sites with a specific question in mind: *which one of these would make the user say "oh, so that's why"?* That is the anchor worth showing. Candidates:
   - An unexpected place that uses it (a module you would not guess).
   - A usage that reveals the real reason the thing exists.
   - A non-obvious choice — used *this way*, not the obvious way.
   - A tension or trade-off the topic is solving.
4. **If there is no turn, accept that.** Some topics are small utils used in obvious places. Do not invent a twist. Drop into the short flat form (see below).
5. **Do not over-collect.** Pick the *minimum* set of anchors that tells the story — often one definition + one turn + maybe one extra. State the total count of usages if it is informative, but do not list them all.

## When the Topic Cannot Be Found

If no file in scope contains a credible match, do not guess and do not fall back to general knowledge. Respond with a short message in simple English along these lines, then stop:

> Could not find the topic `<topic>` anywhere in the search scope (`<scope path>`). Tried these forms: `<list>`. Could you give more context — which module it might live in, a related file or package name, or where you first saw the term?

If there are only *weak/ambiguous* matches (the term appears only in a comment, or only in a changelog), report them honestly as weak matches — do not promote them to a real definition.

## Output Structure — A Story in Four Beats

Tell the result as a four-beat arc 起承轉合: **Setup → Build → Turn → Close**. Each beat is short. The `file:line` anchors stay structured inside the story — do not turn them into prose. Precision matters.

### Setup — What this is, in this codebase
One short sentence: what the topic *is* here, based on what you found. Grounded, not a dictionary definition.

### Build — Where it is defined
The definition site(s). For each:
- `relative/path/to/file.ext:LINE` — one short line: what is declared there (function / class / type / const / config key / route / etc.).
- If more than one definition exists (e.g. trait + impl, interface + implementation), list each. Keep it tight.

### Turn — The one place that makes it click
The single most telling usage — the anchor that reveals *why* this topic exists or how it is really being used.
- `relative/path/to/file.ext:LINE` — one short line: what is happening there, and why it is the telling one.
- If there is a meaningful total count, note it ("used in 42 places; this one shows the pattern").
- If there is no turn, omit this beat and use the short flat form instead (see below).

### Close — Where to start reading
One short sentence: which file the user should open first if they want to actually understand the topic. This is the takeaway — the reason the reader is better off than before.

### Short flat form (when there is no turn)
For tiny topics — one definition, a few obvious usages, no twist — skip the four beats. Give:

- One sentence: what it is.
- `path:line — one short line` for the definition.
- `path:line — one short line` for one or two usages.

Nothing else. Do not pad.

## What to Drop on Purpose

This skill is about grounding, not coverage. Actively cut:
- Exhaustive lists of every usage site.
- Tests, config, docs — unless one of them *is* the turn.
- Anything the reader can easily find by skimming from the definition site themselves.
- Per-location commentary longer than one sentence.

If you are tempted to write "for completeness, also note that…", stop. That sentence is the thing this skill is built to avoid.

## Style

- Every cited location must be a real `path:line` the assistant actually read or matched. Never fabricate line numbers.
- Prefer paths relative to the search scope root, not absolute paths, unless the user searched an absolute path they specified.
- Preserve identifiers, filenames, and code fragments verbatim.
- Simple English. Short sentences. Common words.
- No meta openings ("Here are the search results…"). Start inside the story.
- No personal opinions or speculation about code quality.
- Markdown headings as specified (`###` for each beat). No other decoration.
- Length target: short. If it grows past ~400 words, you are probably listing too many anchors. Cut.
