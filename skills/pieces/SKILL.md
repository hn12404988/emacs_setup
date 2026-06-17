---
name: pieces
description: Turn on "pieces mode". Save the complete answer to a /tmp file, then explain it to the user one small piece at a time, in simple English, moving on only when the user says "next". Use after the agent has shown a wall of complex context. The mode stays on for the session until the user says stop. Do not turn this on yourself — only when the user asks.
disable-model-invocation: true
user-invocable: true
argument-hint: [optional: a file path, a topic, or nothing for the current context]
---

# /pieces — One Small Piece at a Time

## Purpose

When the user is buried under a wall of complex information — a long answer, a
list of questions, a dense file — this skill slows everything down.

Two things make it work:
1. The **complete** answer is saved to a file, so nothing is lost.
2. The chat shows the idea **one small piece at a time**, in plain English, and
   the user controls the pace by saying **"next"**.

This turns on a **mode**. It stays on for the rest of the session (until the
user says stop). While it is on, the agent does not dump walls of text into the
chat. Big answers go to a file; the chat shows one small piece.

## When to use

- Right after the agent gives a wall of complex context, options, or questions.
- The user turns it on themselves. **Never turn this on by yourself.**

## What it works on (the argument — anything)

- **No argument** → the most recent complex content already in the session (the
  wall the user just received). This is the main case.
- **A file path** → read the file; treat its content as the thing to break down.
- **A topic or term** → the subject is that topic, grounded in the current
  session and project.

## The two things that happen when the user turns it on

1. **Save the complete answer to a file.**
   - Make a file: run `mktemp /tmp/pieces-XXXXXX.md` to get a unique path.
   - Write the **complete** answer there — full detail, nothing dropped. The
     file is the whole thing.
   - Tell the user the path in one short line, so they can open the full version
     any time.

2. **Start the piece-by-piece delivery.**
   - Give **one** small piece, in simple English. Then **stop** and wait.

## The piece-by-piece protocol

- One piece = one idea, small enough to read in a few seconds. Short sentences,
  common words.
- After each piece, **stop**. Do **not** move on by yourself. Do **not** give two
  pieces at once.
- The user says **"next"** → give the next piece.
- The user may also ask about the current piece ("explain more", "why?"). Answer
  that, still piece-sized and in simple words, then wait again. **Only "next"
  moves forward.**
- End each piece with a tiny marker so the user knows how far along they are,
  e.g. `(piece 2 of 6)`. You know the count from how you split the file.
- After the last piece, say it is the end, and point again to the /tmp file for
  the full version.

## The persisting mode (the important part)

Once turned on, **pieces mode stays on for the rest of the session.**

- For any **complex or long** answer from then on: first write the complete
  answer to a fresh `/tmp/pieces-XXXXXX.md`, then deliver it piece by piece, same
  protocol.
- For **short, simple** answers (a yes/no, a one-liner, a quick confirmation):
  answer directly. Do not force a tiny answer through the file + piece machinery.
- The mode ends when the user says **"stop"**, **"exit pieces mode"**, or similar
  plain words. Then go back to normal.

## Language rule

**Output is always in simple English**, regardless of the source language.

- The reader is not a native English speaker. Short sentences. Common words.
- Keep proper nouns and identifiers verbatim: `useState`, `git rebase`, file
  paths, function names, flags, numbers.
- Quoted source text can stay in its original language; the words around it are
  simple English.

Simple words — but do not drop ideas. The **file** holds every detail. The
**pieces** still carry all the ideas, just spread out and told plainly. Never
trade clarity for completeness, and never trade completeness for clarity.

Note: this `SKILL.md` is in English because it is instructions for the
assistant. The rule above is about the *output shown to the user*.

## Style

- Within a piece: tell, do not list. A piece is a few sentences, not a bullet
  wall.
- No meta openings ("This file is about…", "In this session we…").
- Keep each piece small. If a "piece" needs more than ~5 sentences, it is
  probably two pieces — split it.
