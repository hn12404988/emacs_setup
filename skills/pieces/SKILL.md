---
name: pieces
description: Turn on "pieces mode". Instead of dumping a wall of complex information into the chat, send it to the local pieces daemon as a JSON payload of small pieces, then point the user at a localhost web page they browse at their own pace. Questions and short answers stay in the terminal. The mode stays on for the session until the user says stop. Do not turn this on yourself — only when the user asks.
disable-model-invocation: true
user-invocable: true
argument-hint: [optional: a file path, a topic, or nothing for the current context]
---

# /pieces — One Small Piece at a Time

## Purpose

When the user is buried under a wall of complex information — a long answer, a
list of questions, a dense file — this skill slows everything down. The complete
answer is broken into small **pieces**, each one idea, and shown on a local web
page the user flips through at their own pace.

This turns on a **mode**. It stays on for the rest of the session (until the user
says stop). While it is on, the agent does not dump walls of text into the chat.
Heavy information goes to the daemon; the chat stays light.

## When to use

- Right after the agent gives (or is about to give) a wall of complex context.
- The user turns it on themselves. **Never turn this on by yourself.**

## What it works on (the argument — anything)

- **No argument** → the most recent complex content already in the session.
- **A file path** → read the file; break its content into pieces.
- **A topic or term** → that topic, grounded in the current session and project.

## What goes where

- **Heavy information → the daemon** (as pieces). The user reads it on the web page.
- **Everything else → the terminal, unchanged.** Questions, short answers, and
  decisions stay in the terminal the normal way. If a question needs a lot of
  background, the background becomes pieces; only the short question stays in chat.

## How the daemon is handled (you do not manage it by hand)

All the mechanics — version check, download, install, start, health, sending —
live in one bundled script: **`${CLAUDE_SKILL_DIR}/pieces.sh`**. You only ever
call it. Do not reimplement any of it (no curl health loops, no download URLs,
no port handling). The script is idempotent — safe to run every time.

## Step 1 — Make sure the daemon is running

```sh
bash "${CLAUDE_SKILL_DIR}/pieces.sh" up
```

That single call checks the pinned version, downloads and installs the right
binary if needed, starts the daemon, and waits until it is healthy. On success
it prints the base URL. (`send` in Step 3 also ensures this, so this step is
mostly for a clean first run. To check state any time: `pieces.sh status`.)

## Step 2 — Build the payload

This is your real job: break the answer into pieces and write the payload JSON
to a temp file (a big JSON full of code does not belong on the command line).

The payload schema:

```jsonc
{
  "title": "short title of this whole answer",
  "thumbnail": "one-line preview, shown in lists",
  "pieces": [
    { "index": 1, "heading": "optional short heading", "body": "markdown — code blocks ok" },
    { "index": 2, "heading": "...", "body": "..." }
  ]
}
```

Write it with a temp file, e.g.:

```sh
PAYLOAD_FILE="$(mktemp)"
# ... write the JSON above into "$PAYLOAD_FILE" ...
```

## Step 3 — Send it

```sh
bash "${CLAUDE_SKILL_DIR}/pieces.sh" send "$PAYLOAD_FILE"
```

The script ensures the daemon, finds or creates this project's thread id, POSTs
the payload, and prints the full URL on stdout — for example
`http://127.0.0.1:8723/t/<thread>/r/<id>`. Tell the user that URL in one line.

If the call exits non-zero, the script already printed the daemon's error
(usually malformed JSON — an unescaped quote or a raw newline inside a code
body). Fix the JSON and run `send` again.

## The piece-by-piece principle (unchanged)

- One piece = one idea, small enough to read in a few seconds. Short sentences,
  common words.
- Pieces are **numbered**. The user refers to them in the terminal ("explain
  piece 3").
- Pieces are the **only** version. Do **not** also write a separate full copy —
  that wastes tokens. The pieces carry every idea, spread out and told plainly.

## Short answers stay direct

For a yes/no, a one-liner, or a quick confirmation, answer directly in the
terminal. Do not push tiny answers through the daemon.

## The persisting mode

Once on, pieces mode stays on for the rest of the session. For any heavy answer,
send it to the daemon as pieces. The mode ends when the user says **"stop"**,
**"exit pieces mode"**, or similar.

## Language rule

**Output is always in simple English**, regardless of the source language. The
reader is not a native English speaker — short sentences, common words. Keep
proper nouns and identifiers verbatim: `useState`, `git rebase`, file paths,
function names, flags, numbers. Simple words — but do not drop ideas.

Note: this `SKILL.md` is in English because it is instructions for the assistant.
The rule above is about the output shown to the user.
