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

## Step 1 — Make sure the daemon is running

Run this in the shell. It checks health, downloads the version-pinned binary if
needed, and starts it. The version below is bound to this skill's release.

```sh
PIECES_VERSION=0.2.0
PIECES_PORT=8723
PIECES_DIR="$HOME/.local/share/pieces"
BIN="$PIECES_DIR/bin/pieces-$PIECES_VERSION"

if ! curl -fsS "http://127.0.0.1:$PIECES_PORT/health" >/dev/null 2>&1; then
  if [ ! -x "$BIN" ]; then
    os=$(uname -s); arch=$(uname -m)
    case "$os" in Linux) os=linux;; Darwin) os=darwin;; esac
    case "$arch" in x86_64|amd64) arch=x86_64;; aarch64|arm64) arch=arm64;; esac
    mkdir -p "$PIECES_DIR/bin"
    url="https://github.com/hn12404988/emacs_setup/releases/download/v$PIECES_VERSION/pieces-$os-$arch"
    curl -fsSL "$url" -o "$BIN" && chmod +x "$BIN"
  fi
  nohup "$BIN" --port "$PIECES_PORT" >> "$PIECES_DIR/daemon.log" 2>&1 &
  for i in $(seq 1 50); do
    curl -fsS "http://127.0.0.1:$PIECES_PORT/health" >/dev/null 2>&1 && break
    sleep 0.1
  done
fi
```

## Step 2 — Make sure this project has a thread id

```sh
THREAD_FILE=".claude/pieces-thread"
if [ ! -f "$THREAD_FILE" ]; then
  mkdir -p .claude
  slug=$(basename "$PWD" | tr -c 'a-zA-Z0-9' '-' | sed 's/-\{1,\}/-/g; s/^-//; s/-$//')
  rand=$(head -c3 /dev/urandom | od -An -tx1 | tr -d ' \n')
  echo "${slug}-${rand}" > "$THREAD_FILE"
fi
THREAD=$(cat "$THREAD_FILE")
```

## Step 3 — Build the payload and send it

Write the payload to a temp file (a big JSON full of code does not belong on the
command line), then POST it with the thread header.

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

Send it:

```sh
# PAYLOAD_FILE is the temp file you just wrote the JSON to.
curl -fsS -X POST "http://127.0.0.1:$PIECES_PORT/messages" \
  -H "PIECES-THREAD: $THREAD" -H "Content-Type: application/json" \
  --data @"$PAYLOAD_FILE"
```

The response is `{"url":"/t/<thread>/r/<id>"}`. Tell the user the full URL in one
line, e.g. `http://127.0.0.1:8723/t/<thread>/r/<id>`.

If the POST fails with a validation error, the JSON was malformed (often
unescaped quotes or newlines inside a code body). Fix the JSON and resend.

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
