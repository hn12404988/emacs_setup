---
name: bounce
description: Hand off the current work to a fresh coding-agent session in a new tmux window, preserving full context via a /tmp dump file. Agent-agnostic — works with either `claude` or `opencode`. Manual-only, triggered when the current session has grown too large but the work must continue. Takes an optional heads-up message from the user, and an optional `--cwd <path>` to start the new window in a different directory.
disable-model-invocation: true
user-invocable: true
argument-hint: [--cwd <path>] <free-form message from user>
---

# /bounce — Hand off work to a fresh agent session in a new tmux window

## Purpose

The user invokes `/bounce` when the current coding-agent session is too big to keep working in, but the task itself is not done. The skill transfers everything the next session needs — background, what was done, what's still ahead, current state, and any heads-up from the user — into a fresh session running in a new tmux window in the same tmux session.

The skill is **agent-agnostic**: the new window can run either `claude` or `opencode`. It asks the user which one to launch.

This skill **takes action**: it creates a tmux window, writes a file to `/tmp`, and types into another window via `tmux send-keys`. It is the opposite of `thinker`.

## Hard rules

- **Tmux is required.** If `$TMUX` is empty (the skill is not running inside a tmux session), stop immediately. Tell the user: bounce only works inside tmux, no action taken.
- **Manual only.** Do not invoke this skill on your own. It is `user-invocable: true` for a reason — only run when the user types `/bounce`.
- **Agent choice is asked, not assumed.** Use `AskUserQuestion` to let the user pick `claude` or `opencode`. Do not pick on the user's behalf, even if you can guess.
- **Stop on tmux errors.** If `tmux new-window` fails for any reason, stop and report the exact tmux error message. Do not write the dump file in that case — there is no destination to bounce to.
- **Validate `--cwd` before any side effect.** If the user passes `--cwd <path>` but the resolved path does not exist or is not a directory, stop and report the resolved path. Do not ask which agent to launch, do not create a window, do not write a dump. Fail fast so no state is left behind.
- **The dump file is the source of truth.** The prompt sent to the new window must be short. All real context lives in `/tmp/bounce-<id>.md`. Do not duplicate the dump's content in the prompt.
- **Language mirroring.** The dump's prose language mirrors the user's heads-up message language. If the heads-up is empty, write the dump in English. Identifiers, paths, file names, commands, and code stay verbatim regardless.

## Steps

### 1. Verify tmux

Check `$TMUX`. If empty, stop and tell the user. Do nothing else.

### 2. Parse arguments

The argument string passed to `/bounce` is split into an optional `--cwd <path>` and the heads-up message (everything else, verbatim).

- If the first whitespace-separated token is exactly `--cwd`, the next token is `<path>` and the remainder is the heads-up message. The heads-up may be empty.
- Otherwise, no `--cwd` was given. The entire argument string is the heads-up message (may be empty).

If `--cwd` was given, resolve `<path>` to an absolute path:

- Absolute paths (starting with `/`) are used as-is.
- `~` and `~/…` are expanded against `$HOME`.
- Relative paths are resolved against the **current pane's directory** (`tmux display-message -p '#{pane_current_path}'`), **not** against your own `pwd`. The current pane is what the user sees; your `pwd` may be different.

Then verify the resolved path exists and is a directory (`[ -d "$resolved" ]`). If not, stop. Report the exact resolved path and say no window was created and no dump was written.

Call the results `<target-dir>` and `<user-message>`. If no `--cwd` was given, `<target-dir>` is filled in by step 4 (from the current pane's path).

### 3. Ask which agent to launch

Use `AskUserQuestion` with exactly two options: `claude` and `opencode`. Do not add a third option, do not default. Whatever the user picks becomes `<AGENT_CMD>` in the steps below.

### 4. Read the current tmux coordinates

- Current session: `tmux display-message -p '#S'`
- Current window: `tmux display-message -p '#W'`
- Current pane path: `tmux display-message -p '#{pane_current_path}'`

If `--cwd` was **not** given in step 2, set `<target-dir>` to the current pane path. If `--cwd` was given, keep the `<target-dir>` already resolved in step 2.

Compute the new window name: `<current-window>-bounce`. If a window with that exact name already exists in the session (`tmux list-windows -t <session>`), append `-2`, then `-3`, etc., until the name is free.

### 5. Create the new window

Run:

```
tmux new-window -d -t <session>: -n <new-window-name> -c <target-dir> '<AGENT_CMD>'
```

- `-d` keeps focus in the current window so the user can read the bounce confirmation here.
- `-c <target-dir>` makes the new window start in the directory chosen in step 2 — the current pane's directory by default, or the `--cwd` path if the user passed one.
- The trailing `'<AGENT_CMD>'` is whichever agent the user picked in step 3 (`claude` or `opencode`).

If this command fails (non-zero exit), stop and report the error. **Do not write the dump file.**

### 6. Build the context dump

Pick a path: `/tmp/bounce-<session>-<window>-<timestamp>.md`, where `<timestamp>` is e.g. `YYYYMMDD-HHMMSS`. Write the dump to this path.

The dump **must include all of the following sections**, in order. Use `##` headings. Skip a section only if it is genuinely not applicable, and say so explicitly (`(none)`).

1. **Background** — The project, the larger goal the user has been pursuing, and any environment context the next session needs (working directory, OS, key tools in use). Enough that the next session does not need to ask "what are we doing here".

2. **What was done in this session** — Concrete actions taken so far that are *related* to what comes next: files edited, decisions made, things ruled out, things confirmed working. Do not list unrelated tangents.

3. **What's going to be done next** — The upcoming task in full detail. Goals, requirements, constraints. The next session should be able to start work from this section alone.

4. **Working state** — Current working directory of the work just done, current git branch, dirty / staged / untracked files, recent commits relevant to the task, and any running background processes (dev servers, watchers) the next session should know about. **If the new window starts in a different directory** (the user passed `--cwd`), also state the new starting directory (`<target-dir>`) explicitly and warn the next session that file paths elsewhere in this dump refer to the *previous* directory unless otherwise noted. Do not tell the next session to `cd` back — the user moved them on purpose.

5. **Files of interest** — Paths the next session will likely read or modify. Each entry: `path/to/file.ext — one-line role`. Keep the list tight (the most relevant 5–15), not exhaustive.

6. **Decisions and rationale** — Non-obvious choices made in this session and *why*. The next session inherits these; without the *why*, it may second-guess them.

7. **Open questions** — Things still undecided, ambiguous, or waiting on the user. Phrase as questions, not as TODOs.

8. **User preferences observed in this session** — Preferences the user expressed during this session that are *not* already in `CLAUDE.md` or memory (e.g. "user prefers terse commit messages this time", "user wants no comments in this file"). Skip if there are none.

9. **Heads-up from the human** — The verbatim message the user passed to `/bounce` (the skill argument). If empty, write `(none)`. Do not paraphrase.

10. **Concrete next steps** — The first 1–3 things the new session should do, in order. This is the on-ramp — it should match what the heads-up message asks for, if any.

### 7. Send the bounce prompt to the new window

Compose a short prompt. Template (English):

```
Continuation of work bounced from another tmux window in the same session.
Full context is in: /tmp/bounce-<id>.md
Please read that file first, then continue the work described there.

Heads-up from human: <USER_MESSAGE_VERBATIM_OR_(none)>
```

Or the equivalent in Traditional Chinese if the user's heads-up is in Chinese.

Send it to the new window:

```
sleep 2
tmux send-keys -t <session>:<new-window-name> -l '<PROMPT_TEXT>'
tmux send-keys -t <session>:<new-window-name> Enter
```

- The `sleep 2` gives the agent (`claude` or `opencode`) time to start up in the new window before keys are typed. Without it, keys may be eaten by the shell or by the agent's startup screen. If the agent feels slow on the user's machine, bump to 3.
- `-l` sends keys literally (no keybinding interpretation), which matters for prompts containing special characters.
- The final `Enter` submits the prompt.

If `tmux send-keys` fails, report the error. The dump file is already written, so the user can paste it manually as a fallback.

### 8. Confirm to the user

A 2–3 line confirmation in the current window. Include:
- The agent that was launched (`claude` or `opencode`).
- The new window name (so the user can switch to it with `C-a w/s` or `C-a M-N`).
- The new window's working directory (`<target-dir>`). If it came from `--cwd`, say so explicitly.
- The dump file path (so the user can read it themselves if curious).
- The heads-up message that was forwarded (verbatim), or `(none)`.

That ends the skill's job.

## Tone and style

- The skill takes action. Output to the *current* user is brief — a short confirmation, nothing more.
- The dump file is for the *next* agent to read, not for the user. Write it as if briefing a colleague who just walked into the room: complete, scannable, no filler.
- Identifiers, paths, commands stay verbatim everywhere.
- No emoji.
- No meta openings ("Here is the dump…"). Go straight into the structure.
