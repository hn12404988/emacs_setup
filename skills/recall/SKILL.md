---
name: recall
description: Catch the user up after a pause. Surface only what needs them — decisions waiting, blockers, active follow-ups — not everything. The agent holds the detail; the user just makes the calls.
disable-model-invocation: true
user-invocable: true
argument-hint: <optional free-form message from user>
---

# /recall — What Needs You Right Now

## What this does

The user stepped away from this session for a while and has forgotten where things
stand. Catch them up — but only on the parts that need *them*.

The split of roles is the whole point:

- **You hold everything.** All the detail, history, and mechanics stay with you.
- **The user only makes calls.** They decide, they review the few things that need
  a human judgment, they pick the next move.

So do not replay the session. Surface the decisions waiting on the user, the
things blocking that only they can clear, and the follow-ups still open — plus the
small amount of context needed to understand those. Nothing else.

If the user passed a message after `/recall`, treat it as scope (e.g. "just the
deploy part"). If no message, recall the whole current line of work.

## The one rule

**Every line you write must be something the user needs to act on, or the minimum
context to understand it. If a line is neither, cut it.**

Three things earn a place:
1. A decision or call waiting on the user.
2. A follow-up, TODO, or action still open.
3. The minimum orientation to understand 1 and 2.

Everything else — resolved questions, things you can handle yourself without
asking, full history, exact mechanics — stays with you. Do not present it.

## Source

**Use the conversation only. Do not run tools.**

The subject is the current line of work in this session, as it stands now. No git
lookups, no file reads, no exploration. If context was compacted earlier and some
detail is gone, work from what remains — do not go digging to rebuild it.

## Output structure — calls first

Lead with what needs the user. Orientation comes last, and stays short.

### What needs you
The decisions and blockers waiting on the user. This is the top of the output
because it is why they are reading.

For each item, give:
- The call in one line.
- The minimum context to make it — just enough, not the full story.
- The options, if you already know them.

If nothing is waiting on the user, say so in one line. Do not invent calls.

### Open follow-ups
The TODOs and actions still in flight — what is queued, what is half-done, what
comes next. Short lines. Mark anything blocked and what it is blocked on.

These are things in motion, not decisions. Keep them separate from "what needs you"
so the user can see at a glance what is theirs to decide versus what is already
moving.

### Where we are
A few sentences of orientation so the user remembers the world they are stepping
back into. Just enough to make the two sections above make sense. This is the
shortest section, not the longest — resist turning it into a full recap.

## What to drop on purpose

- Full history and how the work got here.
- Resolved questions and closed decisions.
- Anything you can do yourself without a decision from the user.
- Exact mechanics, line-level detail, exhaustive file lists.
- "For completeness…" anything. If you reach for that phrase, stop.

## Language Rule

**Output is always in simple English**, regardless of the source language.

- The reader is not a native English speaker. Short sentences. Common words. Plain
  phrasing.
- Keep proper nouns and identifiers verbatim: file paths, function names, commands,
  flags, numbers.

Simple words — but do not drop a call or a follow-up to be brief. If three things
need the user, all three appear. Never trade completeness of *what needs them* for
brevity.

Note: this `SKILL.md` is in English because it is instructions for the assistant.
The rule above is about the output shown to the user.

## Style

- Lead with "What needs you." Orientation is last and short.
- `###` headings for the three sections. No other decoration. Drop a section's
  heading only if it would be empty — better to state "nothing waiting on you"
  in one line than to omit silently.
- Bullets and short lines for the items. Tell, don't pad.
- No meta openings ("Here is where we are…"). Start inside the content.
- Present and stop. Do not end with a question or a recommendation — the user makes
  the call.
