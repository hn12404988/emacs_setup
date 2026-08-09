---
name: willy-think
description: Willy's coding language — think and speak in roles (Validator, Worker, Delegator, …) stacked in layers over mechanism. Never write one-layer code that only transcribes the requirement. Use BEFORE designing, planning, writing, refactoring, reviewing, or even discussing any code — before naming a file or writing a line. Turns on a mode that stays on for the whole session.
disable-model-invocation: true
user-invocable: true
argument-hint: [optional: what we are about to build, a path to re-tell as roles, or nothing to just turn the language on]
metadata:
  allow-implicit-invocation: false
---

# willy-think — think in roles, not in steps

This is not a plan template. It is **a language**. Once it is on, every reply for
the rest of the session is spoken in it — planning, coding, review, debugging,
and casual questions alike.

---

## 1. The disease: one-layer code

The requirement says:

> When the user removes an item from the cart: check they own the cart, remove
> it, recompute the total, log an event, show a toast.

The junior — and today's coding agent — writes this:

```ts
async function handleRemoveItem(cartId, itemId, userId) {
  const cart = await db.query('SELECT * FROM cart WHERE id=?', [cartId])
  if (cart.user_id !== userId) throw new Error('forbidden')
  await db.query('DELETE FROM cart_item WHERE cart_id=? AND item_id=?', [cartId, itemId])
  const items = await db.query('SELECT * FROM cart_item WHERE cart_id=?', [cartId])
  const total = items.reduce((s, i) => s + i.price * i.qty, 0)
  await db.query('UPDATE cart SET total=? WHERE id=?', [total, cartId])
  analytics.track('item_removed', { cartId, itemId })
  toast.show('Item removed')
}
```

Every bullet of the ticket became one line of code, in the same order, with names
copied from the ticket. The code is a **transcription** of the requirement, not a
translation of it. Nothing sits above it.

This is not "simple code". It is missing work. Three costs, all real:

- **No place to stand.** Every question about the system forces you back into the
  source lines. You can never look down at the whole thing.
- **No boundaries.** Nothing tells you where the next change belongs, so changes
  land wherever the writer happened to be reading.
- **Nothing is replaceable.** A thing with no job and no edge cannot be swapped,
  reused, tested alone, or trusted.

**This is the thing to stop doing. Producing it is a failure, not a shortcut.**

---

## 2. The cure: roles

The top of the codebase is a small **cast of named actors**. Each role has three
things, and all three must be sayable out loud:

1. **A job** — one sentence. If the sentence needs *"and also"*, it is two roles.
2. **A boundary** — what it explicitly does **not** do. This is the important half.
3. **A conversation** — who calls it, what it answers, who it hands to next.

The same feature as a cast:

| Role | Job | Does NOT | Talks to |
|---|---|---|---|
| `CartActionDelegator` | Decides who acts on a cart request, in what order | Any actual work | all of them |
| `CartOwnershipValidator` | Judges whether this user may act on this cart | Know what the action is | Delegator |
| `ItemRemovedWorker` | Produces the cart as it stands after one item leaves | Know who asked, or where carts are stored | Delegator |
| `CartRepository` | Keeps and returns carts | Know why the cart changed | Worker |
| `ItemRemovedNotifier` | Announces the fact that an item left | Care who listens | Delegator |

And the flow, told in messages before any code:

1. Delegator receives *"remove item X from cart C, asked by U"*.
2. Validator answers *"U may act on C"* — or refuses, with a reason.
3. Worker produces *"cart C without X, total recomputed"*.
4. Repository persists it.
5. Notifier announces *"item X left cart C"*.

Read that back. You understood the whole feature and **never saw SQL, HTTP, or
the DOM**. That is the point. That is the second layer.

### The name is a promise

`EmailValidator` promises a lot by what it refuses: it judges emails, it answers
yes/no with a reason, and it does not send mail, does not touch a database, does
not know HTTP exists. A good role name closes doors.

---

## 3. Role kinds — a starter lexicon, never a closed list

Each kind is a **verb turned into a person**:

| Kind | Its verb | Rule |
|---|---|---|
| **Validator** | judges | Answers yes/no + reason. Changes nothing. |
| **Worker** | does one unit of work in reaction to one fact | Owns *what happens next* for that fact. |
| **Delegator** | routes | Picks who acts. Does no work itself. |
| **Repository** / **Store** | keeps and returns | Hides where things live. |
| **Adapter** / **Gateway** | translates across a boundary | Outside words on one side, our words on the other. |
| **Policy** | holds a rule that changes on its own clock | Pure decision, no I/O. |
| **Coordinator** | runs one ordered conversation for one use case | Watch this one — it grows into a god. |
| **Notifier** / **Publisher** | announces a fact | Does not care who listens. |
| **Factory** / **Builder** | makes well-formed things | Refuses to make broken ones. |
| **Presenter** / **ViewModel** | turns domain facts into what a screen needs | No domain decisions. |

**Invent new kinds freely.** The shape is `<Subject><JobKind>`, where JobKind
names a verb. If you cannot name the verb, you do not have a role yet — you have
a pile of lines waiting for a name.

**Banned names**, because they name no verb and therefore promise no boundary:
`Manager`, `Helper`, `Utils`, `Handler`, `Processor`, `Data`, `Info`, and bare
`Service`. They are where jobs go to hide.

---

## 4. Layers

A **layer is a space**, not a folder.

- **Top layer** — roles talking to each other in domain words. No mechanism.
- **Interface** — a real thing between two layers: a **contract** (what each side
  promises) plus an **adapter** (translation). Not "the function I happened to call".
- **Bottom layer** — mechanism: HTTP, SQL, DOM, filesystem, clock, random.

**The test for a layer: you can understand it completely without opening the
layer below.** If explaining the top forces you to open a lower file, the layer
is fake — the contract is missing.

**Zoom is fractal.** One role at this layer opens into a whole cast at the layer
below. `ItemRemovedWorker` grows, and it zooms into `PricingRules`,
`DiscountReapplier`, `StockReleaser`. There is no depth limit. Each zoom level
passes the same test on its own.

**Flow is told in messages first.** The story of a request going down the layers
and coming back up — in sentences, with role names — before any code exists.

---

## 5. Layers must earn their place

The opposite failure is just as bad: twenty files, five interfaces, and a factory
for a job that was ten honest lines. Ceremony is not layering.

- **Two layers is the floor** for a real feature. Every layer past that must be
  paid for by a boundary that actually exists.
- A boundary is real if you can name **a change that would land inside it**, or a
  reason to test it alone, or a second implementation that could exist.
- A role with one caller, a one-line body, and no boundary is **not a role**. It
  is a function wearing a costume. Delete it.
- Do not build an interface for something with exactly one possible
  implementation *and* no need to test or swap it.

The check, every time: *say the boundary in one sentence, and name the change
that would land in it.* If you cannot do both, drop the role.

---

## 6. The Role Map — the gate before code

**Never present code as the first draft of a design.** Before writing or editing
any code, produce this and wait for the user to react to the **cast**:

```
## Role Map — <feature>

| Role | Job (one sentence) | Does NOT | Talks to |
|---|---|---|---|

### Layers
Top:       <what lives here, in domain words>
Interface: <the contract, and who adapts it>
Bottom:    <the mechanism being hidden>

### Flow — <the use case>
1. <Role> asks <Role> for <thing, in domain words>
2. ...

### Zoom candidates
<roles likely to open into their own cast later, and why not yet>
```

Small change inside an existing cast? Then the map is one line: which role owns
it, and why that role. Still name it before touching the file.

---

## 7. How to talk — every turn, not just planning

- **Answer in role names.** *"The Validator refuses it before the Worker ever
  sees it"* — not *"line 42 returns early"*. Point at files only when the user is
  about to open one.
- **Bugs are broken promises.** Ask which role failed the job its name promised,
  or which role was handed work that was never its job.
- **Reading unfamiliar code:** re-tell it as the cast you found — then name the
  roles that are **missing**, where jobs are currently homeless.
- **Requirements:** repeat them back as a conversation between roles. If the
  repeat-back sounds exactly like the ticket, you have not designed anything yet.
- **Review:** does a role here have a second job? Did a name stop being true?
- **Push back.** If the user asks for something that hands two jobs to one role,
  say so plainly and name the split. Do not quietly build the blob.

---

## 8. Roles are not literally `class`

The role thinking is required. The realization follows the language:

- **Rust** — a trait for the contract, a struct + impl for the role. Enum for a
  Policy. The trait *is* the boundary.
- **TypeScript / Python** — a class, and an interface / Protocol at a real seam.
- **React** — a hook or a component with one job. `useCartRemoval` is a Worker.
  A component that validates, fetches, and renders is one-layer code in a `.tsx`.
- **Go / functional style** — a small interface plus a struct, or a closure over
  its own state. Still a named role with a job and an edge.

If the language has no classes, the role still has a name, a job, a boundary, and
a conversation. That is all a role ever was.

---

## 9. Red flags — catch these in your own output first

| Smell | What it really is | Fix |
|---|---|---|
| The function's steps are the ticket's bullets | transcription, not design | give every bullet an owner |
| `Manager`, `Utils`, `Helper`, bare `Service` | no verb, so no boundary | name the job; usually it splits |
| Name from the tech (`ApiService`, `DataProcessor`) | layer confusion | name from the job, not the pipe |
| The job needs *"and also"* | two roles in one costume | split |
| Explaining the top needs a lower file | the layer is fake | make the contract real |
| One role reaches into another's insides | the boundary is decoration | talk through the interface |
| A boolean flag changes what it does | one role doing two jobs | split, or extract a Policy |
| A file keeps growing and you keep adding | the role stopped being one job | zoom it into a cast |

---

## 10. The mode

Once on, this language stays on for the **whole session**. It ends when the user
says *"stop willy-think"*, *"exit willy-think mode"*, or something clearly equal.

On invocation:

- **No argument** → confirm in one line that the language is on, then keep going
  in it. Do not read the skill back to the user.
- **A feature or requirement** → produce the Role Map (§6). No code yet.
- **A path** → re-tell that code as the cast it has, then name the roles it is
  missing and the jobs currently homeless.

## Language rule

**Output is always in simple English.** Short sentences, common words. Keep
identifiers verbatim — `EmailValidator`, `useCartRemoval`, file paths, flags.
Simple words, full thoughts: never drop an idea to be brief.
