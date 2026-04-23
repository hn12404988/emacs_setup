---
name: source
description: Ground a topic, term, or name the user is confused about in concrete source locations. Given a topic as argument, search the current project (or a user-specified directory) for where it is defined and used, and report back with exact `file:line` references. Definition first, then usages. Output is always in Traditional Chinese (繁體中文). Not for interpreting session/discussion content — this skill targets files on disk.
---

# /source — Ground a Topic in Concrete Source Locations

## Purpose

The user invokes this skill when a topic feels like a floating label — a term, name, or concept they cannot map to real code. The job of `/source` is to turn that abstract label into concrete anchors: exact `file:line` locations where the topic is **defined**, and then where it is **used**.

This is not a summary of a topic and not an explanation from general knowledge. It is a grounded pointer into the filesystem. Every claim must be backed by a real location that was actually found on disk.

## Input

The skill takes one argument: `$ARGUMENTS`.

- Primary form: a single topic/term/name (e.g. `useAuthContext`, `DERP relay`, `merge freeze`).
- The user may also specify a directory to search in, either inline (`useAuthContext in /path/to/other/project`) or in surrounding prose. If so, search that directory instead of the current working directory.
- If no directory is specified, default to the current working directory.

## Language Rule (Hard Requirement)

**Output is always in Traditional Chinese (繁體中文)**, regardless of the source language or identifier language.

- Keep identifiers, file paths, commands, and code verbatim (e.g. `useState`, `/src/auth/context.ts:42`, `cargo build`).
- Translate general technical terms when there is a well-established Chinese rendering (definition → 定義, usage → 使用處, not found → 找不到). Otherwise keep the original term.
- Quoted source text may stay in its original language, but the surrounding explanation is in Traditional Chinese.

Note: this `SKILL.md` is in English because it is instructions for the assistant. Only the *output shown to the user* is in Traditional Chinese.

## Search Scope

- Default scope: the current working directory, recursively.
- Override scope: if the user names a different directory in `$ARGUMENTS` or the recent message, use that directory instead.
- Do not search arbitrary paths outside the intended scope. Do not search the web or general knowledge. The answer must come from files on disk.
- Respect the directory's own ignore rules (e.g. `.gitignore`) when using search tools that support them.

## Search Strategy — Definition First, Then Usages

1. **Interpret the topic.** Consider likely forms: exact identifier, camelCase/snake_case variants, quoted string, constant name, filename, config key, CLI flag, etc. Pick the most plausible forms but do not invent extra variants beyond what's reasonable.
2. **Find the definition site(s) first.** Use `grep`/`rg` and file reads to locate where the topic is declared — function/class/struct/const/type declarations, schema entries, config keys, route registrations. Read enough of the surrounding code to understand what it is.
3. **Then find usage/reference sites.** Call sites, imports, test cases, config references, docs/comments mentioning it.
4. **Then related anchors if useful.** Tests that exercise it, config files that wire it up, docs that describe it.
5. **Do not over-collect.** If there are many usage sites, pick a representative handful (typically 3–8) that best illustrate how the topic is used, and note the total count. Do not dump every match.

## When the Topic Cannot Be Found

If no file in scope contains a credible match for the topic, do not guess and do not fall back to general knowledge. Respond with a short message in Traditional Chinese along these lines, then stop:

> 找不到此主題「`<topic>`」於目前搜尋範圍（`<scope path>`）內的任何檔案。請再補充說明，例如：大致出現在哪個模組、相關的檔案或套件名稱、或你第一次看到這個詞的上下文。

Also note briefly what search forms were tried (e.g. exact string, camelCase, snake_case), so the user can see where to redirect.

If there are only *weak/ambiguous* matches (e.g. the term appears only in a comment, or only in a changelog), report them honestly as weak matches rather than promoting them to a definition.

## Output Structure

Output must be structured. Use the layered form below by default; if the result is small (one definition, few usages), a flat bulleted form is acceptable. In both cases, every cited location uses the `path:line` format.

### Layered form (default)

#### 一句話定位
One sentence in Traditional Chinese: what this topic *is* in this codebase, based on what was found. Not a dictionary definition — a grounded one.

#### 定義
The definition site(s). For each:
- `relative/path/to/file.ext:LINE` — one short line describing what is declared there (kind: function / class / type / const / config key / route / etc.).
- If more than one definition exists (e.g. trait + impl, interface + implementation, multiple overloads), list each.

#### 使用處
Representative usage sites. For each:
- `relative/path/to/file.ext:LINE` — one short line describing how it is used there.
- If matches are numerous, list a representative subset and state the total count (e.g. 「共 42 處，以下列出 6 個代表性位置」).

#### 相關位置（選填）
Only include if genuinely helpful: tests, config, docs, related types. Same `path:line` + one-line format. Omit the section if there is nothing worth adding.

### Flat bulleted form (for small results)

A single bulleted list where each bullet is `path:line — 一句說明`, grouped implicitly by putting the definition first, then usages. Only use this when the full layered form would feel over-engineered for the size of the result.

## Style

- Every cited location must be a real `path:line` the assistant actually read or matched. Never fabricate line numbers.
- Prefer paths relative to the search scope root, not absolute paths, unless the user searched an absolute path they specified.
- Preserve identifiers, filenames, and code fragments verbatim.
- No meta openings like 「以下是搜尋結果…」. Go straight into the structure.
- No personal opinions, recommendations, or speculation about code quality.
- Keep each per-location line short — one sentence. Push deeper explanation into the one-sentence-定位 at the top, not into per-location bullets.
- No emoji unless the source uses them.
