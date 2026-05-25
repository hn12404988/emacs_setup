# Integration of Forge with Magit

The goal is to enable Magit Forge so the user can interact with Git hosting services (like GitHub/GitLab) directly from Magit, allowing them to view issues, pull requests, and other metadata.

## User Review Required

> [!IMPORTANT]
> **Authentication Token Setup**:
> Forge requires authentication to retrieve issues and PRs. You must add your token (e.g. GitHub Personal Access Token) to `~/.authinfo` or `~/.authinfo.gpg`.
>
> The recommended line format is:
> ```text
> machine api.github.com login <your-github-username>^forge password <your-personal-access-token>
> ```
> For example:
> `machine api.github.com login john-doe^forge password ghp_12345abcde...`
>
> Note: After adding it, you should run `M-x auth-source-forget-all-cached` in Emacs so it updates the cache.

## Proposed Changes

### Emacs Configuration

#### [MODIFY] [init.el](file:///home/m6/willy/emacs_setup/init.el)

We will add a standard configuration block for `forge` using `use-package`, ensuring it loads after `magit`.

```elisp
;; Forge - integration with GitHub/GitLab issues & PRs
(use-package forge
  :after magit)
```

We will insert this block right after the `magit` configuration block (around line 811, after the magit keys/config block).

## Verification Plan

### Manual Verification
- We can verify that Emacs loads correctly without errors by running Emacs with our changes.
- Once loaded, opening a Git repository buffer with `C-x g` (Magit status) should load Forge, and pressing `@` should show the Forge popup menu.
- We will instruct the user to verify by run-testing their Emacs.
