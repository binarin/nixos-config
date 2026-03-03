---
name: fj-issues
description: Working on project issues and pull requests in forgejo. Use for "fj issue", "fj pr", "fj-pr", PR creation, and issue management.
---

IMPORTANT: Primary place for storing planning information/questions is
forgejo issue comments. After plan update/create, this should be
immediately reflected in fj issue comment!!!

# Repository context

IMPORTANT: The `fj` CLI determines repo context from the current branch's
upstream tracking. When working on branches that track `claude-staging`
(the fork), you must specify the upstream repo explicitly for issues.

Always use the full repo reference: `binarin/nixos-config#<ID>`

# Listing open issues

    fj -H forgejo.lynx-lizard.ts.net issue search binarin/nixos-config

Or just use:

    fj issue search

(defaults to current repo context, but issues are in binarin/nixos-config)

# Showing issue

    fj -H forgejo.lynx-lizard.ts.net issue view binarin/nixos-config#ID

# Fetching comments with metadata

Use the wrapper script to get comments with IDs and timestamps:

    .claude/skills/fj-issues/fj-comments.sh <ISSUE_ID>

Returns JSON array with fields: index, id, body, created_at, updated_at, user.
The `index` field is what you use with `fj issue edit ... comment <IDX>`.

Example output:

```json
[
  {
    "index": 0,
    "id": 42,
    "body": "## Plan...",
    "created_at": "2026-02-21T10:00:00Z",
    "updated_at": "2026-02-21T10:05:00Z",
    "user": "claude-nixos-config"
  }
]
```

The plan comment is always the FIRST comment by `claude-nixos-config` (index 0).

**IMPORTANT:** The `fj issue edit` command uses the `index` field (0-based),
NOT the `id` field. The `id` is the API's internal ID (useful for tracking),
but for editing use the `index`.

# Creating PR

See "Pushing and PRs" section below for the full workflow.

# Adding a comment to an issue

    fj -H forgejo.lynx-lizard.ts.net issue comment binarin/nixos-config#ID "markdown body"

# Planning phase

Read issue and all comments. Carefully plan the implementation.

## Git baseline for investigation

Before investigating the codebase during planning, ensure you're on the latest
master from origin (unless the issue specifies a different ref/branch):

    git fetch origin
    git checkout origin/master

This ensures the plan is based on the current state of the codebase, not an
outdated or feature branch state.

## Plan location

The plan is ALWAYS the first comment made by `claude-nixos-config` on the issue.
When starting work on a new issue (no existing claude comments), immediately
reserve a place for the plan:

    fj -H forgejo.lynx-lizard.ts.net issue comment binarin/nixos-config#<ISSUE_ID> "## Plan\n\n(placeholder)"

This ensures the plan stays at the top of claude's comments. Then update it
with the actual plan content.

## Questions

If you have questions during planning, add them as SEPARATE comments after
the plan comment. Do not put questions in the plan comment itself.

## Updating the plan

If there's already a plan comment but new feedback comments exist after it,
update the plan to incorporate the feedback:

    fj -H forgejo.lynx-lizard.ts.net issue edit binarin/nixos-config#<ISSUE_ID> comment <IDX> "NEW_BODY"

Where `<IDX>` is the 0-based index of the comment (use `0` for the first
comment, which should be the plan).

IMPORTANT: Only edit the plan comment (first claude-nixos-config comment),
never edit feedback comments!

The plan should include the timestamp of the latest feedback comment that
was incorporated, so it's easy to check if the plan is up-to-date.

## Plan scope

The plan scope is only the implementation, no deployments at all - if
you can't test something just from the confines of repository, skip
including it from plan altogether. The ultimate checks are `eval-all`
and `build-all`.

# Execute phase

Only check that plan is up-to-date (as described in 'Planning phase'),
stop if it is not.

When being asked to work on a separate issue that exists in forgejo,
you do it in the separate branch with predictable name.

Otherwise:

- Fetch the latest git changes `git fetch --all`
- Get issue info via `fj -H forgejo.lynx-lizard.ts.net issue view binarin/nixos-config#ISSUE_NUMBER`
- Check whether the branch starting `issue-ISSUE_NUMBER` already
  exists in origin, get the branch name from there.
- If not, invent a branch name that starts with issue number and contains some
  short name inferred from the issue description,
  i.e. `issue-28-pkgs-system-deprecation`
- Stash any local changes
- Switch to / create the issue branch
- Work on the issue, creating logically separate commits. Don't
  rewrite commit history, use fixup commits.
- `nix fmt` should be always run before commit.
- Push and create PR as described in "Pushing and PRs" section.
- If there were some valuable learnings in the process of execution,
  add them as an separate issue comment.

# Pushing and PRs

Claude agent does not have write access to `origin` remote. Instead, push
to `claude-staging` remote (fork repository at `claude-nixos-config/nixos-config`).

## Pushing changes

Push the branch to `claude-staging`:

    git push claude-staging <branch-name>

Or to update an existing PR branch:

    git push claude-staging <local-branch>:<remote-branch>

## Creating cross-fork PRs

When creating PRs, use `-r` for the target repo and `--head` with the fork prefix:

    fj pr create -r forgejo.lynx-lizard.ts.net/binarin/nixos-config \
        --base master --head claude-nixos-config:<branch-name> \
        --body "..." "PR title"

The `-r` specifies the target repository (with full host), and `--head` must include
the fork owner prefix (`claude-nixos-config:`) to indicate the source fork.

## Updating existing PRs

To update an existing PR, just push new commits to the same branch on `claude-staging`:

    git push claude-staging <branch-name>

The PR will automatically update with the new commits.

# Passwords

- If you need to add some sops secrets, don't bother getting the real
  values - just stick random placeholders there.
