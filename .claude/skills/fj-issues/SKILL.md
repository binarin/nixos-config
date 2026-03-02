---
name: fj-issues
description: Working on project issues stored in forgejo (fj)
---

IMPORTANT: Primary place for storing planning information/questions is
forgejo issue comments. After plan update/create, this should be
immediately reflected in fj issue comment!!!

# Listing open issues

    fj issue search

# Showing issue

    fj issue view ID

# Fetching comments with metadata

Use the wrapper script to get comments with IDs and timestamps:

    .claude/skills/fj-issues/fj-comments.sh <ISSUE_ID>

Returns JSON array with fields: id, body, created_at, updated_at, user.
This provides the comment ID needed for `fj issue edit` and timestamps
to track plan freshness.

Example output:

```json
[
  {
    "id": 42,
    "body": "## Plan...",
    "created_at": "2026-02-21T10:00:00Z",
    "updated_at": "2026-02-21T10:05:00Z",
    "user": "claude-nixos-config"
  }
]
```

The plan comment is always the FIRST comment by `claude-nixos-config`.
Use the `id` field with `fj issue edit` to update it.

# Creating PR

See "Pushing and PRs" section below for the full workflow.

# Adding a comment to an issue

    fj issue comment ID "markdown body"

# Planning phase

Read issue and all comments. Carefully plan the implementation.

## Plan location

The plan is ALWAYS the first comment made by `claude-nixos-config` on the issue.
When starting work on a new issue (no existing claude comments), immediately
reserve a place for the plan:

    fj issue comment <ISSUE_ID> "## Plan\n\n(placeholder)"

This ensures the plan stays at the top of claude's comments. Then update it
with the actual plan content.

## Questions

If you have questions during planning, add them as SEPARATE comments after
the plan comment. Do not put questions in the plan comment itself.

## Updating the plan

If there's already a plan comment but new feedback comments exist after it,
update the plan to incorporate the feedback:

    fj issue edit <ISSUE> comment <IDX> [NEW_BODY]

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
- Get issue info via `fj issue view ISSUE_NUMBER`
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
