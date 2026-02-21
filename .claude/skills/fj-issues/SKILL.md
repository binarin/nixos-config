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

To identify the plan comment, look for body starting with "## Plan".
Use the `id` field with `fj issue edit` to update specific comments.

# Creating PR

    fj pr create --base master --head issue-29-git-credentials-symlink --body "markdown body" "title"

# Adding a comment to an issue

    fj issue comment ID "markdown body"

# Planning phase

Read issue and all comments. Carefully plan the implementation, and
add the plan as a comment to the issue.

There can already be a plan here, if there is no new comments after
the latest plan - do nothing.

Otherwise update plan with the new inputs, update the plan comment using

    fj issue edit <ISSUE> comment <IDX> [NEW_BODY]

IMPORTANT: Check that you are update the plan comment, and not any of
the feedback comments!!!!!!

As I want to keep the plan near the beginning of the comment list, it
shoud also keep the timestamp of the latest comment with feedback that
was already incorporated, so it'll be possible to check whether the
plan is up-to-date.

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
  short name infejrred from the issue description,
  i.e. `issue-28-pkgs-system-deprecation`
- Stash any local changes
- Switch to / create the issue branch
- Work on the issue, creating logically separate commits. Don't
  rewrite commit history, use fixup commits.
- `nix fmt` should be always run before commit.
- create PR (use issued ID in the PR title), so it'll be easy to find
  using `fj pr search` in case if it already exists.
- If there were some valuable learnings in the process of execution,
  add them as an separate issue comment.

# Passwords

- If you need to add some sops secrets, don't bother getting the real
  values - just stick random placeholders there.
