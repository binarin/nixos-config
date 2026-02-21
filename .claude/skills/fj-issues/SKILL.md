---
name: fj-issues
description: Working on project issues stored in forgejo (fj)
---

# Listing open issues

    fj issue search

# Showing issue

    fj issue view ID
    fj issue view ID comments

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

As I want to keep the plan near the beginning of the comment list, it
shoud also keep the timestamp of the latest comment with feedback that
was already incorporated, so it'll be possible to check whether the
plan is up-to-date.

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
- create PR (use issued ID in the PR title), so it'll be easy to find
  using `fj pr search` in case if it already exists.
- If there were some valuable learnings in the process of execution,
  add them as an separate issue comment.
