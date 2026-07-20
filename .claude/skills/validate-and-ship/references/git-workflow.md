# Committing and opening a PR after validation passes

This part isn't scripted on purpose. Staging, branching, and PR base
selection all depend on the actual state of each repo at the time, and
guessing wrong here is expensive to undo (mixed-up commits, a PR opened
against the wrong base, or worse, someone else's in-progress work getting
swept into a commit that isn't theirs). Go through this by hand, per repo.

## 1. Check for work that isn't yours before touching anything

Run `git status` in each repo *before* staging anything. Look past your own
changed files for anything you don't recognize: modified files you never
touched, untracked directories, a branch name that doesn't match what you
were doing. This actually happened during this skill's own development —
one of the two repos had ~18 modified files and a few untracked ones
sitting uncommitted on the current branch, entirely unrelated, presumably a
colleague's in-progress fix.

If you see anything like that: stop and ask the user what it is and how
they want it handled, rather than guessing. Do not run `git add -A` or
`git add .` — always stage the specific files you know you changed, by
name. A broad add is exactly how someone else's work-in-progress ends up
inside your commit.

If the unrelated work is sitting on the same branch you'd otherwise want to
commit to, you have options short of disturbing it:
- Commit just your files directly on that branch, leaving the rest
  uncommitted and untouched — safe, but means your commit's history lives
  on a branch whose name/purpose doesn't match what you did.
- Cherry-pick your commit onto a fresh branch afterward for a clean PR,
  without ever pushing to the branch the unrelated work lives on. This is
  usually the better choice if that branch has its own remote and its own
  purpose — you don't want to be the one pushing changes to a branch you
  don't own the context for.

## 2. Figure out the right base branch — don't assume master/main

Before creating a branch, check:

```bash
gh pr list --state open
git log --oneline -1 origin/master   # or origin/main
git log --oneline -1 origin/dev      # if a dev/integration branch exists
```

If the feature you're extending is itself still an open, unmerged PR (this
happened during this skill's own development — the base MCP server was
PR #29, not yet merged, and the new work depended on it directly), branch
from that PR's branch instead of master. Opening a PR against master in
that situation would show a diff full of someone else's unreviewed work as
if it were yours.

If there's a `dev` or similar integration branch that's clearly where
active work lands (check what the most recent merges target), prefer that
over a stale `master`/`main`.

## 3. Move your changes to a clean branch based on the right point

```bash
git stash -u -m "description"      # if you have uncommitted changes to move
git fetch origin <base-branch>
git checkout -b <new-branch-name> origin/<base-branch>
git stash pop
```

Watch for merge conflicts here if the base you land on doesn't have files
your stash touches — that's usually a sign you picked the wrong base (see
step 2), not a real conflict to resolve.

## 4. Commit — explain why, not just what

Follow whatever commit message convention the repo's own recent log
already uses (check `git log --oneline -10` first). Focus the message body
on *why* the change exists — a future reader can already see *what*
changed in the diff. If a fix or design decision came from something
non-obvious discovered during testing (a bug the validation run surfaced,
a constraint learned the hard way), say so — that context is exactly what
disappears if you don't write it down.

## 5. Push and open the PR

```bash
git push -u origin <new-branch-name>
gh pr create --base <base-branch> --title "..." --body "..."
```

In the PR body, mention:
- What this depends on, if it's based on another open PR (link it).
- What was actually verified, and how — "ran the validation script,
  N pass / M fail (expected) / K skip" is far more useful to a reviewer
  than "tests pass."
- Any bug fixed along the way that wasn't the original ask, and why it
  mattered (reviewers appreciate knowing it wasn't scope creep, it was a
  blocker you hit).
