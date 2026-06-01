# User-level Claude Code instructions

Loaded into every Claude Code session as global context.
Per-project CLAUDE.md files in repo roots layer on top of this.

## Hard rules

- **No AI attribution.** Never add `Co-Authored-By: Claude ...` to commits,
  `🤖 Generated with Claude Code` footers to PR bodies, or any other Claude
  branding to authored artifacts. The user's name alone owns the work.
- **Always commit with `--no-gpg-sign`.** Every `git commit` (including
  amend, rebase --exec, etc.) must pass `--no-gpg-sign`. This is a standing,
  pre-authorized override of the default "don't bypass signing" guidance.
- **Don't proactively run destructive git operations** (force push, hard
  reset, branch delete) unless explicitly asked.

## Defaults

- Prefer existing patterns in a repo over introducing new abstractions.
- Don't add comments unless the *why* is non-obvious. Identifiers should
  speak for themselves.
- Don't add error handling for paths that can't happen — trust internal
  code and framework guarantees; validate at system boundaries only.
- For exploratory questions, answer in 2-3 sentences with a recommendation
  and the main tradeoff. Don't implement until I agree.
- `gh` for GitHub work, never the web UI.
- `rg` / `fd` / `bat` over `grep` / `find` / `cat` when available.

## Browser work

When a task involves driving a real browser (scrape, screenshot, fill form,
click through a flow, verify deployed UI), delegate to the `browser-agent`
subagent — it has Playwright MCP wired up.

## Go

For any Go work (new code, refactors, code review), invoke the
`use-modern-go` skill. It detects the project's Go version from `go.mod`
and prefers modern idioms (`slices.Contains`, `cmp.Or`, `max(a, b)`,
`for i := range n`, etc.) over older patterns. Source upstream:
<https://github.com/JetBrains/go-modern-guidelines>.
