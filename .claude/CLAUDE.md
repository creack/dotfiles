# User-level Claude Code instructions

# Loaded into every Claude Code session as global context.
# Per-project CLAUDE.md files (in repo roots) layer on top of this.

## About me

<!--
Short bio that helps Claude tailor its responses. Examples:
- Role (founder, IC, etc.)
- Languages / stacks you work in primarily
- What you're typically doing in a session (debugging, writing greenfield, reviewing PRs)
-->

## Style preferences

<!--
Communication style. Examples:
- Be terse; skip preambles and summaries.
- Don't apologize or hedge — just say what you found.
- When uncertain, say "not sure" instead of guessing.
- No emojis unless I add them first.
-->

## Code preferences

<!--
Examples:
- Prefer existing patterns in the repo over introducing new abstractions.
- No comments unless the "why" is non-obvious.
- Don't add error handling for paths that can't happen.
- Stick to the smallest diff that fixes the bug.
-->

## Tools

<!--
Defaults you want Claude to assume. Examples:
- Use `gh` for GitHub work, never the web UI.
- `rg` over `grep`, `fd` over `find`.
- Always lint with golangci-lint before claiming Go work is done.
-->

## Don'ts

<!--
Hard rules. Examples:
- Never commit without explicit "commit this" from me.
- Never force-push to main.
- Don't run migrations.
-->
