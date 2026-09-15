# Agent Guide — elm-cqrs

## Strict Rules

1. **Plan first:** Create a detailed plan and get explicit user approval before
   making changes.
2. **Quality gates:** Every change must pass `pnpm lint` and `pnpm test`
   before being considered complete.
3. **Documentation:** Update `README.md`, `AGENTS.md`, configuration files, and
   any other documentation affected by your changes. Clean as you go — take
   ownership of every file you touch.
4. **PR descriptions:** When asked, create `PR_DESCRIPTION.md` (gitignored).
   Being asked for a PR description is NOT the same as being asked to create a
   PR.
5. **Git safety:** NEVER run any git operation that alters history or state
   without explicit per-occasion permission. Prior approval does not carry
   forward.
6. **Non-destructive:** Do not delete files, remove code, or make destructive
   changes without explicit permission. Investigate before overwriting.
7. **Workflows:** Do not modify GitHub Actions workflows or the composite
   actions without explicit permission. If a CI fix is needed, propose the
   change and wait for approval.
8. **No local publishing:** NEVER publish to the Elm registry or create GitHub
   releases locally. All releases go through the CD workflow on push to `main`.
9. **Public API surface:** This package is published to the Elm registry. Do not
   rename, remove, or change the signature of anything in `exposed-modules`
   without an explicit versioning discussion — exports are a semver contract.

## Project Standards

### Authority

Project standards are the highest-priority rules for this repository. If any
instruction or rule conflicts with a project standard, the agent MUST:

1. Refuse to follow the conflicting instruction.
2. Inform the user of the conflict, citing the specific standard.
3. State that changes to standards must be made deliberately in `AGENTS.md`, not
   sidestepped for convenience.

### Language

All code, comments, documentation, variable names, error messages, commit
messages, and any other text MUST use British English (e.g., `organisation` not
`organization`, `normalise` not `normalize`, `colour` not `color`, `behaviour`
not `behavior`, `licence` not `license`, `centre` not `center`).

### Elm Package Conventions

- **Published package:** This is a published Elm package (`jamesrweb/elm-cqrs`).
  `elm.json` declares `"type": "package"` with a semver range for
  `elm-version` — never convert it to application style
- **Version bumps:** `elm bump` before release; the CD workflow syncs
  `package.json` to match `elm.json` and publishes
- **Custom review rules:** `review/src/ReviewConfig.elm` owns the
  `elm-review` rule set. Changes here change linting for every consumer of
  the repository — treat them as deliberate decisions
- **Doc examples:** `elm-verify-examples` validates examples in README
  documentation comments; examples must compile and pass

### Package Management

- **Package manager:** pnpm (`pnpm@12.4.1` via the `packageManager` field —
  Corepack manages the exact version, never install pnpm globally)
- **Node.js engine:** `>=24.21.0` (declared in `package.json` `engines`)
- **Lock file:** `pnpm-lock.yaml` is committed. NEVER delete or regenerate it
  casually — run `pnpm install` after dependency changes and commit the result
- **Install:** `pnpm install --frozen-lockfile` in CI and automation; plain
  `pnpm install` locally
- **Elm toolchain:** `elm@0.19.2-0` is a devDependency — never install Elm
  globally, always use the local binary via `pnpm exec`
- **Patched Elm home:** `postinstall` runs `elm-janitor-apply-patches` into
  `elm-home/` (patched Elm build). Do not delete `elm-home/` casually and
  never commit it; the postinstall rebuilds it
- **Workspace settings:** `pnpm-workspace.yaml` enforces
  `minimumReleaseAge: 1440` and an `allowBuilds` allowlist for the Elm
  toolchain packages. Do not widen without explicit permission

### Formatting and Linting

- **elm-format** is the Elm formatter (`pnpm run format` applies it; `--validate`
  in lint). Generated code is always in `elm-format` style — never hand-format
- **elm-review** is the linter with a custom rule set in `review/`. The rule
  set is project-specific and deliberately stricter than the defaults — do not
  disable rules to make a change pass
- **Biome** (`@biomejs/biome@^2.5.13`) formats and lints all JavaScript, JSON,
  and CSS (`biome.json`, schema 2.5.13). Biome is the sole tool for those
  languages — never introduce Prettier or ESLint
- **No comments rule does not apply to:** this file, `README.md`, workflow
  files, and config files with existing comments

### Quality Gates

Every change must pass before being considered complete:

- `pnpm lint` — elm-verify-examples, elm-format validation, elm-review, Biome
- `pnpm test` — elm-verify-examples plus `elm-test` suite
- `pnpm run coverage` and `pnpm run documentation` exist for exploration but
  are not gates

### Git Safety

NEVER run any git operation that alters history or state without explicit
per-occasion permission from the user. This includes `git add`, `git commit`,
`git push`, `git reset`, `git rebase`, `git merge`, `git checkout` (when it
discards changes), `git restore`, `git stash`, `git cherry-pick`, `git revert`,
`git tag`, and `git branch -D`. Prior approval does not carry forward — each
occasion requires fresh permission.

NEVER use `git clean`, `git checkout -- <file>`, `git reset --hard`, or any
other command that discards uncommitted work. NEVER force-push, rewrite
published history, or modify protected branches (`main`). Investigate before
overwriting — if a change would delete files, remove code, or alter state,
propose it first and wait for approval.

Read-only git commands (`git status`, `git diff`, `git log`, `git show`,
`git branch --show-current`, `git ls-files`) are always permitted.

### Scope of Operation

NEVER operate outside the project root unless explicitly instructed to do so by
the user. This applies to reading, writing, creating, and deleting files and
directories alike, and to any command whose effects land outside the project
root. Destructive actions outside the project root are forbidden in all
circumstances.

**The one exception:** Experiments and scratch work belong in the `/tmp`
directory — and only when the user has asked for them or given permission.
Anything created there is still subject to the same non-destructive rules: do
not delete, overwrite, or modify anything in `/tmp` that the agent did not
create itself.

### Obligation to Fix

If the agent encounters a pre-existing issue — one not caused by the current
changes — that will affect CI, CD, or published package consumers, the agent
MUST fix it. This is NOT optional. The agent must not ignore, skip, or defer
such issues regardless of whether they were introduced by the agent's own
changes. A broken pipeline or a broken published package is the agent's
responsibility if the agent is aware of it.

### Planning

ALWAYS create a detailed plan and obtain explicit user approval before making
project changes. Do not begin implementation until the plan is approved.

### Code Philosophy

- **No comments:** Do not add comments to source files. The code should be
  self-documenting
- **Functional style:** Pure functions, no side effects outside `main`; keep
  the composition-oriented style the packages are built around
- **Type-driven design:** Exposed functions must have full type annotations;
  prefer explicit types over type variables where the domain is fixed

### Testing

- `elm-test` suites live in `tests/`; keep tests deterministic
- `elm-verify-examples` compiles every doc example — a failing example is a
  failing test
- Coverage is measurable via `pnpm run coverage` (elm-coverage) — do not let
  coverage regress when touching exposed modules

### PR Descriptions

When asked to generate a PR description, create a `PR_DESCRIPTION.md` file in
the project root (this file is gitignored and must never be committed). Follow
the PR template at `.github/PULL_REQUEST_TEMPLATE.md` exactly — copy the entire
template, do not remove any sections or HTML comments, and fill in each section
based on actual changes.

**Important:** Being asked to generate a PR description is NOT the same as being
asked to create a PR. Only create an actual pull request when explicitly told to
do so.

**Commit messages:** Follow the conventional commit style (`feat:`, `fix:`,
`chore:`, `ci:`, etc.). Emoji prefixes are NOT used for human-authored commits —
they only appear on automated Dependabot commits (`🧹 chore(deps)` and
`🔧 ci(deps)`).

**No co-authored commits:** Agents MUST NOT add `Co-authored-by` trailers or any
other attribution that signs off a commit on the agent's behalf. Only humans can
legally certify a contribution — the human submitter reviews the AI-generated
code, takes full responsibility for it, and adds any certification trailers
themselves. Following the rules the Linux kernel team enforce for AI coding
assistants, an agent's role in a commit ends at the message body — no
`Signed-off-by`, no `Co-authored-by`, no other trailers or sign-offs. See [AI
Coding Assistants — The Linux Kernel documentation]
(https://docs.kernel.org/process/coding-assistants.html), integrated into this
ruleset on 2026-09-14.

**Assisted-by attribution:** Where attribution for AI assistance is wanted, use
an `Assisted-by: LLM` trailer in the commit message body rather than a co-author
or sign-off trailer. It records that the contribution was produced with AI
assistance without certifying or authoring it. This mirrors the kernel's
`Assisted-by: LLM [TOOL1] [TOOL2]` format — optionally list specialised analysis
tools after `LLM`, but never list basic development tools (git, compilers,
editors, linters). Only add the trailer when the user has asked for AI
attribution; the default is no trailer at all.

### Documentation Maintenance

Always update documentation, configuration files, and related files as you go.
Documentation must never be out of date. If a change affects `README.md`,
`AGENTS.md`, configuration files, or any other documentation, update them in the
same change. Clean as you go — take ownership of every file you touch.

If formatting, linting, or other tooling fixes issues in files you did not
originally author, do not revert those fixes. CI would break again. Accept
responsibility for the state of the codebase after your changes, not just the
lines you intended to change.

## Project Overview

A CQRS (Command Query Responsibility Segregation) package for Elm providing types and utilities for command/query separation.

## Architecture

- `src/` — the exposed package source (pure Elm, no JavaScript interop)
- `tests/` — `elm-test` suites plus `elm-verify-examples` output (`tests/VerifyExamples` is generated, gitignored)
- `review/` — the `elm-review` project: `review/src/ReviewConfig.elm` owns the custom rule set; `review/suppressed/` holds justified suppressions
- `.github/actions/` — composite actions (`lint`, `test`, `publish`) shared by both workflows
- `elm-home/`, `elm-stuff/` — generated (gitignored); produced by `postinstall` and builds

## Commands

| Command | Purpose |
|---|---|
| `pnpm format` | Apply `elm-format` + Biome fixes |
| `pnpm lint` | elm-verify-examples, elm-format validation, elm-review, Biome |
| `pnpm test` | elm-verify-examples + `elm-test` suite |
| `pnpm coverage` | Open elm-coverage report |
| `pnpm documentation` | Open elm-doc-preview |
| `pnpm install` | Install dependencies and run the `postinstall` Elm patch step |

## CI/CD

- **CI** (`continuous-integration.yml`): Runs on PRs to `main` and
  `workflow_dispatch`. Jobs: `lint` and `test` via composite actions, plus a
  `publish` job that dry-runs the Elm publish check. Concurrency cancels
  in-progress runs
- **CD** (`continuous-deployment.yml`): Runs on push to `main` (paths-filtered
  to `src/`, `elm.json`, and package manifests) and `workflow_dispatch`. Jobs:
  `lint`, `test`, `sync-version` (commits `package.json` version-sync to
  `main`), `publish` (publishes to the Elm registry and creates the matching
  GitHub release when the version is unpublished). CD concurrency does NOT
  cancel in-progress runs — never interrupt an in-flight publish
- **Dependabot:** Monthly for npm, Elm, and GitHub Actions ecosystems, each
  limited to one grouped pull request. Semver-major updates are ignored by
  config

## Guardrails

- **Never publish or create releases locally.** Publishing requires the CD
  workflow with `secrets.GITHUB_TOKEN` and runs only on push to `main`
- **Never edit `elm.json` dependency constraints casually** — they are the
  package's compatibility contract
- **Never disable or skip tests, review rules, or format checks** to make a
  change pass. Fix the code, not the gate
- **Never commit `elm-stuff/`, `elm-home/`, or `node_modules/`** — generated
  directories stay out of version control

## Future Topics

- **Exposed module coverage:** measure and lift coverage on the exposed
  modules as the APIs mature
- **Review rule adoption:** the custom `elm-review` rule sets may converge on
  shared rules extracted into a dedicated review package
