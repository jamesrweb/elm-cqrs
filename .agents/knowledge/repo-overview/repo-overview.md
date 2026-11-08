---
kind: note
id: repo-overview
title: Repo overview - elm-cqrs
context: auto
summary: What the elm-cqrs package is, how it is laid out, how to run its gates, and how CI and CD release it.
tags: overview, architecture, elm, ci-cd
---

# Repo overview - elm-cqrs

## What this is

A CQRS (Command Query Responsibility Segregation) package for Elm providing
types and utilities for command/query separation. The package is published to
the Elm package registry as `jamesrweb/elm-cqrs`.

## Architecture

- `src/` - the exposed package source (pure Elm, no JavaScript interop)
- `tests/` - `elm-test` suites plus `elm-verify-examples` output
  (`tests/VerifyExamples` is generated and gitignored)
- `review/` - the `elm-review` project. `review/src/ReviewConfig.elm` owns the
  custom rule set. `review/suppressed/` holds justified suppressions
- `.github/actions/` - composite actions (`lint`, `test`, `publish`) shared by
  both workflows
- `elm-home/`, `elm-stuff/` - generated (gitignored), produced by `postinstall`
  and builds

## Commands

| Command              | Purpose                                                         |
| -------------------- | --------------------------------------------------------------- |
| `pnpm format`        | Apply `elm-format` and `Biome` fixes                            |
| `pnpm lint`          | elm-verify-examples, elm-format validation, elm-review, `Biome` |
| `pnpm test`          | elm-verify-examples plus the `elm-test` suite                   |
| `pnpm coverage`      | Open the elm-coverage report (not a gate)                       |
| `pnpm documentation` | Open elm-doc-preview (not a gate)                               |
| `pnpm install`       | Install dependencies and run the `postinstall` Elm patch step   |

## CI/CD

- CI (`continuous-integration.yml`) runs on pushes to `main`, on all pull
  requests, and on manual dispatch. Its jobs are `lint` and `test` via the
  composite actions, plus a `publish` job that dry-runs the Elm publish check.
  Concurrency cancels in-progress runs.
- CD (`continuous-delivery.yml`) runs on pushes to `main`, paths-filtered to
  `src/`, `elm.json`, and the package manifests, and on manual dispatch. Its
  jobs are `lint`, `test`, `sync-version` (commits the `package.json` version
  sync to `main`), and `publish` (publishes to the Elm registry and creates the
  matching GitHub release when the version is unpublished). CD concurrency does
  not cancel in-progress runs, so never interrupt an in-flight publish.
- `Dependabot` runs monthly for the npm, Elm, and GitHub Actions ecosystems,
  each limited to one grouped pull request, with semver-major updates ignored.

## Release model

The package version lives in `elm.json`. Bumping is done with `elm bump` before
release. CD syncs `package.json` to match and publishes. Never publish or create
releases locally.
