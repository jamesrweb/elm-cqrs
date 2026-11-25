# ELM CQRS

An Elm library for
[CQRS](https://en.wikipedia.org/wiki/Command_Query_Responsibility_Segregation)
(Command Query Responsibility Segregation). It gives you typed commands and
queries whose success or error response variants are decoded automatically from
an API payload, so your HTTP layer stays thin. Use it when you want
command/query separation enforced at the type level.

## Features

- `Cqrs.Command` for instructions that return no value, success or not
- `Cqrs.Query` for requests that return a value or an error
- Responses decode from a payload with an `error` key (failure) or a `data` key
  (success). Commands ignore `data`
- Pure Elm, no JavaScript interop

## Installation

```sh
elm install jamesrweb/elm-cqrs
```

## Usage

Build a command or query with `Cqrs.Command` / `Cqrs.Query`, pointing at your
API:

```elm
import Cqrs.Command as Command
import Cqrs.Query as Query
```

A command sends a request and decodes only success or failure. A query also
decodes the returned data. A typical command looks like this:

```elm
addUserCommand : User -> Cmd Msg
addUserCommand user =
    Command.request
        { url = "/api/v1/user"
        , body = User.encode user
        , toMsg = UserCommandExecuted
        , toError = Error.decoder
        , defaultError = Error.Unknown
        }
```

Point `toError` at your error decoder and `defaultError` at a fallback.

It is important that your API responds in the following pseudocode format for
both commands and queries:

```elm
type alias ApiSuccess a =
    { data : a
    }

type alias ApiError e =
    { error : e
    }
```

1. If an `error` is present, the failure variant for `Cqrs.Command.Response` or
   `Cqrs.Query.Response` will be decoded.
2. If the `data` key is present, the success variant for the given
   `Cqrs.Query.Response` will be decoded.
3. If the `data` key is present in a `Cqrs.Command.Response`, it will be
   discarded since commands, by definition, do not return data.

Please see the tests for more examples of how the decoders handle the different
cases.

### Commands

A command is an instruction which returns no value in response. It is simply
successful or not.

To send a command, you can do the following:

```elm
import Cqrs.Command as Command exposing (CommandResponse)
import User exposing (User)
import Api.Error as Error exposing (Error)

type alias UserCommandResponse =
    CommandResponse Error

type Msg =
    UserCommandExecuted UserCommandResponse

addUserCommand : User -> Cmd Msg
addUserCommand user =
    let
        body : Json.Encode.Value
        body =
            User.encode user
    in
    Command.request {
        url = "/api/v1/user"
        , body =  body
        , toMsg  = UserCommandExecuted
        , toError = Error.decoder
        , defaultError = Error.Unknown
    }

addUserCommandTask : User -> Task () UserCommandResponse
addUserCommandTask user =
    let
        body : Json.Encode.Value
        body =
            User.encode user
    in
    Command.requestTask {
        url = "/api/v1/user"
        , body =  body
        , toError = Error.decoder
        , defaultError = Error.Unknown
    }
```

### Queries

A query is a request to look up a value or series of values in response. It will
either return the desired values, or an appropriate error.

To execute a query, you can do the following:

```elm
import Cqrs.Query as Query exposing (QueryResponse)
import UUID exposing (UUID)
import Models.User exposing (User)
import Api.Error as Error exposing (Error)

type alias UserQueryResponse =
    QueryResponse Error User

type Msg =
    UserQueryExecuted UserQueryResponse

findUserQuery : UUID -> Cmd Msg
findUserQuery id =
    let
        path : String
        path = "/api/v1/user/" ++ UUID.toString id
    in
    Query.request {
        url = path
        , toMsg = UserQueryExecuted
        , toData = User.decoder
        , toError = Error.decoder
        , defaultError = Error.Unknown
    }

findUserQueryTask : UUID -> Task () UserQueryResponse
findUserQueryTask id =
    let
        path : String
        path = "/api/v1/user/" ++ UUID.toString id
    in
    Query.requestTask {
        url = path
        , toData = User.decoder
        , toError = Error.decoder
        , defaultError = Error.Unknown
    }
```


## Repository layout

```
./
├── .agents/            # agent configuration (DotAgents protocol)
├── .github/            # CI and CD workflows plus shared composite actions
├── .idea/              # JetBrains IDE run configurations (tracked)
├── review/             # elm-review project with the custom rule set
├── src/                # exposed package source (Cqrs, Cqrs.Command, Cqrs.Query)
├── tests/              # elm-test suites including the decoder examples
├── .gitattributes      # git attributes
├── .gitignore          # git ignore rules
├── AGENTS.md           # agent-facing documentation
├── biome.json          # Biome config
├── CLAUDE.md           # symlink to AGENTS.md
├── elm.json            # Elm manifest
├── LICENSE             # BSD 3-Clause licence
├── package.json        # pnpm manifest and scripts
├── pnpm-lock.yaml      # pnpm lockfile
├── pnpm-workspace.yaml # pnpm workspace settings
└── README.md           # this file
```

## Development

Install dependencies, then run the gates:

| Command              | Description                                           |
| -------------------- | ----------------------------------------------------- |
| `pnpm format`        | Applies `elm-format` and `Biome` fixes                |
| `pnpm lint`          | Runs the lint checks (the gate)                       |
| `pnpm test`          | Runs the `elm-test` suite and doc examples (the gate) |
| `pnpm coverage`      | Opens the coverage report                             |
| `pnpm documentation` | Opens the module documentation preview                |

The usual flow is to edit, run `pnpm format`, and then `pnpm test`. Every change
must pass `pnpm lint` and `pnpm test`. CI runs these checks automatically.

## Contributing

This is a personal published package. Issues and suggestions are welcome on the
GitHub issue tracker. I make all changes myself.

## Documentation

Module documentation with examples lives in `src/` and is validated by the test
suite. Agent-facing docs for this repo live in `AGENTS.md` and `.agents/`.

## Licence

Licensed under the
[BSD 3-Clause Licence](https://github.com/jamesrweb/elm-cqrs/blob/main/LICENSE).
