# ELM CQRS

A [CQRS](https://en.wikipedia.org/wiki/Command_Query_Responsibility_Segregation) library to support separation of commands and queries on the type level.

## Installation

```sh
elm install jamesrweb/elm-cqrs
```

## Usage

You interact with this library via the `Cqrs.Command` or `Cqrs.Query` module.

It is important that your API responds in the following pseudocode format for both commands and queries:

```elm
type alias ApiSuccess a =
    { data : a
    }

type alias ApiError e =
    { error : e
    }
```

1. If an `error` is present, the failure variant for `Cqrs.Command.Response` or `Cqrs.Query.Response` will be decoded.
2. If the `data` key is present, the success variant for the given `Cqrs.Query.Response` will be decoded.
3. If the `data` key is present in a `Cqrs.Command.Response`, it will be discarded since commands, by definition, do not return data.

Please see the tests for more examples of how the decoders handle the different cases.

### Commands

A command is an instruction which returns no value in response. It is simply successful or not.

To send a command, you can do the following:

```elm
import Cqrs.Command as Command exposing (CommandResponse)
import User exposing (User) -- An example module
import Api.Error as Error exposing (Error) -- An example module

type alias UserCommandResponse =
    CommandResponse Error ()

type Msg =
    UserCommandExecuted UserCommandResponse

addUserCommand : User -> Cmd Msg
addUserCommand user =
    let
        body : Json.Encode.Value
        body =
            User.encode user
    in
    Command.request "/api/v1/user" Error.Http Error.Unknown body Error.decoder UserCommandExecuted

addUserCommandTask : User -> Task () UserCommandResponse
addUserCommandTask user =
    let
        body : Json.Encode.Value
        body =
            User.encode user
    in
    Command.requestTask "/api/v1/user" Error.Http Error.Unknown body Error.decoder
```

### Queries

A query is a request to look up a value or series of values in response. It will either return the desired values, or an appropriate error.

To execute a query, you can do the following:

```elm
import Cqrs.Query as Query exposing (QueryResponse)
import UUID exposing (UUID) -- elm install TSFoster/elm-uuid (for example)
import Models.User exposing (User) -- An example module
import Api.Error as Error exposing (Error) -- An example module

type alias UserQueryResponse =
    QueryResponse Error User

type Msg =
    UserQueryExecuted UserQueryResponse

findUserQuery : Uuid -> Cmd Msg
findUserQuery id =
    let
        path : String
        path = "/api/v1/user/" ++ Uuid.toString id
    in
    Query.request path Error.Http Error.Unknown UserQueryExecuted User.decoder Error.decoder

findUserQueryTask : Uuid -> Task () UserQueryResponse
findUserQueryTask id =
    let
        path : String
        path = "/api/v1/user/" ++ Uuid.toString id
    in
    Query.requestTask path Error.Http Error.Unknown User.decoder Error.decoder
```

### Error handlers and defaults

In both cases, `Cqrs.Request.command` and `Cqrs.Request.query`, you can pass an optional http error handler in case you need custom errors or logging to be done, for example.

To create a custom http error handler, you need to implement a function which takes an error of type `e` and returns a mapped value of type `f` which is probably some domain value but could be anything, for example being given an `Http.Error` and returns an `ApiError` domain error. For example:

```elm
module Api.Error exposing (Error)

import Http -- elm install elm/http

type Error =
    Unknown
    | Http Http.Error

errorMapper : Http.Error -> Error
errorMapper error =
    Http error

defaultError : Error
defaultError =
    Unknown
```

And then to use it in your requests, you pass it in the second parameter to either `Cqrs.Request.command` or `Cqrs.Request.query`, like so:

```elm
import Cqrs.Query as Query
import Cqrs.Command as Command

Query.request url myCustomHttpErrorHandler defaultError ...
Command.request url myCustomHttpErrorHandler defaultError ...
```
