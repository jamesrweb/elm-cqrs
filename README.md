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
    Query.request {
        url = path
        , toMsg = UserQueryExecuted
        , toData = User.decoder
        , toError = Error.decoder
        , defaultError = Error.Unknown
    }

findUserQueryTask : Uuid -> Task () UserQueryResponse
findUserQueryTask id =
    let
        path : String
        path = "/api/v1/user/" ++ Uuid.toString id
    in
    Query.requestTask {
        url = path
        , toData = User.decoder
        , toError = Error.decoder
        , defaultError = Error.Unknown
    }
```
