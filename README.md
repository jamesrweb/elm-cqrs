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
    { -- Must be `True` to successfully decode the `data`, when decoding occurs
      success : Bool
    , data : a
    }

type alias ApiError =
    { success : Bool
    , error : String
    }
```

1. If an `error` is present, the failure variant for `Cqrs.Command.Response` or `Cqrs.Query.Response` will be decoded.
2. If the `data` is present in the response but the `success` value is `false`, the failure variant for `Cqrs.Command.Response` or `Cqrs.Query.Response` will be decoded.
3. If the `data` key is present in a command response, it will be discarded, since commands do not return data.
4. If the `success` key is `false` for a command or a query but the `error` is not set, the decoder will fail with an `Err` result.

Please see the tests for more examples of how the decoders handle the different cases.

Furthermore, all urls provided to `Cqrs.Query.request` and `Cqrs.Query.response` must be absolute [URLs](https://en.wikipedia.org/wiki/URL#Syntax).

### Commands

A command is an instruction which returns no value in response. It is simply successful or not.

To send a command, you can do the following:

```elm
import Cqrs.Command as Command
import User exposing (User) -- An example module

type Msg =
    UserCommandExecuted Command.Response

baseUrl : String
baseUrl =
    "https://example.com"

addUserCommand : User -> Cmd Msg
addUserCommand user =
    let
        url : String
        url = baseUrl ++ "/api/v1/user"

        body : Json.Encode.Value
        body =
            User.encode user
    in
    Command.request url Nothing body UserCommandExecuted

addUserCommandTask : User -> Task () UserResponse
addUserCommandTask user =
    let
        url : String
        url = baseUrl ++ "/api/v1/user"

        body : Json.Encode.Value
        body =
            User.encode user
    in
    Command.requestTask url Nothing body
```

### Queries

A query is a request to look up a value or series of values in response. It will either return the desired values, or an appropriate error.

To execute a query, you can do the following:

```elm
import Cqrs.Query as Query
import UUID exposing (UUID) -- elm install TSFoster/elm-uuid (for example)
import User exposing (User) -- An example module

type alias UserResponse =
    Query.Response User

type Msg =
    UserQueryExecuted UserResponse

baseUrl : String
baseUrl =
    "https://example.com"

findUserQuery : Uuid -> Cmd Msg
findUserQuery id =
    let
        url : String
        url = baseUrl ++ "/api/v1/user/" ++ Uuid.toString id
    in
    Query.request url Nothing UserQueryExecuted User.decoder

findUserQueryTask : Uuid -> Task () UserResponse
findUserQueryTask id =
    let
        url : String
        url = baseUrl ++ "/api/v1/user/" ++ Uuid.toString id
    in
    Query.requestTask url Nothing User.decoder
```

### Http error handlers

In both cases, `Cqrs.Request.command` and `Cqrs.Request.query`, you can pass an optional http error handler in case you need custom errors or logging to be done, for example. If `Nothing` is provided, a default error handler will be triggered to format the incoming HTTP error.

To create a custom http error handler, you need to implement a function which takes an `Http.Error` and returns a `String`. For example:

```elm
import Http exposing (Error(..)) -- elm install elm/http

myCustomHttpErrorHandler : Http.Error -> String
myCustomHttpErrorHandler error =
    case error of
        ...
```

And then to use it in your requests, you pass it in the second parameter to either `Cqrs.Request.command` or `Cqrs.Request.query`, like so:

```elm
import Cqrs.Query as Query
import Cqrs.Command as Command

Query.request url (Just myCustomHttpErrorHandler) ...
Command.request url (Just myCustomHttpErrorHandler) ...
```
