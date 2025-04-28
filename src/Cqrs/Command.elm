module Cqrs.Command exposing
    ( StandardRequestConfiguration, TaskRequestConfiguration, RequestSettings, RequestWithConfigurationSettings, RequestTaskSettings, RequestTaskWithConfigurationSettings, CommandResponse
    , request, requestWithConfiguration, requestTask, requestTaskWithConfiguration
    , succeed, fail, fromResult, fromRemoteData
    , reason
    , mapError, unwrap, unpack, partition, toResult, toMaybe
    , succeeded, failed
    , decoder
    )

{-|

@docs StandardRequestConfiguration, TaskRequestConfiguration, RequestSettings, RequestWithConfigurationSettings, RequestTaskSettings, RequestTaskWithConfigurationSettings, CommandResponse


## HTTP

@docs request, requestWithConfiguration, requestTask, requestTaskWithConfiguration


## Constructors

@docs succeed, fail, fromResult, fromRemoteData


## State

@docs reason


## Mappers

@docs mapError, unwrap, unpack, partition, toResult, toMaybe


## Helpers

@docs succeeded, failed


## Codec

@docs decoder

-}

import Http exposing (Header)
import Json.Decode exposing (Decoder)
import Json.Encode
import RemoteData exposing (RemoteData)
import RemoteData.Http
import Result.Extra
import Task exposing (Task)


{-| Configuration for requests resulting in a `Cmd`
-}
type alias StandardRequestConfiguration =
    { headers : List Header
    , timeout : Maybe Float
    , tracker : Maybe String
    , risky : Bool
    }


{-| Configuration for requests resulting in a `Task`
-}
type alias TaskRequestConfiguration =
    { headers : List Header
    , timeout : Maybe Float
    , risky : Bool
    }


{-| Configuration settings for a `Cqrs.Command.request`
-}
type alias RequestSettings issue error msg =
    { body : Json.Encode.Value
    , defaultError : error
    , errorMapper : issue -> error
    , toError : Decoder issue
    , toMsg : CommandResponse error -> msg
    , url : String
    }


{-| Configuration settings for a `Cqrs.Command.requestWithConfig`
-}
type alias RequestWithConfigurationSettings issue error msg =
    { body : Json.Encode.Value
    , config : StandardRequestConfiguration
    , defaultError : error
    , errorMapper : issue -> error
    , toError : Decoder issue
    , toMsg : CommandResponse error -> msg
    , url : String
    }


{-| Configuration settings for a `Cqrs.Command.requestTask`
-}
type alias RequestTaskSettings issue error =
    { body : Json.Encode.Value
    , defaultError : error
    , errorMapper : issue -> error
    , toError : Decoder issue
    , url : String
    }


{-| Configuration settings for a `Cqrs.Command.requestTaskWithConfig`
-}
type alias RequestTaskWithConfigurationSettings issue error =
    { body : Json.Encode.Value
    , config : TaskRequestConfiguration
    , defaultError : error
    , errorMapper : issue -> error
    , toError : Decoder issue
    , url : String
    }


{-| Represents the possible states of a command.
-}
type CommandResponse error
    = Succeeded
    | Failed error


{-| Constructs the `Succeeded` variant of a `Cqrs.Command.CommandResponse error`.

This is useful for testing how your UI will respond to the happy path, without actually sending a command.

-}
succeed : CommandResponse error
succeed =
    Succeeded


{-| Constructs the `Failed` variant of a `Cqrs.Command.CommandResponse error`.

This is useful for testing how your UI will respond to the sad path, without actually sending a command.

-}
fail : error -> CommandResponse error
fail =
    Failed


{-| Checks if a given `Cqrs.Command.CommandResponse error` is a `Succeeded` variant.

    succeeded Succeeded --> True

    succeeded (Failed "reason") --> False

-}
succeeded : CommandResponse error -> Bool
succeeded =
    toResult >> Result.Extra.isOk


{-| Checks if a given `Cqrs.Command.CommandResponse error` is a `Failed` variant.

    failed Succeeded --> False

    failed (Failed "reason") --> True

-}
failed : CommandResponse error -> Bool
failed =
    toResult >> Result.Extra.isErr


{-| If a given `Cqrs.Command.CommandResponse error` was unsuccessful, the reason for the failure will be returned.

    reason Succeeded --> Nothing

    reason (Failed "reason") --> Just "reason"

-}
reason : CommandResponse error -> Maybe error
reason =
    toResult >> Result.Extra.error


{-| Decodes a given payload into a `Cqrs.Command.CommandResponse error`.
-}
decoder : Decoder error -> Decoder (CommandResponse error)
decoder errorFn =
    let
        error : Decoder (CommandResponse error)
        error =
            Json.Decode.map Failed <| Json.Decode.at [ "error" ] errorFn

        success : Decoder (CommandResponse error)
        success =
            Json.Decode.succeed Succeeded
    in
    Json.Decode.oneOf
        [ error
        , success
        ]


{-| Sends a command to the given URL with the provided body and returns a parsed `Cqrs.Command.CommandResponse error` in turn.
-}
request : RequestSettings issue error msg -> Cmd msg
request { body, defaultError, errorMapper, toError, toMsg, url } =
    RemoteData.Http.post url (fromRemoteData errorMapper defaultError >> toMsg) (decoder toError) body


{-| Similar to `request` but a `StandardRequestConfig` can be passed in for cases where customisation of the request headers, etc are desired.
-}
requestWithConfiguration : RequestWithConfigurationSettings issue error msg -> Cmd msg
requestWithConfiguration { body, config, defaultError, errorMapper, toError, toMsg, url } =
    RemoteData.Http.postWithConfig config url (fromRemoteData errorMapper defaultError >> toMsg) (decoder toError) body


{-| Sends a command to the given URL and returns a `Task` containing the parsed `Cqrs.Command.CommandResponse error`.
The value contained within the `Cqrs.Command.CommandResponse error`, will be the value parsed via the provided decoder.
-}
requestTask : RequestTaskSettings issue error -> Task () (CommandResponse error)
requestTask { body, defaultError, errorMapper, toError, url } =
    RemoteData.Http.postTask url (decoder toError) body
        |> Task.map (fromRemoteData errorMapper defaultError)


{-| Similar to `requestTask` but a `TaskRequestConfig` can be passed in for cases where customisation of the request headers, etc are desired.
-}
requestTaskWithConfiguration : RequestTaskWithConfigurationSettings issue error -> Task () (CommandResponse error)
requestTaskWithConfiguration { body, config, defaultError, errorMapper, toError, url } =
    RemoteData.Http.postTaskWithConfig config url (decoder toError) body
        |> Task.map (fromRemoteData errorMapper defaultError)


{-| Converts a given `RemoteData x (CommandResponse issue)` into a `Cqrs.Command.CommandResponse error`.
-}
fromRemoteData : (issue -> error) -> error -> RemoteData x (CommandResponse issue) -> CommandResponse error
fromRemoteData errorMapper defaultError response =
    RemoteData.unwrap (fail defaultError) (mapError errorMapper) response


{-| Maps the `Failed` variant of a given `Cqrs.Command.CommandResponse error`.
-}
mapError : (error -> nextError) -> CommandResponse error -> CommandResponse nextError
mapError fn =
    toResult >> Result.mapError fn >> fromResult


{-| Converts a `Cqrs.Command.CommandResponse error` to a `Result error data`
-}
toResult : CommandResponse error -> Result error ()
toResult response =
    case response of
        Succeeded ->
            Result.Ok ()

        Failed error ->
            Result.Err error


{-| Converts a `Result error data` to a `Cqrs.Command.CommandResponse error`
-}
fromResult : Result error data -> CommandResponse error
fromResult result =
    Result.Extra.unpack fail (always succeed) result


{-| Converts a `Cqrs.Command.CommandResponse error` to a `Maybe ()`
-}
toMaybe : CommandResponse error -> Maybe ()
toMaybe =
    toResult >> Result.toMaybe


{-| Partitions a series of `CommandResponse error` instances into a tuple of successful and unsuccessful responses.
-}
partition : List (CommandResponse error) -> ( List (), List error )
partition =
    List.map toResult >> Result.Extra.partition


{-| Convert a `CommandResponse error` to a `value` by applying either the first function if the `CommandResponse error` is an `Failed` variant or the second function if the `CommandResponse error` is a `Succeeded` variant.
-}
unpack : (error -> value) -> (() -> value) -> CommandResponse error -> value
unpack errorFn dataFn =
    toResult >> Result.Extra.unpack errorFn dataFn


{-| Convert a `CommandResponse error` to a `value` by applying a function if the `CommandResponse error` is a `Succeeded` variant or using the provided default value if it is an `Failed` variant.
-}
unwrap : value -> (() -> value) -> CommandResponse error -> value
unwrap default dataFn =
    toResult >> Result.Extra.unwrap default dataFn
