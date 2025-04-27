module Cqrs.Command exposing
    ( StandardRequestConfig, TaskRequestConfig, CommandResponse
    , request, requestWithConfig, requestTask, requestTaskWithConfig
    , succeed, fail, fromResult, fromRemoteData
    , reason
    , mapError, unwrap, unpack, partition, toResult, toMaybe
    , succeeded, failed
    , decoder
    )

{-|

@docs StandardRequestConfig, TaskRequestConfig, CommandResponse


## HTTP

@docs request, requestWithConfig, requestTask, requestTaskWithConfig


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
type alias StandardRequestConfig =
    { headers : List Header
    , timeout : Maybe Float
    , tracker : Maybe String
    , risky : Bool
    }


{-| Configuration for requests resulting in a `Task`
-}
type alias TaskRequestConfig =
    { headers : List Header
    , timeout : Maybe Float
    , risky : Bool
    }


{-| Represents the possible states of a command.
-}
type CommandResponse e
    = Succeeded
    | Failed e


{-| Constructs the `Succeeded` variant of a `Cqrs.Command.CommandResponse e`.

This is useful for testing how your UI will respond to the happy path, without actually sending a command.

-}
succeed : CommandResponse e
succeed =
    Succeeded


{-| Constructs the `Failed` variant of a `Cqrs.Command.CommandResponse e`.

This is useful for testing how your UI will respond to the sad path, without actually sending a command.

-}
fail : e -> CommandResponse e
fail =
    Failed


{-| Checks if a given `Cqrs.Command.CommandResponse e` is a `Succeeded` variant.

    succeeded Succeeded --> True

    succeeded (Failed "reason") --> False

-}
succeeded : CommandResponse e -> Bool
succeeded =
    toResult >> Result.Extra.isOk


{-| Checks if a given `Cqrs.Command.CommandResponse e` is a `Failed` variant.

    failed Succeeded --> False

    failed (Failed "reason") --> True

-}
failed : CommandResponse e -> Bool
failed =
    toResult >> Result.Extra.isErr


{-| If a given `Cqrs.Command.CommandResponse e` was unsuccessful, the reason for the failure will be returned.

    reason Succeeded --> Nothing

    reason (Failed "reason") --> Just "reason"

-}
reason : CommandResponse e -> Maybe e
reason =
    toResult >> Result.Extra.error


{-| Decodes a given payload into a `Cqrs.Command.CommandResponse e`.
-}
decoder : Decoder e -> Decoder (CommandResponse e)
decoder errorFn =
    let
        error : Decoder (CommandResponse e)
        error =
            Json.Decode.map Failed <| Json.Decode.at [ "error" ] errorFn

        success : Decoder (CommandResponse e)
        success =
            Json.Decode.succeed Succeeded
    in
    Json.Decode.oneOf
        [ error
        , success
        ]


{-| Sends a command to the given URL with the provided body and returns a parsed `Cqrs.Command.CommandResponse e` in turn.
-}
request : String -> (e -> f) -> f -> Json.Encode.Value -> Json.Decode.Decoder e -> (CommandResponse f -> msg) -> Cmd msg
request url errorMapper defaultError body toError toMsg =
    RemoteData.Http.post url (fromRemoteData errorMapper defaultError >> toMsg) (decoder toError) body


{-| Similar to `request` but a `StandardRequestConfig` can be passed in for cases where customisation of the request headers, etc are desired.
-}
requestWithConfig : StandardRequestConfig -> String -> (e -> f) -> f -> Json.Encode.Value -> Json.Decode.Decoder e -> (CommandResponse f -> msg) -> Cmd msg
requestWithConfig config url errorMapper defaultError body toError toMsg =
    RemoteData.Http.postWithConfig config url (fromRemoteData errorMapper defaultError >> toMsg) (decoder toError) body


{-| Sends a command to the given URL and returns a `Task` containing the parsed `Cqrs.Command.CommandResponse e`.
The value contained within the `Cqrs.Command.CommandResponse e`, will be the value parsed via the provided decoder.
-}
requestTask : String -> (e -> f) -> f -> Json.Encode.Value -> Json.Decode.Decoder e -> Task () (CommandResponse f)
requestTask url errorMapper defaultError body toError =
    RemoteData.Http.postTask url (decoder toError) body
        |> Task.map (fromRemoteData errorMapper defaultError)


{-| Similar to `requestTask` but a `TaskRequestConfig` can be passed in for cases where customisation of the request headers, etc are desired.
-}
requestTaskWithConfig : TaskRequestConfig -> String -> (e -> f) -> f -> Json.Encode.Value -> Json.Decode.Decoder e -> Task () (CommandResponse f)
requestTaskWithConfig config url errorMapper defaultError body toError =
    RemoteData.Http.postTaskWithConfig config url (decoder toError) body
        |> Task.map (fromRemoteData errorMapper defaultError)


{-| Converts a given `RemoteData x (CommandResponse e)` into a `Cqrs.Command.CommandResponse e`.
-}
fromRemoteData : (e -> f) -> f -> RemoteData x (CommandResponse e) -> CommandResponse f
fromRemoteData errorMapper defaultError response =
    RemoteData.unwrap (fail defaultError) (mapError errorMapper) response


{-| Maps the `Failed` variant of a given `Cqrs.Command.CommandResponse e`.
-}
mapError : (e -> f) -> CommandResponse e -> CommandResponse f
mapError fn =
    toResult >> Result.mapError fn >> fromResult


{-| Converts a `Cqrs.Command.CommandResponse e` to a `Result e a`
-}
toResult : CommandResponse e -> Result e ()
toResult response =
    case response of
        Succeeded ->
            Result.Ok ()

        Failed error ->
            Result.Err error


{-| Converts a `Result e a` to a `Cqrs.Command.CommandResponse e`
-}
fromResult : Result e a -> CommandResponse e
fromResult result =
    Result.Extra.unpack fail (always succeed) result


{-| Converts a `Cqrs.Command.CommandResponse e` to a `Maybe a`
-}
toMaybe : CommandResponse e -> Maybe ()
toMaybe =
    toResult >> Result.toMaybe


{-| Partitions a series of `CommandResponse e` instances into a tuple of successful and unsuccessful responses.
-}
partition : List (CommandResponse e) -> ( List (), List e )
partition =
    List.map toResult >> Result.Extra.partition


{-| Convert a `CommandResponse e` to an `a` by applying either the first function if the `CommandResponse e` is an `Failed` variant or the second function if the `CommandResponse e` is a `Succeeded` variant.
-}
unpack : (e -> b) -> (() -> b) -> CommandResponse e -> b
unpack errorFn dataFn =
    toResult >> Result.Extra.unpack errorFn dataFn


{-| Convert a `CommandResponse e` to an `a` by applying a function if the `CommandResponse e` is a `Succeeded` variant or using the provided default value if it is an `Failed` variant.
-}
unwrap : b -> (() -> b) -> CommandResponse e -> b
unwrap default dataFn =
    toResult >> Result.Extra.unwrap default dataFn
