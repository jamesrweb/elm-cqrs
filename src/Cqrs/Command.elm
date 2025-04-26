module Cqrs.Command exposing
    ( StandardRequestConfig, TaskRequestConfig, CommandResponse
    , request, requestWithConfig, requestTask, requestTaskWithConfig
    , succeed, fail, fromRemoteData
    , map, mapError, withDefault, unwrap, succeeded, failed, reason
    , decoder
    )

{-|


## Types

@docs StandardRequestConfig, TaskRequestConfig, CommandResponse


## Actions

@docs request, requestWithConfig, requestTask, requestTaskWithConfig


## Constructors

@docs succeed, fail, fromRemoteData


## Helpers

@docs map, mapError, withDefault, unwrap, succeeded, failed, reason


## Codecs

@docs decoder

-}

import Http exposing (Header)
import Json.Decode exposing (Decoder)
import Json.Encode
import RemoteData exposing (RemoteData)
import RemoteData.Http
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
type CommandResponse e a
    = Succeeded a
    | Failed e


{-| Constructs the `Succeeded` variant of a `Cqrs.Command.CommandResponse e ()`.

This is useful for testing how your UI will respond to the happy path, without actually sending a command.

-}
succeed : CommandResponse e ()
succeed =
    Succeeded ()


{-| Constructs the `Failed` variant of a `Cqrs.Command.CommandResponse e ()`.

This is useful for testing how your UI will respond to the sad path, without actually sending a command.

-}
fail : e -> CommandResponse e a
fail =
    Failed


{-| Checks if a given `Cqrs.Command.CommandResponse e ()` is a `Succeeded` variant.

    succeeded Succeeded --> True

    succeeded (Failed "reason") --> False

-}
succeeded : CommandResponse e a -> Bool
succeeded response =
    case response of
        Succeeded _ ->
            True

        Failed _ ->
            False


{-| Checks if a given `Cqrs.Command.CommandResponse e ()` is a `Failed` variant.

    failed Succeeded --> False

    failed (Failed "reason") --> True

-}
failed : CommandResponse e a -> Bool
failed response =
    case response of
        Succeeded _ ->
            False

        Failed _ ->
            True


{-| If a given `Cqrs.Command.CommandResponse e ()` was unsuccessful, the reason for the failure will be returned.

    reason Succeeded --> Nothing

    reason (Failed "reason") --> Just "reason"

-}
reason : CommandResponse e a -> Maybe e
reason response =
    case response of
        Succeeded _ ->
            Nothing

        Failed cause ->
            Just cause


{-| Decodes a given payload into a `Cqrs.Command.CommandResponse e ()`.
-}
decoder : Decoder e -> Decoder (CommandResponse e ())
decoder errorFn =
    let
        error : Decoder (CommandResponse e ())
        error =
            Json.Decode.map Failed <| Json.Decode.at [ "error" ] errorFn

        success : Decoder (CommandResponse e ())
        success =
            Json.Decode.succeed <| Succeeded ()
    in
    Json.Decode.oneOf
        [ error
        , success
        ]


{-| Sends a command to the given URL with the provided body and returns a parsed `Cqrs.Command.CommandResponse e ()` in turn.
-}
request : String -> (e -> f) -> f -> Json.Encode.Value -> Json.Decode.Decoder e -> (CommandResponse f () -> msg) -> Cmd msg
request url errorMapper defaultError body toError toMsg =
    RemoteData.Http.post url (fromRemoteData errorMapper defaultError >> toMsg) (decoder toError) body


{-| Similar to `request` but a `StandardRequestConfig` can be passed in for cases where customisation of the request headers, etc are desired.
-}
requestWithConfig : StandardRequestConfig -> String -> (e -> f) -> f -> Json.Encode.Value -> Json.Decode.Decoder e -> (CommandResponse f () -> msg) -> Cmd msg
requestWithConfig config url errorMapper defaultError body toError toMsg =
    RemoteData.Http.postWithConfig config url (fromRemoteData errorMapper defaultError >> toMsg) (decoder toError) body


{-| Sends a command to the given URL and returns a `Task` containing the parsed `Cqrs.Command.CommandResponse e ()`.
The value contained within the `Cqrs.Command.CommandResponse e ()`, will be the value parsed via the provided decoder.
-}
requestTask : String -> (e -> f) -> f -> Json.Encode.Value -> Json.Decode.Decoder e -> Task () (CommandResponse f ())
requestTask url errorMapper defaultError body toError =
    RemoteData.Http.postTask url (decoder toError) body
        |> Task.map (fromRemoteData errorMapper defaultError)


{-| Similar to `requestTask` but a `TaskRequestConfig` can be passed in for cases where customisation of the request headers, etc are desired.
-}
requestTaskWithConfig : TaskRequestConfig -> String -> (e -> f) -> f -> Json.Encode.Value -> Json.Decode.Decoder e -> Task () (CommandResponse f ())
requestTaskWithConfig config url errorMapper defaultError body toError =
    RemoteData.Http.postTaskWithConfig config url (decoder toError) body
        |> Task.map (fromRemoteData errorMapper defaultError)


{-| Converts a given `RemoteData x (CommandResponse e ())` into a `Cqrs.Command.CommandResponse e ()`.
-}
fromRemoteData : (e -> f) -> f -> RemoteData x (CommandResponse e ()) -> CommandResponse f ()
fromRemoteData errorMapper defaultError response =
    RemoteData.unwrap (fail defaultError) (mapError errorMapper) response


{-| Maps the `Failed` variant of a given `Cqrs.Command.CommandResponse e ()`.
-}
mapError : (e -> f) -> CommandResponse e a -> CommandResponse f a
mapError fn response =
    case response of
        Succeeded value ->
            Succeeded value

        Failed error ->
            fail (fn error)


{-| Maps the `Succeeded` variant of a given `Cqrs.Command.CommandResponse e ()`
-}
map : (a -> b) -> CommandResponse e a -> CommandResponse e b
map fn response =
    case response of
        Succeeded value ->
            Succeeded <| fn value

        Failed error ->
            fail error


{-| Unwraps a homogeneous `Cqrs.Command.CommandResponse e ()` to its' inner value from the given variant.
Note: This function is almost useless since only `()` is ever viable to actually unwrap as a `x` value.
-}
unwrap : CommandResponse x x -> x
unwrap response =
    case response of
        Succeeded value ->
            value

        Failed error ->
            error


{-| Provides a default case for a `Cqrs.Command.CommandResponse e ()` where the `Succeeded` variant is in the default `()` state.
-}
withDefault : e -> CommandResponse e () -> e
withDefault default response =
    case response of
        Succeeded _ ->
            default

        Failed error ->
            error
