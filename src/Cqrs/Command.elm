module Cqrs.Command exposing
    ( Response
    , request, requestTask
    , succeed, fail
    , map, mapError, withDefault, unwrap, succeeded, failed, reason
    , decoder
    )

{-|


## Types

@docs Response


## Actions

@docs request, requestTask


## Constructors

@docs succeed, fail


## Helpers

@docs map, mapError, withDefault, unwrap, succeeded, failed, reason


## Codecs

@docs decoder

-}

import Cmd.Extra
import Http
import Http.Error exposing (errorToString)
import Json.Decode exposing (Decoder)
import Json.Encode
import RemoteData exposing (WebData)
import RemoteData.Http
import Task exposing (Task)
import Url exposing (Url)


{-| Represents the possible response states of a command.
-}
type Response e a
    = Succeeded a
    | Failed e


{-| Constructs the `Succeeded` variant of a `Cqrs.Command.Response`.

This is useful for testing how your UI will respond to the happy path, without actually sending a command.

-}
succeed : Response e ()
succeed =
    Succeeded ()


{-| Constructs the `Failed` variant of a `Cqrs.Command.Response`.

This is useful for testing how your UI will respond to the sad path, without actually sending a command.

-}
fail : e -> Response e a
fail =
    Failed


{-| Checks if a given `Cqrs.Command.Response` is a `Succeeded` variant.

    succeeded Succeeded --> True

    succeeded (Failed "reason") --> False

-}
succeeded : Response e a -> Bool
succeeded response =
    case response of
        Succeeded _ ->
            True

        Failed _ ->
            False


{-| Checks if a given `Cqrs.Command.Response` is a `Failed` variant.

    failed Succeeded --> False

    failed (Failed "reason") --> True

-}
failed : Response e a -> Bool
failed response =
    case response of
        Succeeded _ ->
            False

        Failed _ ->
            True


{-| If a given `Cqrs.Command.Response` was unsuccessful, the reason for the failure will be returned.

    reason Succeeded --> Nothing

    reason (Failed "reason") --> Just "reason"

-}
reason : Response e a -> Maybe e
reason response =
    case response of
        Succeeded _ ->
            Nothing

        Failed cause ->
            Just cause


{-| Decodes a given payload into a `Cqrs.Command.Response`.
-}
decoder : Decoder (Response String ())
decoder =
    let
        error : Decoder (Response String a)
        error =
            Json.Decode.map Failed <| Json.Decode.at [ "error" ] Json.Decode.string

        success : Decoder (Response e ())
        success =
            let
                toData : Bool -> Decoder (Response e ())
                toData accepted =
                    if accepted then
                        Json.Decode.succeed <| Succeeded ()

                    else
                        Json.Decode.fail "An unknown error occurred"
            in
            Json.Decode.field "success" Json.Decode.bool
                |> Json.Decode.andThen toData
    in
    Json.Decode.oneOf
        [ error
        , success
        ]


{-| Sends a command to the given URL with the provided body and returns a parsed `Cqrs.Command.Response` in turn.
-}
request : String -> Maybe (Http.Error -> String) -> Json.Encode.Value -> (Response String () -> msg) -> Cmd msg
request url errorMapper body toMsg =
    let
        command : Url -> Cmd msg
        command uri =
            RemoteData.Http.post (Url.toString uri) (fromRemoteData errorHandler >> toMsg) decoder body

        errorHandler : Http.Error -> String
        errorHandler =
            Maybe.withDefault errorToString errorMapper
    in
    Url.fromString url
        |> Maybe.map command
        |> Maybe.withDefault (Cmd.Extra.perform <| toMsg invalidUrlFailure)


{-| Sends a command to the given URL and returns a `Task` containing the parsed `Cqrs.Command.Response`.
The value contained within the `Cqrs.Command.Response`, will be the value parsed via the provided decoder.
-}
requestTask : String -> Maybe (Http.Error -> String) -> Json.Encode.Value -> Task () (Response String ())
requestTask url errorMapper body =
    let
        command : Url -> Task () (Response String ())
        command uri =
            RemoteData.Http.postTask (Url.toString uri) decoder body
                |> Task.map (fromRemoteData errorHandler)

        errorHandler : Http.Error -> String
        errorHandler =
            Maybe.withDefault errorToString errorMapper
    in
    Url.fromString url
        |> Maybe.map command
        |> Maybe.withDefault (Task.succeed invalidUrlFailure)


{-| Converts a given `RemoteData.WebData a` into a `Cqrs.Command.Response`.
-}
fromRemoteData : (Http.Error -> String) -> WebData a -> Response String ()
fromRemoteData errorToString response =
    let
        mappedResponse : RemoteData.RemoteData (Response String ()) (Response String ())
        mappedResponse =
            RemoteData.mapBoth (always <| Succeeded ()) (errorToString >> Failed) response
    in
    case mappedResponse of
        RemoteData.NotAsked ->
            Failed "The command has not been sent yet"

        RemoteData.Loading ->
            Failed "The response is still being loaded"

        RemoteData.Failure error ->
            error

        RemoteData.Success value ->
            value


invalidUrlFailure : Response String a
invalidUrlFailure =
    fail "An absolute URL must be provided in the format: <scheme ':' ['//' authority] path ['?' query] ['#' fragment]>"


{-| Maps the `Failed` variant of a given `Cqrs.Command.Response`.
-}
mapError : (e -> f) -> Response e a -> Response f a
mapError fn response =
    case response of
        Succeeded value ->
            Succeeded value

        Failed error ->
            fail (fn error)


{-| Maps the `Succeeded` variant of a given `Cqrs.Command.Response`
-}
map : (a -> b) -> Response e a -> Response e b
map fn response =
    case response of
        Succeeded value ->
            Succeeded <| fn value

        Failed error ->
            fail error


{-| Unwraps a homogeneous `Cqrs.Command.Response` to its' inner value from the given variant.
-}
unwrap : Response x x -> x
unwrap response =
    case response of
        Succeeded value ->
            value

        Failed error ->
            error


{-| Provides a default case for a `Cqrs.Command.Response` where the `Succeeded` variant is in the default `()` state.
-}
withDefault : e -> Response e () -> e
withDefault default response =
    case response of
        Succeeded _ ->
            default

        Failed error ->
            error
