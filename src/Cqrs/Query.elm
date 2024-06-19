module Cqrs.Query exposing
    ( Response
    , request, requestTask
    , succeed, fail
    , map, mapError, withDefault, succeeded, failed, data, reason
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

@docs map, mapError, withDefault, succeeded, failed, data, reason


## Codecs

@docs decoder

-}

import Cmd.Extra
import Http
import Http.Error exposing (errorToString)
import Json.Decode exposing (Decoder)
import RemoteData exposing (WebData)
import RemoteData.Http
import Task exposing (Task)
import Url exposing (Url)


{-| Represents the possible response states of a query.
-}
type Response e a
    = Data a
    | Error e


{-| Checks if a given `Cqrs.Query.Response` is a `Data` variant.

    succeeded (Data []) --> True

    succeeded (Error "reason") --> False

-}
succeeded : Response e a -> Bool
succeeded response =
    case response of
        Data _ ->
            True

        Error _ ->
            False


{-| Checks if a given `Cqrs.Query.Response` is an `Error` variant.

    failed (Data []) --> False

    failed (Error "reason") --> True

-}
failed : Response e a -> Bool
failed response =
    case response of
        Data _ ->
            False

        Error _ ->
            True


{-| If a given `Cqrs.Query.Response` was unsuccessful, the reason for the failure will be returned.

    reason (Data []) --> Nothing

    reason (Error "reason") --> Just "reason"

-}
reason : Response e a -> Maybe e
reason response =
    case response of
        Data _ ->
            Nothing

        Error cause ->
            Just cause


{-| If a given `Cqrs.Query.Response` was successful, the data it contains will be returned.

    data (Data []) --> Just []

    data (Error "reason") --> Nothing

-}
data : Response e a -> Maybe a
data response =
    case response of
        Data value ->
            Just value

        Error _ ->
            Nothing


{-| Decodes a given payload into a `Cqrs.Query.Response a`.
-}
decoder : Decoder a -> Decoder (Response String a)
decoder itemDecoder =
    let
        errorDecoder : Decoder (Response String a)
        errorDecoder =
            Json.Decode.map fail <| Json.Decode.at [ "error" ] Json.Decode.string

        successDecoder : Decoder (Response e a)
        successDecoder =
            let
                toData : Bool -> a -> Decoder (Response e a)
                toData accepted value =
                    if accepted then
                        Json.Decode.succeed <| succeed value

                    else
                        Json.Decode.fail "An unknown error occurred"
            in
            Json.Decode.map2 toData
                (Json.Decode.field "success" Json.Decode.bool)
                (Json.Decode.field "data" itemDecoder)
                |> Json.Decode.andThen identity
    in
    Json.Decode.oneOf
        [ errorDecoder
        , successDecoder
        ]


{-| Maps the `Data` variant of a given `Cqrs.Query.Response a` and returns a `Cqrs.Query.Response b`.
-}
map : (a -> b) -> Response e a -> Response e b
map fn response =
    case response of
        Data value ->
            succeed <| fn value

        Error error ->
            fail error


{-| Maps the `Error` variant of a given `Cqrs.Query.Response a`.
-}
mapError : (e -> f) -> Response e a -> Response f a
mapError fn response =
    case response of
        Data value ->
            succeed value

        Error error ->
            fail <| fn error


{-| Constructs the `Error` variant of a `Cqrs.Query.Response a`.

This is useful for testing how your UI will respond to the sad path, without actually sending a query.

-}
fail : e -> Response e a
fail =
    Error


{-| Constructs the `Data` variant of a `Cqrs.Query.Response a`.

This is useful for testing how your UI will respond to the happy path, without actually sending a query.

-}
succeed : a -> Response e a
succeed =
    Data


{-| Sends a query to the given URL and returns a `Cmd` for the given `msg` containing the parsed `Cqrs.Query.Response a`.
The value contained within the `Cqrs.Query.Response a`, will be the value parsed via the provided decoder.
-}
request : String -> Maybe (Http.Error -> String) -> (Response String a -> msg) -> Decoder a -> Cmd msg
request url errorMapper toMsg toData =
    let
        errorHandler : Http.Error -> String
        errorHandler =
            Maybe.withDefault errorToString errorMapper

        query : Url -> Cmd msg
        query uri =
            RemoteData.Http.get (Url.toString uri) (fromRemoteData errorHandler >> toMsg) (decoder toData)
    in
    Url.fromString url
        |> Maybe.map query
        |> Maybe.withDefault (Cmd.Extra.perform <| toMsg invalidUrlFailure)


{-| Sends a query to the given URL and returns a `Task` which will contain the parsed `Cqrs.Query.Response a`.
The value contained within the `Cqrs.Query.Response a`, will be the value parsed via the provided decoder.
-}
requestTask : String -> Maybe (Http.Error -> String) -> Decoder a -> Task () (Response String a)
requestTask url errorMapper toData =
    let
        errorHandler : Http.Error -> String
        errorHandler =
            Maybe.withDefault errorToString errorMapper

        query : Url -> Task () (Response String a)
        query uri =
            RemoteData.Http.getTask (Url.toString uri) (decoder toData)
                |> Task.map (fromRemoteData errorHandler)
    in
    Url.fromString url
        |> Maybe.map query
        |> Maybe.withDefault (Task.succeed invalidUrlFailure)


{-| Converts a given `RemoteData.WebData (Cqrs.Query.Response a)` into a `Cqrs.Query.Response a`.
-}
fromRemoteData : (Http.Error -> String) -> WebData (Response String a) -> Response String a
fromRemoteData errorToString response =
    let
        mappedResponse : RemoteData.RemoteData (Response String a) (Response String a)
        mappedResponse =
            RemoteData.mapBoth identity (errorToString >> fail) response
    in
    case mappedResponse of
        RemoteData.NotAsked ->
            fail "The query has not been sent yet"

        RemoteData.Loading ->
            fail "The response is still being loaded"

        RemoteData.Failure errorResponse ->
            errorResponse

        RemoteData.Success dataResponse ->
            dataResponse


invalidUrlFailure : Response String a
invalidUrlFailure =
    fail "An absolute URL must be provided in the format: <scheme ':' ['//' authority] path ['?' query] ['#' fragment]>"


{-| Provides a default case for a `Cqrs.Query.Response` in place of the value held within the `Error` variant.
-}
withDefault : a -> Response e a -> a
withDefault default response =
    case response of
        Data value ->
            value

        Error _ ->
            default
