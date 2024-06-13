module Cqrs.Query exposing
    ( Response
    , request, requestTask
    , succeed, fail
    , map, mapError, succeeded, failed, data, reason
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

@docs map, mapError, succeeded, failed, data, reason


## Codecs

@docs decoder

-}

import Cmd.Extra
import Http
import Http.Helpers
import Json.Decode exposing (Decoder)
import RemoteData exposing (WebData)
import RemoteData.Http
import Task exposing (Task)
import Url exposing (Url)


{-| Represents the possible response states of a query.
-}
type Response a
    = Data a
    | Error String


{-| Checks if a given `Cqrs.Query.Response` is a `Data` variant.

    succeeded (Data []) --> True

    succeeded (Error "reason") --> False

-}
succeeded : Response a -> Bool
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
failed : Response a -> Bool
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
reason : Response a -> Maybe String
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
data : Response a -> Maybe a
data response =
    case response of
        Data value ->
            Just value

        Error _ ->
            Nothing


{-| Decodes a given payload into a `Cqrs.Query.Response a`.
-}
decoder : Decoder a -> Decoder (Response a)
decoder itemDecoder =
    let
        errorDecoder : Decoder (Response a)
        errorDecoder =
            Json.Decode.map fail <| Json.Decode.at [ "error" ] Json.Decode.string

        successDecoder : Decoder (Response a)
        successDecoder =
            let
                toData : Bool -> a -> Decoder (Response a)
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
map : (a -> b) -> Response a -> Response b
map fn response =
    case response of
        Data value ->
            succeed <| fn value

        Error error ->
            fail error


{-| Maps the `Error` variant of a given `Cqrs.Query.Response a`.
-}
mapError : (String -> String) -> Response a -> Response a
mapError fn response =
    case response of
        Data value ->
            succeed value

        Error error ->
            fail (fn error)


{-| Constructs the `Error` variant of a `Cqrs.Query.Response a`.

This is useful for testing how your UI will respond to the sad path, without actually sending a query.

-}
fail : String -> Response a
fail =
    Error


{-| Constructs the `Data` variant of a `Cqrs.Query.Response a`.

This is useful for testing how your UI will respond to the happy path, without actually sending a query.

-}
succeed : a -> Response a
succeed =
    Data


{-| Sends a query to the given URL and returns a `Cmd` for the given `msg` containing the parsed `Cqrs.Query.Response a`.
The value contained within the `Cqrs.Query.Response a`, will be the value parsed via the provided decoder.
-}
request : String -> Maybe (Http.Error -> String) -> (Response a -> msg) -> Decoder a -> Cmd msg
request url errorToString toMsg toData =
    let
        errorHandler : Http.Error -> String
        errorHandler =
            Maybe.withDefault Http.Helpers.errorToString errorToString

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
requestTask : String -> Maybe (Http.Error -> String) -> Decoder a -> Task () (Response a)
requestTask url errorToString toData =
    let
        errorHandler : Http.Error -> String
        errorHandler =
            Maybe.withDefault Http.Helpers.errorToString errorToString

        query : Url -> Task () (Response a)
        query uri =
            RemoteData.Http.getTask (Url.toString uri) (decoder toData)
                |> Task.map (fromRemoteData errorHandler)
    in
    Url.fromString url
        |> Maybe.map query
        |> Maybe.withDefault (Task.succeed invalidUrlFailure)


{-| Converts a given `RemoteData.WebData (Cqrs.Query.Response a)` into a `Cqrs.Query.Response a`.
-}
fromRemoteData : (Http.Error -> String) -> WebData (Response a) -> Response a
fromRemoteData errorToString response =
    let
        mappedResponse : RemoteData.RemoteData (Response a) (Response a)
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


invalidUrlFailure : Response a
invalidUrlFailure =
    fail "An absolute URL must be provided in the format: <scheme ':' ['//' authority] path ['?' query] ['#' fragment]>"
