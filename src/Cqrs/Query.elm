module Cqrs.Query exposing
    ( StandardRequestConfig, TaskRequestConfig, Response, QueryError, QueryResponse
    , request, requestWithConfig, requestTask, requestTaskWithConfig
    , succeed, fail
    , map, mapError, withDefault, succeeded, failed, data, reason
    , decoder
    )

{-|


## Types

@docs StandardRequestConfig, TaskRequestConfig, Response, QueryError, QueryResponse


## Actions

@docs request, requestWithConfig, requestTask, requestTaskWithConfig


## Constructors

@docs succeed, fail


## Helpers

@docs map, mapError, withDefault, succeeded, failed, data, reason


## Codecs

@docs decoder

-}

import Cmd.Extra
import Http exposing (Header)
import Http.Error exposing (errorToString)
import Json.Decode exposing (Decoder)
import RemoteData exposing (WebData)
import RemoteData.Http
import Task exposing (Task)
import Url exposing (Url)


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


{-| Represents the possible states of a query.
-}
type QueryResponse e a
    = Data a
    | Error e


{-| Represents the default inner data type of an `Error` variant.
-}
type alias QueryError =
    String


{-| Represents the response state of a query.
-}
type alias Response a =
    QueryResponse QueryError a


{-| Checks if a given `Cqrs.Query.Response` is a `Data` variant.

    succeeded (Data []) --> True

    succeeded (Error "reason") --> False

-}
succeeded : QueryResponse e a -> Bool
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
failed : QueryResponse e a -> Bool
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
reason : QueryResponse e a -> Maybe e
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
data : QueryResponse e a -> Maybe a
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

        successDecoder : Decoder (QueryResponse e a)
        successDecoder =
            let
                toData : Bool -> a -> Decoder (QueryResponse e a)
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
map : (a -> b) -> QueryResponse e a -> QueryResponse e b
map fn response =
    case response of
        Data value ->
            succeed <| fn value

        Error error ->
            fail error


{-| Maps the `Error` variant of a given `Cqrs.Query.Response a`.
-}
mapError : (e -> f) -> QueryResponse e a -> QueryResponse f a
mapError fn response =
    case response of
        Data value ->
            succeed value

        Error error ->
            fail <| fn error


{-| Constructs the `Error` variant of a `Cqrs.Query.Response a`.

This is useful for testing how your UI will respond to the sad path, without actually sending a query.

-}
fail : e -> QueryResponse e a
fail =
    Error


{-| Constructs the `Data` variant of a `Cqrs.Query.Response a`.

This is useful for testing how your UI will respond to the happy path, without actually sending a query.

-}
succeed : a -> QueryResponse e a
succeed =
    Data


{-| Sends a query to the given URL and returns a `Cmd` for the given `msg` containing the parsed `Cqrs.Query.Response a`.
The value contained within the `Cqrs.Query.Response a`, will be the value parsed via the provided decoder.
-}
request : String -> Maybe (Http.Error -> String) -> (Response a -> msg) -> Decoder a -> Cmd msg
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


{-| Similar to `request` but a `StandardRequestConfig` can be passed in for cases where customisation of the request headers, etc are desired.
-}
requestWithConfig : StandardRequestConfig -> String -> Maybe (Http.Error -> String) -> (Response a -> msg) -> Decoder a -> Cmd msg
requestWithConfig config url errorMapper toMsg toData =
    let
        errorHandler : Http.Error -> String
        errorHandler =
            Maybe.withDefault errorToString errorMapper

        query : Url -> Cmd msg
        query uri =
            RemoteData.Http.getWithConfig config (Url.toString uri) (fromRemoteData errorHandler >> toMsg) (decoder toData)
    in
    Url.fromString url
        |> Maybe.map query
        |> Maybe.withDefault (Cmd.Extra.perform <| toMsg invalidUrlFailure)


{-| Sends a query to the given URL and returns a `Task` which will contain the parsed `Cqrs.Query.Response a`.
The value contained within the `Cqrs.Query.Response a`, will be the value parsed via the provided decoder.
-}
requestTask : String -> Maybe (Http.Error -> String) -> Decoder a -> Task () (Response a)
requestTask url errorMapper toData =
    let
        errorHandler : Http.Error -> String
        errorHandler =
            Maybe.withDefault errorToString errorMapper

        query : Url -> Task () (Response a)
        query uri =
            RemoteData.Http.getTask (Url.toString uri) (decoder toData)
                |> Task.map (fromRemoteData errorHandler)
    in
    Url.fromString url
        |> Maybe.map query
        |> Maybe.withDefault (Task.succeed invalidUrlFailure)


{-| Similar to `requestTask` but a `TaskRequestConfig` can be passed in for cases where customisation of the request headers, etc are desired.
-}
requestTaskWithConfig : TaskRequestConfig -> String -> Maybe (Http.Error -> String) -> Decoder a -> Task () (Response a)
requestTaskWithConfig config url errorMapper toData =
    let
        errorHandler : Http.Error -> String
        errorHandler =
            Maybe.withDefault errorToString errorMapper

        query : Url -> Task () (Response a)
        query uri =
            RemoteData.Http.getTaskWithConfig config (Url.toString uri) (decoder toData)
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


{-| Provides a default case for a `Cqrs.Query.Response` in place of the value held within the `Error` variant.
-}
withDefault : a -> QueryResponse e a -> a
withDefault default response =
    case response of
        Data value ->
            value

        Error _ ->
            default
