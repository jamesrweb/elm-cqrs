module Cqrs.Query exposing
    ( StandardRequestConfig, TaskRequestConfig, QueryResponse
    , request, requestWithConfig, requestTask, requestTaskWithConfig
    , succeed, fail, fromRemoteData
    , map, mapError, withDefault, succeeded, failed, data, reason
    , decoder
    )

{-|


## Types

@docs StandardRequestConfig, TaskRequestConfig, QueryResponse


## Actions

@docs request, requestWithConfig, requestTask, requestTaskWithConfig


## Constructors

@docs succeed, fail, fromRemoteData


## Helpers

@docs map, mapError, withDefault, succeeded, failed, data, reason


## Codecs

@docs decoder

-}

import Http exposing (Header)
import Json.Decode exposing (Decoder)
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


{-| Represents the possible states of a query.
-}
type QueryResponse e a
    = Data a
    | Error e


{-| Checks if a given `Cqrs.Query.QueryResponse e a` is a `Data` variant.

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


{-| Checks if a given `Cqrs.Query.QueryResponse e a` is an `Error` variant.

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


{-| If a given `Cqrs.Query.QueryResponse e a` was unsuccessful, the reason for the failure will be returned.

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


{-| If a given `Cqrs.Query.QueryResponse e a` was successful, the data it contains will be returned.

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


{-| Decodes a given payload into a `Cqrs.Query.QueryResponse e a`.
-}
decoder : Decoder a -> Decoder e -> Decoder (QueryResponse e a)
decoder dataFn errorFn =
    let
        errorDecoder : Decoder (QueryResponse e a)
        errorDecoder =
            Json.Decode.map fail <| Json.Decode.at [ "error" ] errorFn

        successDecoder : Decoder (QueryResponse e a)
        successDecoder =
            Json.Decode.map succeed <| Json.Decode.at [ "data" ] dataFn
    in
    Json.Decode.oneOf
        [ errorDecoder
        , successDecoder
        ]


{-| Maps the `Data` variant of a given `Cqrs.Query.QueryResponse e a` and returns a `Cqrs.Query.Response b`.
-}
map : (a -> b) -> QueryResponse e a -> QueryResponse e b
map fn response =
    case response of
        Data value ->
            succeed <| fn value

        Error error ->
            fail error


{-| Maps the `Error` variant of a given `Cqrs.Query.QueryResponse e a`.
-}
mapError : (e -> f) -> QueryResponse e a -> QueryResponse f a
mapError fn response =
    case response of
        Data value ->
            succeed value

        Error error ->
            fail <| fn error


{-| Constructs the `Error` variant of a `Cqrs.Query.QueryResponse e a`.

This is useful for testing how your UI will respond to the sad path, without actually sending a query.

-}
fail : e -> QueryResponse e a
fail =
    Error


{-| Constructs the `Data` variant of a `Cqrs.Query.QueryResponse e a`.

This is useful for testing how your UI will respond to the happy path, without actually sending a query.

-}
succeed : a -> QueryResponse e a
succeed =
    Data


{-| Sends a query to the given URL and returns a `Cmd` for the given `msg` containing the parsed `Cqrs.Query.QueryResponse e a`.
The value contained within the `Cqrs.Query.QueryResponse e a`, will be the value parsed via the provided decoder.
-}
request : String -> (e -> f) -> f -> (QueryResponse f a -> msg) -> Decoder a -> Decoder e -> Cmd msg
request url errorMapper defaultError toMsg toData toError =
    RemoteData.Http.get url (fromRemoteData errorMapper defaultError >> toMsg) (decoder toData toError)


{-| Similar to `request` but a `StandardRequestConfig` can be passed in for cases where customisation of the request headers, etc are desired.
-}
requestWithConfig : StandardRequestConfig -> String -> (e -> f) -> f -> (QueryResponse f a -> msg) -> Decoder a -> Decoder e -> Cmd msg
requestWithConfig config url errorMapper defaultError toMsg toData toError =
    RemoteData.Http.getWithConfig config url (fromRemoteData errorMapper defaultError >> toMsg) (decoder toData toError)


{-| Sends a query to the given URL and returns a `Task` which will contain the parsed `Cqrs.Query.QueryResponse e a`.
The value contained within the `Cqrs.Query.QueryResponse e a`, will be the value parsed via the provided decoder.
-}
requestTask : String -> (e -> f) -> f -> Decoder a -> Decoder e -> Task () (QueryResponse f a)
requestTask url errorMapper defaultError toData toError =
    RemoteData.Http.getTask url (decoder toData toError)
        |> Task.map (fromRemoteData errorMapper defaultError)


{-| Similar to `requestTask` but a `TaskRequestConfig` can be passed in for cases where customisation of the request headers, etc are desired.
-}
requestTaskWithConfig : TaskRequestConfig -> String -> (e -> f) -> f -> Decoder a -> Decoder e -> Task () (QueryResponse f a)
requestTaskWithConfig config url errorMapper defaultError toData toError =
    RemoteData.Http.getTaskWithConfig config url (decoder toData toError)
        |> Task.map (fromRemoteData errorMapper defaultError)


{-| Converts a given `RemoteData x (Cqrs.Query.QueryResponse e a)` into a `Cqrs.Query.QueryResponse f a`.
-}
fromRemoteData : (e -> f) -> f -> RemoteData x (QueryResponse e a) -> QueryResponse f a
fromRemoteData errorHandler default response =
    RemoteData.unwrap (fail default) (mapError errorHandler) response


{-| Provides a default case for a `Cqrs.Query.QueryResponse e a` in place of the value held within the `Error` variant.
-}
withDefault : a -> QueryResponse e a -> a
withDefault default response =
    case response of
        Data value ->
            value

        Error _ ->
            default
