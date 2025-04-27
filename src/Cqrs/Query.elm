module Cqrs.Query exposing
    ( StandardRequestConfig, TaskRequestConfig, QueryResponse
    , request, requestWithConfig, requestTask, requestTaskWithConfig
    , succeed, fail, fromResult, fromRemoteData
    , data, reason
    , map, mapError, mapBoth, unwrap, unpack, partition, merge, filter, toResult, toMaybe
    , withDefault, succeeded, failed
    , decoder
    )

{-|

@docs StandardRequestConfig, TaskRequestConfig, QueryResponse


## HTTP

@docs request, requestWithConfig, requestTask, requestTaskWithConfig


## Constructors

@docs succeed, fail, fromResult, fromRemoteData


## State

@docs data, reason


## Mappers

@docs map, mapError, mapBoth, unwrap, unpack, partition, merge, filter, toResult, toMaybe


## Helpers

@docs withDefault, succeeded, failed


## Codec

@docs decoder

-}

import Http exposing (Header)
import Json.Decode exposing (Decoder)
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
succeeded =
    toResult >> Result.Extra.isOk


{-| Checks if a given `Cqrs.Query.QueryResponse e a` is an `Error` variant.

    failed (Data []) --> False

    failed (Error "reason") --> True

-}
failed : QueryResponse e a -> Bool
failed =
    toResult >> Result.Extra.isErr


{-| If a given `Cqrs.Query.QueryResponse e a` was unsuccessful, the reason for the failure will be returned.

    reason (Data []) --> Nothing

    reason (Error "reason") --> Just "reason"

-}
reason : QueryResponse e a -> Maybe e
reason =
    toResult >> Result.Extra.error


{-| If a given `Cqrs.Query.QueryResponse e a` was successful, the data it contains will be returned.

    data (Data []) --> Just []

    data (Error "reason") --> Nothing

-}
data : QueryResponse e a -> Maybe a
data =
    toResult >> Result.toMaybe


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
map fn =
    toResult >> Result.map fn >> fromResult


{-| Maps the `Error` variant of a given `Cqrs.Query.QueryResponse e a`.
-}
mapError : (e -> f) -> QueryResponse e a -> QueryResponse f a
mapError fn =
    toResult >> Result.mapError fn >> fromResult


{-| Maps both the `Data` and the `Error` variant of a given `Cqrs.Query.QueryResponse e a`.
-}
mapBoth : (e -> f) -> (a -> b) -> QueryResponse e a -> QueryResponse f b
mapBoth errorFn dataFn =
    toResult >> Result.Extra.mapBoth errorFn dataFn >> fromResult


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
withDefault default =
    toResult >> Result.withDefault default


{-| Converts a `Cqrs.Query.QueryResponse e a` to a `Result e a`
-}
toResult : QueryResponse e a -> Result e a
toResult response =
    case response of
        Data value ->
            Result.Ok value

        Error error ->
            Result.Err error


{-| Converts a `Result e a` to a `Cqrs.Query.QueryResponse e a`
-}
fromResult : Result e a -> QueryResponse e a
fromResult result =
    Result.Extra.unpack fail succeed result


{-| Converts a `Cqrs.Query.QueryResponse e a` to a `Maybe a`
-}
toMaybe : QueryResponse e a -> Maybe a
toMaybe =
    toResult >> Result.toMaybe


{-| When a given `Cqrs.Query.QueryResponse e a` is actually in the form `Cqrs.Query.QueryResponse a a` then we can return the underlying value from either state. This is especially useful when the error and value have been mapped to domain types.
-}
merge : QueryResponse a a -> a
merge =
    toResult >> Result.Extra.merge


{-| Partitions a series of `QueryResponse e a` instances into a tuple of successful and unsuccessful responses.
-}
partition : List (QueryResponse e a) -> ( List a, List e )
partition =
    List.map toResult >> Result.Extra.partition


{-| Convert a `QueryResponse e a` to an `a` by applying either the first function if the `QueryResponse e a` is an `Error` variant or the second function if the `QueryResponse e a` is a `Data` variant.
-}
unpack : (e -> b) -> (a -> b) -> QueryResponse e a -> b
unpack errorFn dataFn =
    toResult >> Result.Extra.unpack errorFn dataFn


{-| Convert a `QueryResponse e a` to an `a` by applying a function if the `QueryResponse e a` is a `Data` variant or using the provided default value if it is an `Error` variant.
-}
unwrap : b -> (a -> b) -> QueryResponse e a -> b
unwrap default dataFn =
    toResult >> Result.Extra.unwrap default dataFn


{-| Filter a given `QueryResponse e a` if it is in the `Data` variant and should the predicate not pass, map to a `Error` variant with the given error. Useful for error value mapping within a given domain.
-}
filter : e -> (a -> Bool) -> QueryResponse e a -> QueryResponse e a
filter errorMessage predicate =
    toResult >> Result.Extra.filter errorMessage predicate >> fromResult
