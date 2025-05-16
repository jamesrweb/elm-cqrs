module Cqrs.Query exposing
    ( StandardRequestConfiguration, TaskRequestConfiguration, RequestSettings, RequestWithConfigurationSettings, RequestTaskSettings, RequestTaskWithConfigurationSettings, QueryRequest(..), QueryResponse
    , request, requestWithConfiguration, requestTask, requestTaskWithConfiguration
    , succeed, fail, fromResult, fromRemoteData
    , data, reason
    , map, mapError, mapBoth, unwrap, unpack, partition, merge, filter, toResult, toMaybe
    , withDefault, succeeded, failed
    , decoder
    )

{-|

@docs StandardRequestConfiguration, TaskRequestConfiguration, RequestSettings, RequestWithConfigurationSettings, RequestTaskSettings, RequestTaskWithConfigurationSettings, QueryRequest, QueryResponse


## HTTP

@docs request, requestWithConfiguration, requestTask, requestTaskWithConfiguration


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
import RemoteData exposing (WebData)
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


{-| Configuration settings for a `request`
-}
type alias RequestSettings data error msg =
    { defaultError : error
    , toData : Decoder data
    , toError : Decoder error
    , toMsg : QueryResponse error data -> msg
    , url : String
    }


{-| Configuration settings for a `requestWithConfig`
-}
type alias RequestWithConfigurationSettings data error msg =
    { config : StandardRequestConfiguration
    , defaultError : error
    , toData : Decoder data
    , toError : Decoder error
    , toMsg : QueryResponse error data -> msg
    , url : String
    }


{-| Configuration settings for a `requestTask`
-}
type alias RequestTaskSettings data error =
    { defaultError : error
    , toData : Decoder data
    , toError : Decoder error
    , url : String
    }


{-| Configuration settings for a `requestTaskWithConfig`
-}
type alias RequestTaskWithConfigurationSettings data error =
    { config : TaskRequestConfiguration
    , defaultError : error
    , toData : Decoder data
    , toError : Decoder error
    , url : String
    }


{-| Represents the possible types of query which can be executed depending on your needs.
-}
type QueryRequest data error msg
    = Request (RequestSettings data error msg)
    | RequestWithConfig (RequestWithConfigurationSettings data error msg)
    | Task (RequestTaskSettings data error)
    | TaskWithConfig (RequestTaskWithConfigurationSettings data error)


{-| Represents the possible states of a query.
-}
type QueryResponse error data
    = Data data
    | Error error


{-| Checks if a given `QueryResponse error data` is a `Data` variant.

    succeeded (Data []) --> True

    succeeded (Error "reason") --> False

-}
succeeded : QueryResponse error data -> Bool
succeeded =
    toResult >> Result.Extra.isOk


{-| Checks if a given `QueryResponse error data` is an `Error` variant.

    failed (Data []) --> False

    failed (Error "reason") --> True

-}
failed : QueryResponse error data -> Bool
failed =
    toResult >> Result.Extra.isErr


{-| If a given `QueryResponse error data` was unsuccessful, the reason for the failure will be returned.

    reason (Data []) --> Nothing

    reason (Error "reason") --> Just "reason"

-}
reason : QueryResponse error data -> Maybe error
reason =
    toResult >> Result.Extra.error


{-| If a given `QueryResponse error data` was successful, the data it contains will be returned.

    data (Data []) --> Just []

    data (Error "reason") --> Nothing

-}
data : QueryResponse error data -> Maybe data
data =
    toResult >> Result.toMaybe


{-| Decodes a given payload into a `QueryResponse error data`.
-}
decoder : Decoder data -> Decoder error -> Decoder (QueryResponse error data)
decoder dataFn errorFn =
    let
        errorDecoder : Decoder (QueryResponse error data)
        errorDecoder =
            Json.Decode.map fail <| Json.Decode.at [ "error" ] errorFn

        successDecoder : Decoder (QueryResponse error data)
        successDecoder =
            Json.Decode.map succeed <| Json.Decode.at [ "data" ] dataFn
    in
    Json.Decode.oneOf
        [ errorDecoder
        , successDecoder
        ]


{-| Maps the `Data` variant of a given `QueryResponse error data`.
-}
map : (data -> nextData) -> QueryResponse error data -> QueryResponse error nextData
map fn =
    toResult >> Result.map fn >> fromResult


{-| Maps the `Error` variant of a given `QueryResponse error data`.
-}
mapError : (error -> nextError) -> QueryResponse error data -> QueryResponse nextError data
mapError fn =
    toResult >> Result.mapError fn >> fromResult


{-| Maps both the `Data` and the `Error` variant of a given `QueryResponse error data`.
-}
mapBoth : (error -> nextError) -> (data -> nextData) -> QueryResponse error data -> QueryResponse nextError nextData
mapBoth errorFn dataFn =
    toResult >> Result.Extra.mapBoth errorFn dataFn >> fromResult


{-| Constructs the `Error` variant of a `QueryResponse error data`.

This is useful for testing how your UI will respond to the sad path, without actually sending a query, for example.

-}
fail : error -> QueryResponse error data
fail =
    Error


{-| Constructs the `Data` variant of a `QueryResponse error data`.

This is useful for testing how your UI will respond to the happy path, without actually sending a query, for example.

-}
succeed : data -> QueryResponse error data
succeed =
    Data


{-| Sends a query to the given URL and returns a `Cmd` for the given `msg` containing the parsed `QueryResponse error data`.
-}
request : RequestSettings data error msg -> Cmd msg
request { defaultError, toData, toError, toMsg, url } =
    RemoteData.Http.get url (fromRemoteData defaultError >> toMsg) (decoder toData toError)


{-| Similar to `request` but a `StandardRequestConfig` can be passed in for cases where customisation of the request headers, etc are desired.
-}
requestWithConfiguration : RequestWithConfigurationSettings data error msg -> Cmd msg
requestWithConfiguration { config, defaultError, toData, toError, toMsg, url } =
    RemoteData.Http.getWithConfig config url (fromRemoteData defaultError >> toMsg) (decoder toData toError)


{-| Sends a query to the given URL and returns a `Task` which will contain the parsed `QueryResponse error data`.
-}
requestTask : RequestTaskSettings data error -> Task () (QueryResponse error data)
requestTask { defaultError, toData, toError, url } =
    RemoteData.Http.getTask url (decoder toData toError)
        |> Task.map (fromRemoteData defaultError)


{-| Similar to `requestTask` but a `TaskRequestConfig` can be passed in for cases where customisation of the request headers, etc are desired.
-}
requestTaskWithConfiguration : RequestTaskWithConfigurationSettings data error -> Task () (QueryResponse error data)
requestTaskWithConfiguration { config, defaultError, toData, toError, url } =
    RemoteData.Http.getTaskWithConfig config url (decoder toData toError)
        |> Task.map (fromRemoteData defaultError)


{-| Converts a given `WebData (QueryResponse issue data)` into a `QueryResponse error data`.
-}
fromRemoteData : error -> WebData (QueryResponse error data) -> QueryResponse error data
fromRemoteData defaultError response =
    RemoteData.unwrap (fail defaultError) identity response


{-| Provides a default case for a `QueryResponse error data` in place of the value held within the `Error` variant.
-}
withDefault : data -> QueryResponse error data -> data
withDefault default =
    toResult >> Result.withDefault default


{-| Converts a `QueryResponse error data` to a `Result error data`
-}
toResult : QueryResponse error data -> Result error data
toResult response =
    case response of
        Data value ->
            Result.Ok value

        Error error ->
            Result.Err error


{-| Converts a `Result error data` to a `QueryResponse error data`
-}
fromResult : Result error data -> QueryResponse error data
fromResult result =
    Result.Extra.unpack fail succeed result


{-| Converts a `QueryResponse error data` to a `Maybe data`
-}
toMaybe : QueryResponse error data -> Maybe data
toMaybe =
    toResult >> Result.toMaybe


{-| When a given `QueryResponse error data` is actually in the form `QueryResponse value value` then we can return the underlying value from either state. This is especially useful when the error and value have been mapped to domain types.
-}
merge : QueryResponse value value -> value
merge =
    toResult >> Result.Extra.merge


{-| Partitions a series of `QueryResponse error data` instances into a tuple of successful and unsuccessful responses.
-}
partition : List (QueryResponse error data) -> ( List data, List error )
partition =
    List.map toResult >> Result.Extra.partition


{-| Convert a `QueryResponse error data` to a `value` by applying either the first function if the `QueryResponse error data` is an `Error` variant or the second function if the `QueryResponse error data` is a `Data` variant.
-}
unpack : (error -> value) -> (data -> value) -> QueryResponse error data -> value
unpack errorFn dataFn =
    toResult >> Result.Extra.unpack errorFn dataFn


{-| Convert a `QueryResponse error data` to a `value` by applying a function if the `QueryResponse error data` is a `Data` variant or using the provided default value if it is an `Error` variant.
-}
unwrap : value -> (data -> value) -> QueryResponse error data -> value
unwrap default dataFn =
    toResult >> Result.Extra.unwrap default dataFn


{-| Filter a given `QueryResponse error data` if it is in the `Data` variant and should the predicate not pass, map to a `Error` variant with the given error. Useful for error value mapping within a given domain.
-}
filter : error -> (data -> Bool) -> QueryResponse error data -> QueryResponse error data
filter error predicate =
    toResult >> Result.Extra.filter error predicate >> fromResult
