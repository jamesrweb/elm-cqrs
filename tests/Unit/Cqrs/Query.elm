module Unit.Cqrs.Query exposing (suite)

import Cqrs.Query as Query
import Expect
import Fuzz
import Json.Decode
import RemoteData
import String.Format
import Test exposing (Test)
import Unit.Helpers as Helpers


suite : Test
suite =
    Test.describe "Cqrs.Query"
        [ Test.describe "decoder"
            [ Test.test "It decodes to a `Ok Data` when the response has `succeeded` set to `true` and no `error` is present" <|
                \_ ->
                    let
                        decoded : Result Json.Decode.Error (Query.QueryResponse String (List String))
                        decoded =
                            decode """{  "data": [] }"""
                    in
                    Expect.equal (Ok <| Query.succeed []) decoded
            , Test.fuzz Fuzz.string "It decodes to a `Ok Error` when the response has `succeeded` set to `false` and an `error` is present" <|
                \reason ->
                    let
                        decoded : Result Json.Decode.Error (Query.QueryResponse String (List String))
                        decoded =
                            """{  "error": "{{ reason }}" }"""
                                |> String.Format.namedValue "reason" (Helpers.sanitise reason)
                                |> decode
                    in
                    Expect.equal (Ok <| Query.fail <| Helpers.sanitise reason) decoded
            , Test.fuzz Fuzz.string "It decodes to an `Ok Error` when an `error` is present" <|
                \reason ->
                    let
                        decoded : Result Json.Decode.Error (Query.QueryResponse String (List String))
                        decoded =
                            """{  "error": "{{ reason }}" }"""
                                |> String.Format.namedValue "reason" (Helpers.sanitise reason)
                                |> decode
                    in
                    Expect.equal (Ok <| Query.fail <| Helpers.sanitise reason) decoded
            ]
        , Test.describe "succeeded"
            [ Test.fuzz (Fuzz.list Fuzz.string) "It returns `True` when passed a `Data` variant" <|
                \data ->
                    Expect.equal True (Query.succeeded (Query.succeed data))
            , Test.fuzz Fuzz.string "It returns `False` when passed an `Error` variant" <|
                \reason ->
                    Expect.equal False (Query.succeeded <| Query.fail reason)
            ]
        , Test.describe "failed"
            [ Test.fuzz (Fuzz.list Fuzz.string) "It returns `False` when passed a `Data` variant" <|
                \data ->
                    Expect.equal False (Query.failed (Query.succeed data))
            , Test.fuzz Fuzz.string "It returns `True` when passed an `Error` variant" <|
                \reason ->
                    Expect.equal True (Query.failed <| Query.fail reason)
            ]
        , Test.describe "succeed"
            [ Test.fuzz (Fuzz.list Fuzz.string) "It constructs a `Data` variant" <|
                \data ->
                    Expect.equal True (Query.succeeded <| Query.succeed data)
            ]
        , Test.describe "fail"
            [ Test.fuzz Fuzz.string "It constructs an `Error` variant" <|
                \reason ->
                    Expect.equal False (Query.succeeded <| Query.fail reason)
            ]
        , Test.describe "reason"
            [ Test.fuzz Fuzz.string "It returns `Just` when given an `Error` variant" <|
                \reason ->
                    Expect.equal (Just reason) (Query.reason <| Query.fail reason)
            , Test.fuzz (Fuzz.list Fuzz.string) "It returns `Nothing` when given a `Data` variant" <|
                \data ->
                    Expect.equal Nothing (Query.reason <| Query.succeed data)
            ]
        , Test.describe "data"
            [ Test.fuzz (Fuzz.list Fuzz.string) "It returns `Just` when given a `Data` variant" <|
                \data ->
                    Expect.equal (Just data) (Query.data <| Query.succeed data)
            , Test.fuzz Fuzz.string "It returns `Nothing` when given an `Error` variant" <|
                \reason ->
                    Expect.equal Nothing (Query.data <| Query.fail reason)
            ]
        , Test.describe "map"
            [ Test.fuzz (Fuzz.list Fuzz.int) "It returns with the altered value when given a `Data` variant" <|
                \data ->
                    Expect.equal (Just <| data ++ [ 1 ]) (Query.succeed data |> Query.map (\value -> value ++ [ 1 ]) |> Query.data)
            , Test.fuzz (Fuzz.list Fuzz.string) "It returns with the same value when given a `Data` variant and the `identity` function" <|
                \data ->
                    Expect.equal (Query.succeed data |> Query.map identity |> Query.data) (Just data)
            , Test.fuzz Fuzz.string "It returns the error as it was when provided an `Error` variant" <|
                \reason ->
                    Expect.equal (Just reason) (Query.fail reason |> Query.map identity |> Query.reason)
            ]
        , Test.describe "mapError"
            [ Test.fuzz Fuzz.string "It returns with the altered value when given a `Error` variant" <|
                \reason ->
                    Expect.equal (Just <| String.toUpper reason) (Query.fail reason |> Query.mapError String.toUpper |> Query.reason)
            , Test.fuzz Fuzz.string "It returns with the same value when given a `Error` variant and the `identity` function" <|
                \reason ->
                    Expect.equal (Just reason) (Query.fail reason |> Query.mapError identity |> Query.reason)
            , Test.fuzz (Fuzz.list Fuzz.string) "It returns the data as it was when provided an `Data` variant" <|
                \data ->
                    Expect.equal (Just data) (Query.succeed data |> Query.mapError identity |> Query.data)
            ]
        , Test.describe "withDefault"
            [ Test.fuzz2 Fuzz.string Fuzz.string "It returns with the default value when given an `Error` variant" <|
                \reason default ->
                    Expect.equal default (Query.fail reason |> Query.withDefault default)
            , Test.fuzz2 Fuzz.string Fuzz.string "It returns with the default value when given a mapped `Error` variant" <|
                \reason default ->
                    Expect.equal default (Query.fail reason |> Query.mapError String.toUpper |> Query.withDefault default)
            , Test.fuzz2 Fuzz.string Fuzz.string "It returns with the data value when given a `Succeeded` variant" <|
                \value default ->
                    Expect.equal value (Query.succeed value |> Query.withDefault default)
            ]
        , Test.describe "fromRemoteData"
            [ Test.fuzz3 Fuzz.string Fuzz.string Fuzz.string "It returns the given default when the remote data response is in the `RemoteData.NotAsked` variant" <|
                \default error reason ->
                    Query.fromRemoteData (always error) default (RemoteData.Failure <| Query.fail reason)
                        |> Expect.equal (Query.fail default)
            , Test.fuzz3 Fuzz.string Fuzz.string Fuzz.string "It returns the given default when the remote data response is in the `RemoteData.Loading` variant" <|
                \default error reason ->
                    Query.fromRemoteData (always error) default (RemoteData.Failure <| Query.fail reason)
                        |> Expect.equal (Query.fail default)
            , Test.fuzz3 Fuzz.string Fuzz.string Fuzz.string "It returns the given default when the remote data response is in the `RemoteData.Failure` variant" <|
                \default error reason ->
                    Query.fromRemoteData (always error) default (RemoteData.Failure <| Query.fail reason)
                        |> Expect.equal (Query.fail default)
            , Test.fuzz3 Fuzz.string Fuzz.string Fuzz.int "It returns the underlying `QueryResponse e a` in the `Data` variant when the remote data response is in the `RemoteData.Success` variant and the underlying `QueryResponse e a` is in the `Data` variant" <|
                \default error data ->
                    Query.fromRemoteData (always error) default (RemoteData.Success <| Query.succeed data)
                        |> Expect.equal (Query.succeed data)
            , Test.fuzz2 Fuzz.string Fuzz.string "It returns the underlying `QueryResponse e a` in the `Error` variant when the remote data response is in the `RemoteData.Success` variant and the underlying `QueryResponse e a` is in the `Error` variant" <|
                \default reason ->
                    Query.fromRemoteData identity default (RemoteData.Success <| Query.fail reason)
                        |> Expect.equal (Query.fail reason)
            ]
        ]


decode : String -> Result Json.Decode.Error (Query.QueryResponse String (List String))
decode =
    Helpers.run (Query.decoder (Json.Decode.list Json.Decode.string) Json.Decode.string)
