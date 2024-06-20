module Unit.Cqrs.Query exposing (suite)

import Cqrs.Query as Query
import Expect
import Fuzz
import Json.Decode
import String.Format
import Test exposing (Test)
import Unit.Helpers as Helpers


suite : Test
suite =
    Test.describe "Cqrs.Query"
        [ Test.describe "Cqrs.Query.decoder"
            [ Test.test "It decodes to a `Ok Data` when the response has `succeeded` set to `true` and no `error` is present" <|
                \_ ->
                    let
                        decoded : Result Json.Decode.Error (Query.Response (List String))
                        decoded =
                            decode """{ "success" : true, "data": [] }"""
                    in
                    Expect.equal (Ok <| Query.succeed []) decoded
            , Test.fuzz Fuzz.string "It decodes to a `Ok Error` when the response has `succeeded` set to `false` and an `error` is present" <|
                \reason ->
                    let
                        decoded : Result Json.Decode.Error (Query.Response (List String))
                        decoded =
                            """{ "success" : false, "error": "{{ reason }}" }"""
                                |> String.Format.namedValue "reason" (Helpers.sanitise reason)
                                |> decode
                    in
                    Expect.equal (Ok <| Query.fail <| Helpers.sanitise reason) decoded
            , Test.fuzz Fuzz.string "It decodes to an `Ok Error` when an `error` is present, even if the response data says `successful` was `true`" <|
                \reason ->
                    let
                        decoded : Result Json.Decode.Error (Query.Response (List String))
                        decoded =
                            """{ "success" : true, "error": "{{ reason }}" }"""
                                |> String.Format.namedValue "reason" (Helpers.sanitise reason)
                                |> decode
                    in
                    Expect.equal (Ok <| Query.fail <| Helpers.sanitise reason) decoded
            , Test.test "It decodes to an `Err Json.Decode.Error` when `success` is false and `error` is not present" <|
                \_ ->
                    let
                        decoded : Result Json.Decode.Error (Query.Response (List String))
                        decoded =
                            decode """{ "success" : false, "data": [] }"""
                    in
                    Expect.err decoded
            ]
        , Test.describe "Cqrs.Query.succeeded"
            [ Test.fuzz (Fuzz.list Fuzz.string) "It returns `True` when passed a `Data` variant" <|
                \data ->
                    Expect.equal True (Query.succeeded (Query.succeed data))
            , Test.fuzz Fuzz.string "It returns `False` when passed an `Error` variant" <|
                \reason ->
                    Expect.equal False (Query.succeeded <| Query.fail reason)
            ]
        , Test.describe "Cqrs.Query.failed"
            [ Test.fuzz (Fuzz.list Fuzz.string) "It returns `False` when passed a `Data` variant" <|
                \data ->
                    Expect.equal False (Query.failed (Query.succeed data))
            , Test.fuzz Fuzz.string "It returns `True` when passed an `Error` variant" <|
                \reason ->
                    Expect.equal True (Query.failed <| Query.fail reason)
            ]
        , Test.describe "Cqrs.Query.succeed"
            [ Test.fuzz (Fuzz.list Fuzz.string) "It constructs a `Data` variant" <|
                \data ->
                    Expect.equal True (Query.succeeded <| Query.succeed data)
            ]
        , Test.describe "Cqrs.Query.fail"
            [ Test.fuzz Fuzz.string "It constructs an `Error` variant" <|
                \reason ->
                    Expect.equal False (Query.succeeded <| Query.fail reason)
            ]
        , Test.describe "Cqrs.Query.reason"
            [ Test.fuzz Fuzz.string "It returns `Just` when given an `Error` variant" <|
                \reason ->
                    Expect.equal (Just reason) (Query.reason <| Query.fail reason)
            , Test.fuzz (Fuzz.list Fuzz.string) "It returns `Nothing` when given a `Data` variant" <|
                \data ->
                    Expect.equal Nothing (Query.reason <| Query.succeed data)
            ]
        , Test.describe "Cqrs.Query.data"
            [ Test.fuzz (Fuzz.list Fuzz.string) "It returns `Just` when given a `Data` variant" <|
                \data ->
                    Expect.equal (Just data) (Query.data <| Query.succeed data)
            , Test.fuzz Fuzz.string "It returns `Nothing` when given an `Error` variant" <|
                \reason ->
                    Expect.equal Nothing (Query.data <| Query.fail reason)
            ]
        , Test.describe "Cqrs.Query.map"
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
        , Test.describe "Cqrs.Query.mapError"
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
        , Test.describe "Cqrs.Query.withDefault"
            [ Test.fuzz2 Fuzz.string Fuzz.string "It returns with the default value when given a `Failed` variant" <|
                \reason default ->
                    Expect.equal default (Query.fail reason |> Query.withDefault default)
            , Test.fuzz2 Fuzz.string Fuzz.string "It returns with the default value when given a mapped `Failed` variant" <|
                \reason default ->
                    Expect.equal default (Query.fail reason |> Query.mapError String.toUpper |> Query.withDefault default)
            , Test.fuzz2 Fuzz.string Fuzz.string "It returns with the data value when given a `Succeeded` variant" <|
                \value default ->
                    Expect.equal value (Query.succeed value |> Query.withDefault default)
            ]
        ]


decode : String -> Result Json.Decode.Error (Query.Response (List String))
decode =
    Helpers.run (Query.decoder <| Json.Decode.list Json.Decode.string)
