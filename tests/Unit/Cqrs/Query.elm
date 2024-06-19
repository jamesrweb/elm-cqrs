module Unit.Cqrs.Query exposing (suite)

import Cqrs.Query as Query exposing (Response)
import Expect
import Json.Decode
import Test exposing (Test)
import Unit.Helpers as Helpers


suite : Test
suite =
    Test.describe "Cqrs.Query"
        [ Test.describe "Cqrs.Query.decoder"
            [ Test.test "It decodes to a `Ok (Data a)` when the response has `succeeded` set to `true` and no `error` is present" <|
                \_ ->
                    let
                        decoded : Result Json.Decode.Error (Response String (List String))
                        decoded =
                            decode """{ "success" : true, "data": [] }"""
                    in
                    Expect.equal decoded (Ok <| Query.succeed [])
            , Test.test "It decodes to a `Ok (Error String)` when the response has `succeeded` set to `false` and an `error` is present" <|
                \_ ->
                    let
                        decoded : Result Json.Decode.Error (Response String (List String))
                        decoded =
                            decode """{ "success" : false, "error": "reason" }"""
                    in
                    Expect.equal decoded (Ok <| Query.fail "reason")
            , Test.test "It decodes to an `Error` when an `error` is present, even if the response data says `successful` was `true`" <|
                \_ ->
                    let
                        decoded : Result Json.Decode.Error (Response String (List String))
                        decoded =
                            decode """{ "success" : true, "error": "reason" }"""
                    in
                    Expect.equal decoded (Ok <| Query.fail "reason")
            , Test.test "It decodes to an `Err Json.Decode.Error` when `success` is false and `error` is not present" <|
                \_ ->
                    let
                        decoded : Result Json.Decode.Error (Response String (List String))
                        decoded =
                            decode """{ "success" : false, "data": [] }"""
                    in
                    Expect.equal (Helpers.success decoded) False
            ]
        , Test.describe "Cqrs.Query.succeeded"
            [ Test.test "It returns `True` when passed a `Data` variant" <|
                \_ ->
                    Expect.equal (Query.succeeded (Query.succeed [])) True
            , Test.test "It returns `False` when passed an `Error` variant" <|
                \_ ->
                    Expect.equal (Query.succeeded <| Query.fail "reason") False
            ]
        , Test.describe "Cqrs.Query.failed"
            [ Test.test "It returns `False` when passed a `Data` variant" <|
                \_ ->
                    Expect.equal (Query.failed (Query.succeed [])) False
            , Test.test "It returns `True` when passed an `Error` variant" <|
                \_ ->
                    Expect.equal (Query.failed <| Query.fail "reason") True
            ]
        , Test.describe "Cqrs.Query.succeed"
            [ Test.test "It constructs a `Data a` variant" <|
                \_ ->
                    Expect.equal (Query.succeeded <| Query.succeed []) True
            ]
        , Test.describe "Cqrs.Query.fail"
            [ Test.test "It constructs an `Error` variant" <|
                \_ ->
                    Expect.equal (Query.succeeded <| Query.fail "reason") False
            ]
        , Test.describe "Cqrs.Query.reason"
            [ Test.test "It returns `Just` when given an `Error` variant" <|
                \_ ->
                    Expect.equal (Query.reason <| Query.fail "reason") (Just "reason")
            , Test.test "It returns `Nothing` when given a `Data` variant" <|
                \_ ->
                    Expect.equal (Query.reason <| Query.succeed []) Nothing
            ]
        , Test.describe "Cqrs.Query.data"
            [ Test.test "It returns `Just` when given a `Data` variant" <|
                \_ ->
                    Expect.equal (Query.data <| Query.succeed []) (Just [])
            , Test.test "It returns `Nothing` when given an `Error` variant" <|
                \_ ->
                    Expect.equal (Query.data <| Query.fail "reason") Nothing
            ]
        , Test.describe "Cqrs.Query.map"
            [ Test.test "It returns with the altered value when given a `Data` variant" <|
                \_ ->
                    Expect.equal (Query.succeed [] |> Query.map (\value -> value ++ [ 1 ]) |> Query.data) (Just [ 1 ])
            , Test.test "It returns with the same value when given a `Data` variant and the `identity` function" <|
                \_ ->
                    Expect.equal (Query.succeed [] |> Query.map identity |> Query.data) (Just [])
            , Test.test "It returns the error as it was when provided an `Error` variant" <|
                \_ ->
                    let
                        failure : Response String a
                        failure =
                            Query.fail "reason"
                    in
                    Expect.equal (failure |> Query.map identity |> Query.reason) (Just "reason")
            ]
        , Test.describe "Cqrs.Query.mapError"
            [ Test.test "It returns with the altered value when given a `Error` variant" <|
                \_ ->
                    Expect.equal (Query.fail "reason" |> Query.mapError String.toUpper |> Query.reason) (Just "REASON")
            , Test.test "It returns with the same value when given a `Error` variant and the `identity` function" <|
                \_ ->
                    Expect.equal (Query.fail "reason" |> Query.mapError identity |> Query.reason) (Just "reason")
            , Test.test "It returns the data as it was when provided an `Data` variant" <|
                \_ ->
                    Expect.equal (Query.succeed [] |> Query.mapError identity |> Query.data) (Just [])
            ]
        , Test.describe "Cqrs.Query.withDefault"
            [ Test.test "It returns with the default value when given a `Failed` variant" <|
                \_ ->
                    Expect.equal (Query.fail "reason" |> Query.withDefault "test") "test"
            , Test.test "It returns with the default value when given a mapped `Failed` variant" <|
                \_ ->
                    Expect.equal (Query.fail "reason" |> Query.mapError String.toUpper |> Query.withDefault "test") "test"
            , Test.test "It returns with the data value when given a `Succeeded` variant" <|
                \_ ->
                    Expect.equal (Query.succeed "value" |> Query.withDefault "test") "value"
            ]
        ]


decode : String -> Result Json.Decode.Error (Response String (List String))
decode =
    Helpers.run (Query.decoder <| Json.Decode.list Json.Decode.string)
