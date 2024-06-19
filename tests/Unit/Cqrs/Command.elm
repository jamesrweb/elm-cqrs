module Unit.Cqrs.Command exposing (suite)

import Cqrs.Command as Command exposing (Response)
import Expect
import Json.Decode
import Test exposing (Test)
import Unit.Helpers as Helpers


suite : Test
suite =
    Test.describe "Cqrs.Command"
        [ Test.describe "Cqrs.Command.decode"
            [ Test.test "It decodes to a `Ok Succeeded` when the response has `succeeded` set to `true` and no `error` is present" <|
                \_ ->
                    let
                        decoded : Result Json.Decode.Error (Response String ())
                        decoded =
                            decode """{ "success" : true, "data": null }"""
                    in
                    Expect.equal decoded (Ok Command.succeed)
            , Test.test "It decodes to a `Ok (Failed String)` when the response has `succeeded` set to `false` and an `error` is present" <|
                \_ ->
                    let
                        decoded : Result Json.Decode.Error (Response String ())
                        decoded =
                            decode """{ "success" : false, "error": "reason" }"""
                    in
                    Expect.equal decoded (Ok <| Command.fail "reason")
            , Test.test "It decodes to a `Failed` when an `error` is present, even if the response data says `successful` was `true`" <|
                \_ ->
                    let
                        decoded : Result Json.Decode.Error (Response String ())
                        decoded =
                            decode """{ "success" : true, "error": "reason" }"""
                    in
                    Expect.equal decoded (Ok <| Command.fail "reason")
            , Test.test "It decodes to an `Err Json.Decode.Error` when `success` is false and `error` is not present" <|
                \_ ->
                    let
                        decoded : Result Json.Decode.Error (Response String ())
                        decoded =
                            decode """{ "success" : false, "data": null }"""
                    in
                    Expect.equal (Helpers.success decoded) False
            ]
        , Test.describe "Cqrs.Command.succeeded"
            [ Test.test "It returns `True` when passed a `Succeeded` variant" <|
                \_ ->
                    Expect.equal (Command.succeeded Command.succeed) True
            , Test.test "It returns `False` when passed a `Failed` variant" <|
                \_ ->
                    Expect.equal (Command.succeeded <| Command.fail "reason") False
            ]
        , Test.describe "Cqrs.Command.failed"
            [ Test.test "It returns `False` when passed a `Succeeded` variant" <|
                \_ ->
                    Expect.equal (Command.failed Command.succeed) False
            , Test.test "It returns `True` when passed a `Failed` variant" <|
                \_ ->
                    Expect.equal (Command.failed <| Command.fail "reason") True
            ]
        , Test.describe "Cqrs.Command.reason"
            [ Test.test "It returns `Nothing` when passed a `Succeeded` variant" <|
                \_ ->
                    Expect.equal (Command.reason Command.succeed) Nothing
            , Test.test "It returns `Just String` when passed a `Failed` variant" <|
                \_ ->
                    Expect.equal (Command.reason <| Command.fail "reason") (Just "reason")
            ]
        , Test.describe "Cqrs.Command.succeed"
            [ Test.test "It constructs a `Succeeded` variant of a `Cqrs.Command.Response` when called" <|
                \_ ->
                    Expect.equal (Command.succeeded Command.succeed) True
            ]
        , Test.describe "Cqrs.Command.fail"
            [ Test.test "It constructs a `Failed` variant of a `Cqrs.Command.Response` when called" <|
                \_ ->
                    Expect.equal (Command.succeeded <| Command.fail "reason") False
            ]
        , Test.describe "Cqrs.Command.map"
            [ Test.test "It returns with the altered value when given a `Succeeded` variant" <|
                \_ ->
                    Expect.equal (Command.succeed |> Command.map (always "mapped") |> Command.unwrap) "mapped"
            , Test.test "It does not map when given a `Failed` variant" <|
                \_ ->
                    Expect.equal (Command.fail "reason" |> Command.map (always "mapped") |> Command.reason) (Just "reason")
            ]
        , Test.describe "Cqrs.Command.mapError"
            [ Test.test "It returns with the altered value when given a `Failed` variant" <|
                \_ ->
                    Expect.equal (Command.fail "reason" |> Command.mapError List.singleton |> Command.reason) (Just [ "reason" ])
            , Test.test "It returns with the same value when given a `Failed` variant and the `identity` function" <|
                \_ ->
                    Expect.equal (Command.fail "reason" |> Command.mapError identity |> Command.reason) (Just "reason")
            , Test.test "It retains it's state when passed a `Succeeded` variant" <|
                \_ ->
                    Expect.equal (Command.succeed |> Command.mapError identity |> Command.reason) Nothing
            ]
        , Test.describe "Cqrs.Command.withDefault"
            [ Test.test "It returns with the error value when given a `Failed` variant" <|
                \_ ->
                    Expect.equal (Command.fail "reason" |> Command.withDefault "test") "reason"
            , Test.test "It returns with the error value when given a mapped `Failed` variant" <|
                \_ ->
                    Expect.equal (Command.fail "reason" |> Command.mapError String.toUpper |> Command.withDefault "test") "REASON"
            , Test.test "It returns with the default value when given a `Succeeded` variant" <|
                \_ ->
                    Expect.equal (Command.succeed |> Command.withDefault "test") "test"
            ]
        , Test.describe "Cqrs.Command.unwrap"
            [ Test.test "It unwraps a `Failed` variant to it's inner value" <|
                \_ ->
                    Expect.equal (Command.fail "reason" |> Command.map (always "value") |> Command.unwrap) "reason"
            , Test.test "It unwraps a `Succeeded` variant to it's inner value when mapped" <|
                \_ ->
                    Expect.equal (Command.succeed |> Command.map (always "value") |> Command.unwrap) "value"
            , Test.test "It unwraps a `Succeeded` variant to it's inner value when the error is mapped to match the default case" <|
                \_ ->
                    Expect.equal (Command.succeed |> Command.mapError (always ()) |> Command.unwrap) ()
            ]
        ]


decode : String -> Result Json.Decode.Error (Response String ())
decode =
    Helpers.run Command.decoder
