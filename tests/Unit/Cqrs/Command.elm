module Unit.Cqrs.Command exposing (suite)

import Cqrs.Command as Command
import Expect
import Fuzz
import Json.Decode
import String.Format
import Test exposing (Test)
import Unit.Helpers as Helpers


suite : Test
suite =
    Test.describe "Cqrs.Command"
        [ Test.describe "Cqrs.Command.decode"
            [ Test.test "It decodes to a `Ok Succeeded` when the response has `succeeded` set to `true` and no `error` is present" <|
                \_ ->
                    let
                        decoded : Result Json.Decode.Error Command.Response
                        decoded =
                            decode """{ "success" : true, "data": null }"""
                    in
                    Expect.equal (Ok Command.succeed) decoded
            , Test.fuzz Fuzz.string "It decodes to a `Ok Failed` when the response has `succeeded` set to `false` and an `error` is present" <|
                \reason ->
                    let
                        decoded : Result Json.Decode.Error Command.Response
                        decoded =
                            """{ "success" : false, "error": "{{ reason }}" }"""
                                |> String.Format.namedValue "reason" (Helpers.sanitise reason)
                                |> decode
                    in
                    Expect.equal (Ok <| Command.fail <| Helpers.sanitise reason) decoded
            , Test.fuzz Fuzz.string "It decodes to a `Ok Failed` when an `error` is present, even if the response data says `successful` was `true`" <|
                \reason ->
                    let
                        decoded : Result Json.Decode.Error Command.Response
                        decoded =
                            """{ "success" : true, "error": "{{ reason }}" }"""
                                |> String.Format.namedValue "reason" (Helpers.sanitise reason)
                                |> decode
                    in
                    Expect.equal (Ok <| Command.fail <| Helpers.sanitise reason) decoded
            , Test.test "It decodes to an `Err Json.Decode.Error` when `success` is false and `error` is not present" <|
                \_ ->
                    let
                        decoded : Result Json.Decode.Error Command.Response
                        decoded =
                            decode """{ "success" : false, "data": null }"""
                    in
                    Expect.err decoded
            ]
        , Test.describe "Cqrs.Command.succeeded"
            [ Test.test "It returns `True` when passed a `Succeeded` variant" <|
                \_ ->
                    Expect.equal True (Command.succeeded Command.succeed)
            , Test.fuzz Fuzz.string "It returns `False` when passed a `Failed` variant" <|
                \reason ->
                    Expect.equal False (Command.succeeded <| Command.fail reason)
            ]
        , Test.describe "Cqrs.Command.failed"
            [ Test.test "It returns `False` when passed a `Succeeded` variant" <|
                \_ ->
                    Expect.equal False (Command.failed Command.succeed)
            , Test.fuzz Fuzz.string "It returns `True` when passed a `Failed` variant" <|
                \reason ->
                    Expect.equal True (Command.failed <| Command.fail reason)
            ]
        , Test.describe "Cqrs.Command.reason"
            [ Test.test "It returns `Nothing` when passed a `Succeeded` variant" <|
                \_ ->
                    Expect.equal Nothing (Command.reason Command.succeed)
            , Test.fuzz Fuzz.string "It returns `Just String` when passed a `Failed` variant" <|
                \reason ->
                    Expect.equal (Just reason) (Command.reason <| Command.fail reason)
            ]
        , Test.describe "Cqrs.Command.succeed"
            [ Test.test "It constructs a `Succeeded` variant of a `Cqrs.Command.Response` when called" <|
                \_ ->
                    Expect.equal True (Command.succeeded Command.succeed)
            ]
        , Test.describe "Cqrs.Command.fail"
            [ Test.fuzz Fuzz.string "It constructs a `Failed` variant of a `Cqrs.Command.Response` when called" <|
                \reason ->
                    Expect.equal True (Command.failed <| Command.fail reason)
            ]
        , Test.describe "Cqrs.Command.map"
            [ Test.fuzz Fuzz.string "It returns with the altered value when given a `Succeeded` variant" <|
                \content ->
                    Expect.equal content (Command.succeed |> Command.map (always content) |> Command.unwrap)
            , Test.fuzz2 Fuzz.string Fuzz.string "It does not map when given a `Failed` variant" <|
                \reason content ->
                    Expect.equal (Just reason) (Command.fail reason |> Command.map (always content) |> Command.reason)
            ]
        , Test.describe "Cqrs.Command.mapError"
            [ Test.fuzz Fuzz.string "It returns with the altered value when given a `Failed` variant" <|
                \reason ->
                    Expect.equal (Just [ reason ]) (Command.fail reason |> Command.mapError List.singleton |> Command.reason)
            , Test.fuzz Fuzz.string "It returns with the same value when given a `Failed` variant and the `identity` function" <|
                \reason ->
                    Expect.equal (Command.fail reason |> Command.mapError identity |> Command.reason) (Just reason)
            , Test.test "It retains it's state when passed a `Succeeded` variant" <|
                \_ ->
                    Expect.equal Nothing (Command.succeed |> Command.mapError identity |> Command.reason)
            ]
        , Test.describe "Cqrs.Command.withDefault"
            [ Test.fuzz2 Fuzz.string Fuzz.string "It returns with the error value when given a `Failed` variant" <|
                \reason content ->
                    Expect.equal reason (Command.fail reason |> Command.withDefault content)
            , Test.fuzz2 Fuzz.string Fuzz.string "It returns with the error value when given a mapped `Failed` variant" <|
                \reason content ->
                    Expect.equal (String.toUpper reason) (Command.fail reason |> Command.mapError String.toUpper |> Command.withDefault content)
            , Test.fuzz Fuzz.string "It returns with the default value when given a `Succeeded` variant" <|
                \content ->
                    Expect.equal content (Command.succeed |> Command.withDefault content)
            ]
        , Test.describe "Cqrs.Command.unwrap"
            [ Test.fuzz2 Fuzz.string Fuzz.string "It unwraps a `Failed` variant to it's inner value" <|
                \reason content ->
                    Expect.equal reason (Command.fail reason |> Command.map (always content) |> Command.unwrap)
            , Test.fuzz Fuzz.string "It unwraps a `Succeeded` variant to it's inner value when mapped" <|
                \content ->
                    Expect.equal content (Command.succeed |> Command.map (always content) |> Command.unwrap)
            , Test.fuzz Fuzz.unit "It unwraps a `Succeeded` variant to it's inner value when the error is mapped to match the default case" <|
                \unit ->
                    Expect.equal unit (Command.succeed |> Command.mapError (always unit) |> Command.unwrap)
            ]
        ]


decode : String -> Result Json.Decode.Error Command.Response
decode =
    Helpers.run Command.decoder
