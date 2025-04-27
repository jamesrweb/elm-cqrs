module Unit.Cqrs.Command exposing (suite)

import Cqrs.Command as Command
import Expect
import Fuzz
import Json.Decode
import Maybe.Extra
import RemoteData
import Result.Extra
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
                        decoded : Result Json.Decode.Error (Command.CommandResponse String)
                        decoded =
                            decode """{  "data": null }"""
                    in
                    Expect.equal (Ok Command.succeed) decoded
            , Test.fuzz Fuzz.string "It decodes to a `Ok Failed` when the response has `succeeded` set to `false` and an `error` is present" <|
                \reason ->
                    let
                        decoded : Result Json.Decode.Error (Command.CommandResponse String)
                        decoded =
                            """{  "error": "{{ reason }}" }"""
                                |> String.Format.namedValue "reason" (Helpers.sanitise reason)
                                |> decode
                    in
                    Expect.equal (Ok <| Command.fail <| Helpers.sanitise reason) decoded
            , Test.fuzz Fuzz.string "It decodes to a `Ok Failed` when an `error` is present" <|
                \reason ->
                    let
                        decoded : Result Json.Decode.Error (Command.CommandResponse String)
                        decoded =
                            """{  "error": "{{ reason }}" }"""
                                |> String.Format.namedValue "reason" (Helpers.sanitise reason)
                                |> decode
                    in
                    Expect.equal (Ok <| Command.fail <| Helpers.sanitise reason) decoded
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
            [ Test.test "It constructs a `Succeeded` variant of a `Cqrs.Command.CommandResponse e ()` when called" <|
                \_ ->
                    Expect.equal True (Command.succeeded Command.succeed)
            ]
        , Test.describe "Cqrs.Command.fail"
            [ Test.fuzz Fuzz.string "It constructs a `Failed` variant of a `Cqrs.Command.CommandResponse e ()` when called" <|
                \reason ->
                    Expect.equal True (Command.failed <| Command.fail reason)
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
        , Test.describe "fromRemoteData"
            [ Test.fuzz3 Fuzz.string Fuzz.string Fuzz.string "It returns the given default when the remote data response is in the `RemoteData.NotAsked` variant" <|
                \default error reason ->
                    Command.fromRemoteData (always error) default (RemoteData.Failure <| Command.fail reason)
                        |> Expect.equal (Command.fail default)
            , Test.fuzz3 Fuzz.string Fuzz.string Fuzz.string "It returns the given default when the remote data response is in the `RemoteData.Loading` variant" <|
                \default error reason ->
                    Command.fromRemoteData (always error) default (RemoteData.Failure <| Command.fail reason)
                        |> Expect.equal (Command.fail default)
            , Test.fuzz3 Fuzz.string Fuzz.string Fuzz.string "It returns the given default when the remote data response is in the `RemoteData.Failure` variant" <|
                \default error reason ->
                    Command.fromRemoteData (always error) default (RemoteData.Failure <| Command.fail reason)
                        |> Expect.equal (Command.fail default)
            , Test.fuzz2 Fuzz.string Fuzz.string "It returns the underlying `CommandResponse e ()` in the `Succeeded` variant when the remote data response is in the `RemoteData.Success` variant and the underlying `CommandResponse e ()` is in the `Succeeded` variant" <|
                \default error ->
                    Command.fromRemoteData (always error) default (RemoteData.Success <| Command.succeed)
                        |> Expect.equal Command.succeed
            , Test.fuzz2 Fuzz.string Fuzz.string "It returns the underlying `CommandResponse e ()` in the `Failed` variant when the remote data response is in the `RemoteData.Success` variant and the underlying `CommandResponse e ()` is in the `Failed` variant" <|
                \default reason ->
                    Command.fromRemoteData identity default (RemoteData.Success <| Command.fail reason)
                        |> Expect.equal (Command.fail reason)
            ]
        , Test.describe "toMaybe"
            [ Test.test "Converts a `CommandResponse e` in the `Succeeded` variant to a `Just` representation." <|
                \_ ->
                    Command.succeed
                        |> Command.toMaybe
                        |> Maybe.Extra.isJust
                        |> Expect.equal True
            , Test.fuzz Fuzz.string "Converts a `CommandResponse e` in the `Failed` variant to a `Nothing` representation." <|
                \reason ->
                    Command.fail reason
                        |> Command.toMaybe
                        |> Maybe.Extra.isNothing
                        |> Expect.equal True
            ]
        , Test.describe "toResult"
            [ Test.test "Converts a `CommandResponse e` in the `Succeeded` variant to an `Ok` representation." <|
                \_ ->
                    Command.succeed
                        |> Command.toResult
                        |> Result.Extra.isOk
                        |> Expect.equal True
            , Test.fuzz Fuzz.string "Converts a `CommandResponse e` in the `Failed` variant to an `Err` representation." <|
                \reason ->
                    Command.fail reason
                        |> Command.toResult
                        |> Result.Extra.isErr
                        |> Expect.equal True
            ]
        , Test.describe "partition"
            [ Test.fuzz3 Fuzz.unit Fuzz.string Fuzz.string "Partitions a `CommandResponse e` list into a `(List (), List e)`." <|
                \data error1 error2 ->
                    [ Command.succeed, Command.fail error1, Command.fail error2 ]
                        |> Command.partition
                        |> Expect.equal ( [ data ], [ error1, error2 ] )
            ]
        , Test.describe "unpack"
            [ Test.fuzz2 Fuzz.string Fuzz.string "Maps the current variant of a given `CommandResponse e` and returns the data value when `Succeeded` is the current variant." <|
                \data error ->
                    Command.succeed
                        |> Command.unpack (always error) (always data)
                        |> Expect.equal data
            , Test.fuzz2 Fuzz.string Fuzz.string "Maps the current variant of a given `CommandResponse e` and returns the error value when `Failed` is the current variant." <|
                \data error ->
                    Command.fail error
                        |> Command.unpack identity (always data)
                        |> Expect.equal error
            ]
        , Test.describe "unwrap"
            [ Test.fuzz2 Fuzz.string Fuzz.string "Maps the current variant of a given `CommandResponse e` and returns the data value when `Succeeded` is the current variant." <|
                \data error ->
                    Command.succeed
                        |> Command.unwrap error (always data)
                        |> Expect.equal data
            , Test.fuzz2 Fuzz.string Fuzz.string "Maps the current variant of a given `CommandResponse e` and returns the error value when `Failed` is the current variant." <|
                \data error ->
                    Command.fail error
                        |> Command.unwrap error (always data)
                        |> Expect.equal error
            ]
        ]


decode : String -> Result Json.Decode.Error (Command.CommandResponse String)
decode =
    Helpers.run (Command.decoder Json.Decode.string)
