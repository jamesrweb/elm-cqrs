module Unit.Http.Error exposing (..)

import Expect
import Fuzz
import Http exposing (Error(..))
import Http.Error exposing (errorToString)
import Test exposing (Test)


suite : Test
suite =
    Test.describe "Http.Error"
        [ Test.describe "Cqrs.Command.errorToString"
            [ Test.fuzz Fuzz.string "It handles `BadUrl` correctly" <|
                \url ->
                    Expect.equal
                        (errorToString <| BadUrl url)
                        ("The URL '" ++ url ++ "' is invalid")
            , Test.test "It handles `Timeout` correctly" <|
                \_ ->
                    Expect.equal (errorToString Timeout) "The request timed out, try again"
            , Test.test "It handles `NetworkError` correctly" <|
                \_ ->
                    Expect.equal (errorToString NetworkError) "There was a network error returned, please check your network connection"
            , Test.fuzz Fuzz.int "It handles `BadStatus` correctly" <|
                \code ->
                    Expect.equal
                        (errorToString <| BadStatus code)
                        ("A bad status with code '" ++ String.fromInt code ++ "' was returned")
            , Test.fuzz Fuzz.string "It handles `BadBody` correctly" <|
                \reason ->
                    Expect.equal (errorToString <| BadBody reason) reason
            ]
        ]
