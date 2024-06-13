module Unit.Http.Helpers exposing (suite)

import Expect
import Http exposing (Error(..))
import Http.Helpers exposing (errorToString)
import Test exposing (Test)


suite : Test
suite =
    Test.describe "Http.Helpers"
        [ Test.describe "errorToString"
            [ Test.test "Converts a `BadUrl url` to a string representation" <|
                \_ ->
                    Expect.equal (errorToString <| BadUrl "https://example.com") "The URL 'https://example.com' is invalid"
            , Test.test "Converts a `Timeout` to a string representation" <|
                \_ ->
                    Expect.equal (errorToString Timeout) "The request timed out, try again"
            , Test.test "Converts a `NetworkError` to a string representation" <|
                \_ ->
                    Expect.equal (errorToString NetworkError) "There was a network error returned, please check your network connection"
            , Test.test "Converts a `BadStatus code` to a string representation" <|
                \_ ->
                    Expect.equal (errorToString <| BadStatus 418) "A bad status with code '418' was returned"
            , Test.test "Converts a `BadBody errorMessage` to a string representation" <|
                \_ ->
                    Expect.equal (errorToString <| BadBody "error") "error"
            ]
        ]
