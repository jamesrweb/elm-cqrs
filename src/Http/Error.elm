module Http.Error exposing (errorToString)

import Http exposing (Error(..))


errorToString : Http.Error -> String
errorToString error =
    case error of
        BadUrl url ->
            "The URL '" ++ url ++ "' is invalid"

        Timeout ->
            "The request timed out, try again"

        NetworkError ->
            "There was a network error returned, please check your network connection"

        BadStatus code ->
            "A bad status with code '" ++ String.fromInt code ++ "' was returned"

        BadBody errorMessage ->
            errorMessage
