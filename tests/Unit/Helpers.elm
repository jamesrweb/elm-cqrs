module Unit.Helpers exposing (run, success)

import Json.Decode exposing (Decoder)


run : Decoder a -> String -> Result Json.Decode.Error a
run decoder input =
    Json.Decode.decodeString decoder input


success : Result a b -> Bool
success result =
    case result of
        Ok _ ->
            True

        Err _ ->
            False
