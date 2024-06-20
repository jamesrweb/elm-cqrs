module Unit.Helpers exposing (run, sanitise)

import Char.Extra exposing (isControl)
import Json.Decode exposing (Decoder)


run : Decoder a -> String -> Result Json.Decode.Error a
run decoder input =
    Json.Decode.decodeString decoder input


sanitise : String -> String
sanitise =
    String.filter (\c -> not <| isControl c)
        >> String.filter (\c -> c /= '"')
        >> String.filter (\c -> c /= '\\')
