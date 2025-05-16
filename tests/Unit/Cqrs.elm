module Unit.Cqrs exposing (Msg(..), suite)

import Cqrs exposing (Operation(..))
import Cqrs.Command
import Cqrs.Query
import Expect
import Json.Decode
import Json.Encode
import Test exposing (Test)


type Msg
    = QueryResponse (Cqrs.Query.QueryResponse String Int)
    | CommandResponse (Cqrs.Command.CommandResponse String)


suite : Test
suite =
    Test.describe "Cqrs"
        [ Test.describe "Cqrs.batch"
            [ Test.test "Cqrs.Query" <|
                \_ ->
                    Cqrs.batch
                        [ Query
                            (Cqrs.Query.Request
                                { defaultError = "default"
                                , toData = Json.Decode.int
                                , toError = Json.Decode.string
                                , toMsg = QueryResponse
                                , url = "/a/b/c"
                                }
                            )
                        , Query
                            (Cqrs.Query.RequestWithConfig
                                { config =
                                    { headers = []
                                    , timeout = Nothing
                                    , tracker = Nothing
                                    , risky = True
                                    }
                                , defaultError = "default"
                                , toData = Json.Decode.int
                                , toError = Json.Decode.string
                                , toMsg = QueryResponse
                                , url = "/a/b/c"
                                }
                            )
                        , Query
                            (Cqrs.Query.Task
                                { defaultError = "default"
                                , toData = Json.Decode.int
                                , toError = Json.Decode.string
                                , url = "/a/b/c"
                                }
                            )
                        , Query
                            (Cqrs.Query.TaskWithConfig
                                { config =
                                    { headers = []
                                    , timeout = Nothing
                                    , risky = True
                                    }
                                , defaultError = "default"
                                , toData = Json.Decode.int
                                , toError = Json.Decode.string
                                , url = "/a/b/c"
                                }
                            )
                        ]
                        |> List.indexedMap
                            (\index response ->
                                if index == 0 then
                                    isQueryCmd response

                                else if index == 1 then
                                    isQueryCmd response

                                else
                                    isQueryTask response
                            )
                        |> Expect.equalLists [ True, True, True, True ]
            , Test.test "Cqrs.Command" <|
                \_ ->
                    Cqrs.batch
                        [ Command
                            (Cqrs.Command.Request
                                { body = Json.Encode.string "test"
                                , defaultError = "default"
                                , toError = Json.Decode.string
                                , toMsg = CommandResponse
                                , url = "/a/b/c"
                                }
                            )
                        , Command
                            (Cqrs.Command.RequestWithConfig
                                { body = Json.Encode.string "test"
                                , config =
                                    { headers = []
                                    , timeout = Nothing
                                    , tracker = Nothing
                                    , risky = True
                                    }
                                , defaultError = "default"
                                , toError = Json.Decode.string
                                , toMsg = CommandResponse
                                , url = "/a/b/c"
                                }
                            )
                        , Command
                            (Cqrs.Command.Task
                                { body = Json.Encode.string "test"
                                , defaultError = "default"
                                , toError = Json.Decode.string
                                , url = "/a/b/c"
                                }
                            )
                        , Command
                            (Cqrs.Command.TaskWithConfig
                                { body = Json.Encode.string "test"
                                , config =
                                    { headers = []
                                    , timeout = Nothing
                                    , risky = True
                                    }
                                , defaultError = "default"
                                , toError = Json.Decode.string
                                , url = "/a/b/c"
                                }
                            )
                        ]
                        |> List.indexedMap
                            (\index response ->
                                if index == 0 then
                                    isCommandCmd response

                                else if index == 1 then
                                    isCommandCmd response

                                else
                                    isCommandTask response
                            )
                        |> Expect.equalLists [ True, True, True, True ]
            ]
        ]


isQueryCmd : Cqrs.Response error data msg -> Bool
isQueryCmd response =
    case response of
        Cqrs.QueryCmd _ ->
            True

        _ ->
            False


isQueryTask : Cqrs.Response error data msg -> Bool
isQueryTask response =
    case response of
        Cqrs.QueryTask _ ->
            True

        _ ->
            False


isCommandCmd : Cqrs.Response error data msg -> Bool
isCommandCmd response =
    case response of
        Cqrs.CommandCmd _ ->
            True

        _ ->
            False


isCommandTask : Cqrs.Response error data msg -> Bool
isCommandTask response =
    case response of
        Cqrs.CommandTask _ ->
            True

        _ ->
            False
