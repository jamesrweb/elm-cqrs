module Cqrs exposing
    ( Response(..), Operation(..)
    , batch
    )

{-|

@docs Response, Operation


## Actions

@docs batch

-}

import Cqrs.Command
import Cqrs.Query
import Task exposing (Task)


{-| Represents a CQRS operation.
-}
type Operation data error msg
    = Query (Cqrs.Query.QueryRequest data error msg)
    | Command (Cqrs.Command.CommandRequest error msg)


{-| Represents the responses for the batched operations.
-}
type Response error data msg
    = QueryCmd (Cmd msg)
    | QueryTask (Task () (Cqrs.Query.QueryResponse error data))
    | CommandCmd (Cmd msg)
    | CommandTask (Task () (Cqrs.Command.CommandResponse error))


{-| Batches a list of queries and commands to be executed in unison.
-}
batch : List (Operation data error msg) -> List (Response error data msg)
batch operations =
    List.map execute operations



-- INTERNAL


{-| Executes a given operation and maps to it's underlying representation.
-}
execute : Operation data error msg -> Response error data msg
execute operation =
    case operation of
        Query queryRequest ->
            executeQuery queryRequest

        Command commandRequest ->
            executeCommand commandRequest


{-| Specifically handles the execution of queries.
-}
executeQuery : Cqrs.Query.QueryRequest data error msg -> Response error data msg
executeQuery request =
    case request of
        Cqrs.Query.Request requestSettings ->
            Cqrs.Query.request requestSettings
                |> QueryCmd

        Cqrs.Query.RequestWithConfig requestWithConfigurationSettings ->
            Cqrs.Query.requestWithConfiguration requestWithConfigurationSettings
                |> QueryCmd

        Cqrs.Query.Task requestTaskSettings ->
            Cqrs.Query.requestTask requestTaskSettings
                |> QueryTask

        Cqrs.Query.TaskWithConfig requestTaskWithConfigurationSettings ->
            Cqrs.Query.requestTaskWithConfiguration requestTaskWithConfigurationSettings
                |> QueryTask


{-| Specifically handles the execution of commands.
-}
executeCommand : Cqrs.Command.CommandRequest error msg -> Response error data msg
executeCommand request =
    case request of
        Cqrs.Command.Request requestSettings ->
            Cqrs.Command.request requestSettings
                |> CommandCmd

        Cqrs.Command.RequestWithConfig requestWithConfigurationSettings ->
            Cqrs.Command.requestWithConfiguration requestWithConfigurationSettings
                |> CommandCmd

        Cqrs.Command.Task requestTaskSettings ->
            Cqrs.Command.requestTask requestTaskSettings
                |> CommandTask

        Cqrs.Command.TaskWithConfig requestTaskWithConfigurationSettings ->
            Cqrs.Command.requestTaskWithConfiguration requestTaskWithConfigurationSettings
                |> CommandTask
