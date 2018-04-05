module Interpreter exposing (..)

import Debug exposing (..)
import List exposing (..)
import Maybe exposing (..)
import Parser exposing (..)
import TortoiseParser exposing (..)


type CommandList
    = Error Parser.Error
    | CommandList
        { before : List Token
        , current : Token
        , after : List Token
        }


type alias TortoiseWorld =
    { worldDimensions : ( Int, Int )
    , position : ( Int, Int )
    , heading : Int
    }


type alias State =
    { commandList : CommandList
    , tortoiseWorld : TortoiseWorld
    }


initialize : String -> Result () State
initialize code =
    let
        parseResult =
            run tortoiseParser code
    in
    case parseResult of
        Ok commands ->
            case commands of
                [] ->
                    Err ()

                head :: tail ->
                    Debug.log
                        (String.join
                            "\n"
                            (printTokens (head :: tail))
                        )
                        Ok
                        (State
                            (CommandList { before = [], current = head, after = tail })
                            (TortoiseWorld ( 400, 400 ) ( 0, 0 ) 0)
                        )

        Err _ ->
            Debug.log "ERROR IN CODE"
                Err
                ()


stepCommand : CommandList -> CommandList
stepCommand commandList =
    case commandList of
        Error _ ->
            commandList

        CommandList cl ->
            CommandList
                { before = append cl.before [ cl.current ]
                , current = withDefault END (head cl.after)
                , after = take ((-) (List.length cl.after) 1) cl.after
                }


executeCommand : TortoiseWorld -> Token -> Result () TortoiseWorld
executeCommand world command =
    case command of
        a ->
            Debug.log (TortoiseParser.tokenToText a)
                Ok
                world


runCommand : State -> Result () State
runCommand state =
    case state.commandList of
        Error _ ->
            Err ()

        CommandList cl ->
            let
                runResult =
                    executeCommand state.tortoiseWorld cl.current
            in
            case runResult of
                Ok resultWorld ->
                    Ok (State (stepCommand state.commandList) resultWorld)

                Err _ ->
                    Err ()


isDone : State -> Bool
isDone state =
    case state.commandList of
        CommandList cl ->
            case cl.current of
                END ->
                    True

                _ ->
                    False

        Error _ ->
            True
