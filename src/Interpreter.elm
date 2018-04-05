module Interpreter exposing (..)

import Debug exposing (..)
import List exposing (..)
import Maybe exposing (..)
import Parser exposing (..)
import TortoiseParser exposing (..)


type alias CommandList =
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
                        (State (CommandList [] head tail) (TortoiseWorld ( 400, 400 ) ( 0, 0 ) 0))

        Err _ ->
            Debug.log "ERROR IN CODE"
                Err
                ()


stepCommand : CommandList -> CommandList
stepCommand commandList =
    CommandList (append commandList.before [ commandList.current ])
        (withDefault END (head commandList.after))
        (take ((-) (List.length commandList.after) 1) commandList.after)


executeCommand : TortoiseWorld -> Token -> Result () TortoiseWorld
executeCommand world command =
    case command of
        a ->
            Debug.log (TortoiseParser.tokenToText a)
                Ok
                world


runCommand : State -> Result () State
runCommand state =
    let
        runResult =
            executeCommand state.tortoiseWorld state.commandList.current
    in
    case runResult of
        Ok resultWorld ->
            Ok (State (stepCommand state.commandList) resultWorld)

        Err _ ->
            Err ()


isDone : State -> Bool
isDone state =
    case state.commandList.current of
        END ->
            True

        _ ->
            False
