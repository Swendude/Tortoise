module Interpreter exposing (..)

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


initialize : String -> Result
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
                    Ok
                        (State (CommandList [] head tail) (( 400, 400 ) ( 0, 0 ) 0))



--step : State -> State
--step state =


stepCommand : CommandList -> CommandList
stepCommand commandList =
    CommandList (append commandList.before [ commandList.current ])
        (withDefault END (head commandList.after))
        (take ((-) (List.length commandList.after) 1) commandList.after)
