module Interpreter exposing (..)

import Debug exposing (..)
import IntDict exposing (..)
import List exposing (..)
import Maybe exposing (..)
import Parser exposing (..)
import TortoiseParser exposing (..)
import Tuple exposing (..)


type ControlToken
    = Repeat
        { iter : Int
        , return : Int
        }


type StateControl
    = Error Parser.Error
    | StateControl
        { code : IntDict Token
        , pc : Int
        , stack : List ControlToken
        }


type alias TortoiseLine =
    { x1 : Int
    , y1 : Int
    , x2 : Int
    , y2 : Int
    , color : { r : Int, g : Int, b : Int }
    }


type alias TortoiseWorld =
    { worldDimensions : ( Int, Int )
    , position : ( Int, Int )
    , heading : Int
    , pendown : Bool
    , color : { r : Int, g : Int, b : Int }
    , lines : List TortoiseLine
    }


type alias State =
    { stateControl : StateControl
    , tortoiseWorld : TortoiseWorld
    }


defaultTortoiseWorld : TortoiseWorld
defaultTortoiseWorld =
    TortoiseWorld ( 400, 400 ) ( 0, 0 ) 0 False { r = 0, g = 0, b = 0 } []


initialize : String -> State
initialize code =
    let
        parseResult =
            run
                tortoiseParser
                code
    in
    case parseResult of
        Ok tokenList ->
            let
                indices =
                    List.range 0 (List.length tokenList)

                enumerated =
                    List.map2 (,) indices tokenList

                indexed =
                    IntDict.fromList enumerated
            in
            State
                (StateControl { code = indexed, pc = 0, stack = [] })
                defaultTortoiseWorld

        Err parserError ->
            State
                (Error parserError)
                defaultTortoiseWorld


stepCommand : StateControl -> StateControl
stepCommand stateControl =
    case stateControl of
        Error _ ->
            stateControl

        StateControl cl ->
            StateControl { cl | pc = cl.pc + 1 }


tortoiseDegrees : Int -> Int
tortoiseDegrees deg =
    (90 - deg) % 360


takeSteps : Int -> Int -> ( Int, Int ) -> ( Int, Int )
takeSteps steps heading oldpos =
    ( round (toFloat steps * Basics.cos (degrees (toFloat (tortoiseDegrees heading)))) + first oldpos
    , round (toFloat steps * Basics.sin (degrees (toFloat (tortoiseDegrees heading)))) + second oldpos
    )


executeCommand : State -> Result () State
executeCommand state =
    case state.stateControl of
        StateControl stateControl ->
            let
                currentCommand =
                    (Maybe.withDefault END << IntDict.get stateControl.pc) stateControl.code

                tortoiseWorld =
                    state.tortoiseWorld
            in
            case currentCommand of
                FORWARD n ->
                    let
                        newPos =
                            takeSteps n tortoiseWorld.heading tortoiseWorld.position

                        newLines =
                            case tortoiseWorld.pendown of
                                True ->
                                    tortoiseWorld.lines
                                        ++ [ TortoiseLine (first tortoiseWorld.position)
                                                (second tortoiseWorld.position)
                                                (first newPos)
                                                (second newPos)
                                                tortoiseWorld.color
                                           ]

                                False ->
                                    tortoiseWorld.lines
                    in
                    Ok (State (StateControl { stateControl | pc = stateControl.pc + 1 }) { tortoiseWorld | position = newPos, lines = newLines })

                LEFT n ->
                    Ok (State (StateControl { stateControl | pc = stateControl.pc + 1 }) { tortoiseWorld | heading = tortoiseWorld.heading - n })

                RIGHT n ->
                    Ok (State (StateControl { stateControl | pc = stateControl.pc + 1 }) { tortoiseWorld | heading = tortoiseWorld.heading + n })

                PENDOWN ->
                    Ok (State (StateControl { stateControl | pc = stateControl.pc + 1 }) { tortoiseWorld | pendown = True })

                PENUP ->
                    Ok (State (StateControl { stateControl | pc = stateControl.pc + 1 }) { tortoiseWorld | pendown = False })

                PENCOLOR r g b ->
                    Ok (State (StateControl { stateControl | pc = stateControl.pc + 1 }) { tortoiseWorld | color = { r = r, g = g, b = b } })

                REPEAT_start c ->
                    Ok (State (StateControl { stateControl | pc = stateControl.pc + 1 }) tortoiseWorld)

                REPEAT_end ->
                    Ok (State (StateControl { stateControl | pc = stateControl.pc + 1 }) tortoiseWorld)

                END ->
                    Ok (State (StateControl stateControl) tortoiseWorld)

        Error error ->
            Err ()



--repeatFolder : Token -> Result () TortoiseWorld -> Result () TortoiseWorld
--repeatFolder t result =
--    case result of
--        Ok world ->
--            executeCommand world t
--        Err () ->
--            Err ()
--replicateList : List a -> Int -> List a
--replicateList list n =
--    case n of
--        0 ->
--            []
--        n ->
--            list ++ replicateList list (n - 1)


runCommand : State -> Result () State
runCommand state =
    case state.stateControl of
        Error _ ->
            Err ()

        StateControl cl ->
            executeCommand state


isDone : State -> Bool
isDone state =
    case state.stateControl of
        StateControl cl ->
            if cl.pc > (length << IntDict.toList) cl.code then
                True
            else
                False

        Error _ ->
            True
