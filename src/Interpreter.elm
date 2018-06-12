module Interpreter exposing (..)

import Debug exposing (..)
import List exposing (..)
import Maybe exposing (..)
import Parser exposing (..)
import TortoiseParser exposing (..)
import Tuple exposing (..)


type CommandList
    = Error Parser.Error
    | CommandList
        { before : List Token
        , current : Token
        , after : List Token
        }


type alias TortoiseLine =
    { x1 : Int
    , y1 : Int
    , x2 : Int
    , y2 : Int
    , color : ( Int, Int, Int )
    }


type alias TortoiseWorld =
    { worldDimensions : ( Int, Int )
    , position : ( Int, Int )
    , heading : Int
    , pendown : Bool
    , color : ( Int, Int, Int )
    , lines : List TortoiseLine
    }


type alias State =
    { commandList : CommandList
    , tortoiseWorld : TortoiseWorld
    }


defaultTortoiseWorld : TortoiseWorld
defaultTortoiseWorld =
    TortoiseWorld ( 400, 400 ) ( 0, 0 ) 0 False ( 0, 0, 0 ) []


initialize : String -> State
initialize code =
    let
        parseResult =
            run
                tortoiseParser
                code
    in
    case parseResult of
        Ok commands ->
            case commands of
                [] ->
                    State
                        (CommandList { before = [], current = END, after = [] })
                        defaultTortoiseWorld

                head :: tail ->
                    State
                        (CommandList { before = [], current = head, after = tail })
                        defaultTortoiseWorld

        Err parserError ->
            State
                (Error parserError)
                defaultTortoiseWorld


stepCommand : CommandList -> CommandList
stepCommand commandList =
    case commandList of
        Error _ ->
            commandList

        CommandList cl ->
            CommandList
                { before = append cl.before [ cl.current ]
                , current = withDefault END (head cl.after)
                , after = drop 1 cl.after
                }


tortoiseDegrees : Int -> Int
tortoiseDegrees deg =
    (90 - deg) % 360


takeSteps : Int -> Int -> ( Int, Int ) -> ( Int, Int )
takeSteps steps heading oldpos =
    ( round (toFloat steps * Basics.cos (degrees (toFloat (tortoiseDegrees heading)))) + first oldpos
    , round (toFloat steps * Basics.sin (degrees (toFloat (tortoiseDegrees heading)))) + second oldpos
    )


executeListOfCommands : List Token -> TortoiseWorld -> Result () TortoiseWorld
executeListOfCommands code tw =
    case code of
        [] ->
            Ok tw

        first :: rest ->
            let
                result =
                    executeCommand tw first
            in
            case result of
                Ok tw ->
                    executeListOfCommands rest tw

                Err () ->
                    Err ()


executeCommand : TortoiseWorld -> Token -> Result () TortoiseWorld
executeCommand world command =
    case command of
        FORWARD n ->
            let
                newPos =
                    takeSteps n world.heading world.position

                newLines =
                    case world.pendown of
                        True ->
                            world.lines
                                ++ [ TortoiseLine (first world.position)
                                        (second world.position)
                                        (first newPos)
                                        (second newPos)
                                        world.color
                                   ]

                        False ->
                            world.lines
            in
            Ok { world | position = newPos, lines = newLines }

        LEFT n ->
            Ok { world | heading = world.heading - n }

        RIGHT n ->
            Ok { world | heading = world.heading + n }

        PENDOWN ->
            Ok { world | pendown = True }

        PENUP ->
            Ok { world | pendown = False }

        PENCOLOR r g b ->
            Ok { world | color = ( r, g, b ) }

        REPEAT c code ->
            Ok world

        END ->
            Ok world


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
