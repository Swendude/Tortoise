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


type alias TortoiseWorld =
    { worldDimensions : ( Int, Int )
    , position : ( Int, Int )
    , heading : Int
    }


type alias State =
    { commandList : CommandList
    , tortoiseWorld : TortoiseWorld
    }


initialize : String -> State
initialize code =
    let
        parseResult =
            run tortoiseParser code
    in
    case parseResult of
        Ok commands ->
            case commands of
                [] ->
                    State
                        (CommandList { before = [], current = END, after = [] })
                        (TortoiseWorld ( 400, 400 ) ( 0, 0 ) 0)

                head :: tail ->
                    State
                        (CommandList { before = [], current = head, after = tail })
                        (TortoiseWorld ( 400, 400 ) ( 0, 0 ) 0)

        Err parserError ->
            State
                (Error parserError)
                (TortoiseWorld ( 400, 400 ) ( 0, 0 ) 0)


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



--radians : Float -> Float
--radians degree =
--    degree * pi / 180


takeSteps : Int -> Int -> ( Int, Int ) -> ( Int, Int )
takeSteps steps heading oldpos =
    ( round (toFloat steps * Basics.cos (degrees (toFloat (tortoiseDegrees heading)))) + first oldpos
    , round (toFloat steps * Basics.sin (degrees (toFloat (tortoiseDegrees heading)))) + second oldpos
    )


executeCommand : TortoiseWorld -> Token -> Result () TortoiseWorld
executeCommand world command =
    case command of
        FORWARD n ->
            Ok { world | position = takeSteps n world.heading world.position }

        LEFT n ->
            Ok { world | heading = world.heading - n }

        RIGHT n ->
            Ok { world | heading = world.heading + n }

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
