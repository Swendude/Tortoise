module TortoiseParser exposing (TrtDeadend, TrtToken(..), parse, problemToString, tortoiseParser)

import Parser.Advanced exposing ((|.), (|=), DeadEnd, Step(..), Token(..), Trailing(..), backtrackable, chompIf, chompWhile, end, int, keyword, loop, map, oneOf, run, succeed, token)
import String exposing (fromInt)


type alias TrtDeadend =
    DeadEnd TrtContext TrtProblem


parse : String -> Result (List TrtDeadend) (List TrtToken)
parse inp =
    run tortoiseParser inp


type TrtToken
    = FORWARD Int
    | LEFT Int
    | RIGHT Int
    | PENUP
    | PENDOWN
    | REPEAT_start Int
    | REPEAT_end


type TrtContext
    = SingleArg
    | NoArg
    | Whitespace


type TrtProblem
    = CommandExpected Int
    | NumberExpected
    | NumberInvalid
    | NewLineExpected
    | EndExpected


problemToString : TrtProblem -> String
problemToString pr =
    case pr of
        NumberExpected ->
            "Expected number"

        NumberInvalid ->
            "Expected a round number"

        EndExpected ->
            "Expected end of script"

        NewLineExpected ->
            "Expected a newline"

        CommandExpected n ->
            case n of
                0 ->
                    "Expected a command with zero(0) arguments like 'PENUP' or 'PENDOWN"

                1 ->
                    "Expected a command with one argument like 'FORWARD 10' or 'LEFT 45"

                m ->
                    "Expected a command with " ++ fromInt m ++ " arguments"


type alias TrtParser target =
    Parser.Advanced.Parser TrtContext TrtProblem target


tortoiseParser : TrtParser (List TrtToken)
tortoiseParser =
    statements


statements : TrtParser (List TrtToken)
statements =
    -- succeed (\stm -> [ stm ]) |= singleArgCommand "FORWARD" FORWARD
    loop [] statementsHelp


statementsHelp revStmts =
    succeed identity
        |. multipleSpaces
        |= oneOf
            [ backtrackable <|
                succeed (\stmt -> Loop (stmt :: revStmts))
                    |= oneOf
                        [ singleArgCommand "FORWARD" FORWARD
                        , singleArgCommand "LEFT" LEFT
                        , singleArgCommand "RIGHT" RIGHT
                        , singleArgCommand "REPEAT" REPEAT_start
                        , noArgCommand "ENDREPEAT" REPEAT_end
                        , noArgCommand "PENUP" PENUP
                        , noArgCommand "PENDOWN" PENDOWN
                        ]
                    |. multipleSpaces
                    |. oneOf [ newLine, end EndExpected ]

            -- allow empty lines
            , backtrackable <|
                succeed (Loop revStmts)
                    |. newLine
                    |. multipleSpaces
            , succeed (Done (List.reverse revStmts))
                |. end EndExpected
            ]


multipleSpaces : TrtParser ()
multipleSpaces =
    chompWhile (\c -> c == ' ')


newLine : TrtParser ()
newLine =
    chompIf (\c -> c == '\n') NewLineExpected


singleArgCommand : String -> (Int -> TrtToken) -> TrtParser TrtToken
singleArgCommand identifier constr =
    succeed constr
        |. token (Token identifier <| CommandExpected 1)
        -- |. token (Token "FORWARD" <| CommandExpected 1)
        |. multipleSpaces
        |= int NumberExpected NumberInvalid


noArgCommand : String -> TrtToken -> TrtParser TrtToken
noArgCommand identifier constr =
    succeed constr |. keyword (Token identifier <| CommandExpected 0)
