module TortoiseParser exposing (parse, tortoiseParser)

import Parser exposing ((|.), (|=), Parser, Step(..), Trailing(..), chompIf, chompWhile, end, int, keyword, loop, map, oneOf, run, sequence, spaces, succeed)
import Utils.AST exposing (Token(..))
import Utils.Stringifiers exposing (deadEndsToString)


parse : String -> Result String (List Token)
parse inp =
    Result.mapError deadEndsToString <| run tortoiseParser inp


tortoiseParser : Parser (List Token)
tortoiseParser =
    statements


statements : Parser (List Token)
statements =
    loop [] statementsHelp


statementsHelp revStmts =
    oneOf
        [ succeed (\stmt -> Loop (stmt :: revStmts))
            |. multipleSpaces
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
            |. oneOf [ newLine, end ]

        -- allow empty lines
        , succeed (Loop revStmts)
            |. multipleSpaces
            |. newLine
        , succeed () |> map (\_ -> Done (List.reverse revStmts))
        ]


multipleSpaces : Parser ()
multipleSpaces =
    chompWhile (\c -> c == ' ')


newLine : Parser ()
newLine =
    chompIf (\c -> c == '\n')


singleArgCommand : String -> (Int -> Token) -> Parser Token
singleArgCommand identifier constr =
    succeed constr
        |. keyword identifier
        |. multipleSpaces
        |= int


noArgCommand : String -> Token -> Parser Token
noArgCommand identifier constr =
    succeed constr
        |. keyword identifier
        |. multipleSpaces
