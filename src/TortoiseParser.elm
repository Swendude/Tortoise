module TortoiseParser exposing (Token(..), tortoiseParser)

import Parser exposing ((|.), (|=), Parser, Step(..), Trailing(..), chompIf, chompWhile, end, int, keyword, loop, map, oneOf, sequence, spaces, succeed)


type Token
    = FORWARD Int
    | LEFT Int
    | RIGHT Int
    | PENUP
    | PENDOWN
      -- | PENCOLOR Int Int Int
      -- | REPEAT_start Int
      -- | REPEAT_end
    | END


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
