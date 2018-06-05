module TortoiseParser exposing (..)

import Parser exposing ((|.), (|=), Parser, ignore, int, keyword, oneOf, oneOrMore, repeat, succeed, symbol, zeroOrMore)


type Token
    = FORWARD Int
    | LEFT Int
    | RIGHT Int
    | PENUP
    | PENDOWN
    | PENCOLOR Int Int Int
    | END


space : Parser ()
space =
    ignore (Parser.AtLeast 1) ((==) ' ')


newLine : Parser ()
newLine =
    ignore Parser.oneOrMore ((==) '\n')


tortoiseParser : Parser (List Token)
tortoiseParser =
    succeed Basics.identity
        |= repeat oneOrMore move
        |. Parser.end


tortoiseWhitespace : Char -> Bool
tortoiseWhitespace c =
    List.member c [ ' ', '\t' ]


move : Parser Token
move =
    oneOf
        [ forwardParser
        , leftParser
        , rightParser
        , pendownParser
        , penupParser
        , pencolorParser
        ]
        |. ignore zeroOrMore tortoiseWhitespace
        |. oneOf [ newLine, Parser.end ]


forwardParser : Parser Token
forwardParser =
    succeed FORWARD
        |. keyword "FORWARD"
        |. space
        |= int


leftParser : Parser Token
leftParser =
    succeed LEFT
        |. keyword "LEFT"
        |. space
        |= int


rightParser : Parser Token
rightParser =
    succeed RIGHT
        |. keyword "RIGHT"
        |. space
        |= int


penupParser : Parser Token
penupParser =
    succeed PENUP
        |. keyword "PENUP"


pendownParser : Parser Token
pendownParser =
    succeed PENDOWN
        |. keyword "PENDOWN"


pencolorParser : Parser Token
pencolorParser =
    succeed PENCOLOR
        |. keyword "PENCOLOR"
        |. space
        |= int
        |. space
        |= int
        |. space
        |= int



--        |. space
--        |= int
--        |. space
--        |= int
-- DEBUG


printTokens : List Token -> List String
printTokens =
    List.map tokenToText


tokenToText : Token -> String
tokenToText token =
    case token of
        FORWARD n ->
            "FORWARD " ++ toString n

        LEFT n ->
            "LEFT " ++ toString n

        RIGHT n ->
            "RIGHT " ++ toString n

        PENUP ->
            "PENUP"

        PENDOWN ->
            "PENDOWN"

        PENCOLOR r g b ->
            "PENCOLOR " ++ toString r ++ " " ++ toString g ++ " " ++ toString b

        END ->
            "END"


printProblems : Parser.Problem -> List String
printProblems problem =
    case problem of
        Parser.BadOneOf problems ->
            List.concat (List.map printProblems problems)

        Parser.BadInt ->
            [ "Bad Integer" ]

        Parser.BadFloat ->
            [ "Bad Float" ]

        Parser.BadRepeat ->
            [ "Bad Repeat" ]

        Parser.ExpectingEnd ->
            [ "Expecting End" ]

        Parser.ExpectingSymbol symbol ->
            [ "Expecting Symbol '" ++ symbol ++ "'" ]

        Parser.ExpectingKeyword keyword ->
            [ "Expecting Keyword '" ++ keyword ++ "'" ]

        Parser.ExpectingVariable ->
            [ "Expecting Variable" ]

        Parser.ExpectingClosing closing ->
            [ "Expecting Closing" ]

        Parser.Fail fail ->
            [ "Fail '" ++ fail ++ "'" ]
