module TortoiseParser exposing (..)

import Parser exposing ((|.), (|=), Parser, ignore, int, keyword, lazy, oneOf, oneOrMore, repeat, succeed, symbol, zeroOrMore)
import Parser.LanguageKit exposing (MultiComment, whitespace)


type Token
    = FORWARD Int
    | LEFT Int
    | RIGHT Int
    | PENUP
    | PENDOWN
    | PENCOLOR Int Int Int
    | REPEAT Int (List Token)
    | END


spaces : Parser ()
spaces =
    ignore (Parser.AtLeast 1) ((==) ' ')


space : Parser ()
space =
    keyword " "


tortoiseWhitespace : Char -> Bool
tortoiseWhitespace c =
    List.member c [ ' ', '\t' ]


zeroOrMoreWhitespace : Parser ()
zeroOrMoreWhitespace =
    ignore zeroOrMore tortoiseWhitespace


newLines : Parser ()
newLines =
    ignore Parser.oneOrMore ((==) '\n')


tortoiseParser : Parser (List Token)
tortoiseParser =
    succeed Basics.identity
        |= repeat oneOrMore tortoiseCommand
        |. Parser.end


tortoiseCommand : Parser Token
tortoiseCommand =
    Parser.inContext "COMMAND" <|
        oneOf
            [ forwardParser
            , leftParser
            , rightParser
            , pendownParser
            , penupParser
            , pencolorParser
            , lazy (\_ -> repeatParser)
            ]
            |. zeroOrMoreWhitespace
            |. oneOf [ newLines, Parser.end ]


repeatParser : Parser Token
repeatParser =
    Parser.inContext "REPEAT" <|
        succeed REPEAT
            |. keyword "REPEAT"
            |. space
            |= int
            |. space
            |. Parser.symbol "["
            |. newLines
            |= lazy (\_ -> Parser.repeat oneOrMore tortoiseCommand)
            |. newLines
            |. Parser.symbol "]"


forwardParser : Parser Token
forwardParser =
    Parser.inContext "FORWARD" <|
        succeed FORWARD
            |. keyword "FORWARD"
            |. space
            |= int


leftParser : Parser Token
leftParser =
    Parser.inContext "LEFT" <|
        succeed LEFT
            |. keyword "LEFT"
            |. space
            |= int


rightParser : Parser Token
rightParser =
    Parser.inContext "RIGHT" <|
        succeed RIGHT
            |. keyword "RIGHT"
            |. space
            |= int


penupParser : Parser Token
penupParser =
    Parser.inContext "PENUP" <|
        succeed PENUP
            |. keyword "PENUP"


pendownParser : Parser Token
pendownParser =
    Parser.inContext "PENDOWN" <|
        succeed PENDOWN
            |. keyword "PENDOWN"


pencolorParser : Parser Token
pencolorParser =
    Parser.inContext "PENCOLOR" <|
        succeed PENCOLOR
            |. keyword "PENCOLOR"
            |. space
            |= int
            |. space
            |= int
            |. space
            |= int



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

        REPEAT c code ->
            "REPEAT [ " ++ String.join "\n" (List.map tokenToText code) ++ " ]"

        END ->
            "END"


printContexts : List Parser.Context -> String
printContexts contexts =
    Maybe.withDefault "" (List.head (List.map printContext contexts))


printContext : Parser.Context -> String
printContext c =
    "( line: " ++ toString c.row ++ " ) " ++ c.description


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
