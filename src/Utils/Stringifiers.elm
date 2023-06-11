module Utils.Stringifiers exposing (deadEndsToString, tokensToString)

import Parser exposing (DeadEnd, Problem(..))
import String exposing (fromInt)
import Utils.AST exposing (Token(..))


tokensToString : List Token -> String
tokensToString tokens =
    String.concat (List.intersperse "; " (List.map tokenToString tokens))


tokenToString : Token -> String
tokenToString t =
    case t of
        LEFT n ->
            "LEFT" ++ fromInt n

        RIGHT n ->
            "RIGHT" ++ fromInt n

        PENUP ->
            "PENUP"

        PENDOWN ->
            "PENDOWN"

        REPEAT_start n ->
            "REPEAT" ++ fromInt n

        REPEAT_end ->
            "REPEAT_END"

        FORWARD n ->
            "FORWARD " ++ fromInt n


deadEndsToString : List DeadEnd -> String
deadEndsToString deadEnds =
    String.concat (List.intersperse "; " (List.map deadEndToString deadEnds))


deadEndToString : DeadEnd -> String
deadEndToString deadend =
    problemToString deadend.problem ++ " at row " ++ String.fromInt deadend.row ++ ", col " ++ String.fromInt deadend.col


problemToString : Problem -> String
problemToString p =
    case p of
        Expecting s ->
            "expecting '" ++ s ++ "'"

        ExpectingInt ->
            "expecting int"

        ExpectingHex ->
            "expecting hex"

        ExpectingOctal ->
            "expecting octal"

        ExpectingBinary ->
            "expecting binary"

        ExpectingFloat ->
            "expecting float"

        ExpectingNumber ->
            "expecting number"

        ExpectingVariable ->
            "expecting variable"

        ExpectingSymbol s ->
            "expecting symbol '" ++ s ++ "'"

        ExpectingKeyword s ->
            "expecting keyword '" ++ s ++ "'"

        ExpectingEnd ->
            "expecting end"

        UnexpectedChar ->
            "unexpected char"

        Problem s ->
            "problem " ++ s

        BadRepeat ->
            "bad repeat"
