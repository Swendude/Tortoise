module Utils.Stringifiers exposing (deadEndsToString, tokensToString)

import String exposing (fromInt)
import TortoiseParser exposing (TrtDeadend, TrtToken(..), problemToString)


tokensToString : List TrtToken -> String
tokensToString tokens =
    String.concat (List.intersperse "; " (List.map tokenToString tokens))


tokenToString : TrtToken -> String
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


deadEndsToString : List TrtDeadend -> String
deadEndsToString deadEnds =
    String.concat (List.intersperse "; " (List.map deadEndToString deadEnds))


deadEndToString : TrtDeadend -> String
deadEndToString deadend =
    problemToString deadend.problem ++ " at row " ++ String.fromInt deadend.row ++ ", col " ++ String.fromInt deadend.col
