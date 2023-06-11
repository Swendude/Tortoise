module Utils.AST exposing (Token(..))


type Token
    = FORWARD Int
    | LEFT Int
    | RIGHT Int
    | PENUP
    | PENDOWN
    | REPEAT_start Int
    | REPEAT_end
