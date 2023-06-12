module ParserTests exposing (..)

import Expect exposing (Expectation)
import Parser.Advanced exposing (run)
import Test exposing (..)
import TortoiseParser exposing (TrtToken(..), tortoiseParser)
import Utils.Stringifiers exposing (deadEndsToString)


suite : Test
suite =
    describe "The Parser" <|
        [ describe "Single commands"
            [ test "Parses single FORWARD command" <|
                \_ -> testScript "FORWARD 10" [ FORWARD 10 ]
            , test "Parses single FORWARD command with multiple spaces" <|
                \_ -> testScript "FORWARD      10" [ FORWARD 10 ]
            , test "Parses single LEFT command" <|
                \_ -> testScript "LEFT 45" [ LEFT 45 ]
            , test "Parses single LEFT command with trailing spaces" <|
                \_ -> testScript "LEFT 70    " [ LEFT 70 ]
            , test "Parses single RIGHT command" <|
                \_ -> testScript "RIGHT 45" [ RIGHT 45 ]
            , test "Parses single PENUP command with whitspace" <|
                \_ -> testScript "   PENUP " [ PENUP ]
            , test "Parses single PENDOWN command" <|
                \_ -> testScript "PENDOWN" [ PENDOWN ]
            ]
        , describe "SCRIPTS"
            [ test "Parses script" <|
                \_ -> testScript "FORWARD 10\nLEFT 20" [ FORWARD 10, LEFT 20 ]
            , test "Parses script with whitespace " <|
                \_ -> testScript "  FORWARD 10  \n FORWARD 20   \n\n LEFT 25 " [ FORWARD 10, FORWARD 20, LEFT 25 ]
            , test "Fails on gibberish " <|
                \_ -> Expect.err <| run tortoiseParser "lfi"
            , test "Fails on unknown " <|
                \_ -> Expect.err <| run tortoiseParser "FORWARD 10\nFORWARD 20\nLEFT 25\nblooob"
            , test "Succeeds on trailing whitespace" <|
                \_ -> testScript "  FORWARD 10\nFORWARD 20\nLEFT 25\n  " [ FORWARD 10, FORWARD 20, LEFT 25 ]
            ]
        ]


testScript : String -> List TrtToken -> Expectation
testScript inp exp =
    let
        parsed =
            run tortoiseParser inp
    in
    case parsed of
        Ok result ->
            Expect.equal result exp

        Err des ->
            Expect.fail <| deadEndsToString des
