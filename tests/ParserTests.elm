module ParserTests exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Parser exposing (run)
import Test exposing (..)
import TortoiseParser exposing (tortoiseParser)


isOk : Result error value -> Bool
isOk res =
    case res of
        Ok _ ->
            True

        Err _ ->
            False


parserSuite : Test
parserSuite =
    describe "The Parser module"
        [ describe "Test the parser for the validity Tortoise programs regarding whitespace"
            [ test "Allows simple programs" <|
                \_ ->
                    let
                        input =
                            "FORWARD 10\nLEFT 90\nFORWARD 10"
                    in
                    Expect.true "Expect the result to be Ok" (isOk (run tortoiseParser input))
            , test "Allow arbitrary spaces after command" <|
                \_ ->
                    let
                        input =
                            "FORWARD 10  \nLEFT 90          \nLEFT 40\nFORWARD 20\nFORWARD 80"
                    in
                    Expect.true "Expect the result to be Ok" (isOk (run tortoiseParser input))
            , test "Don't allow arbitrary spaces before command" <|
                \_ ->
                    let
                        input =
                            "    FORWARD 10\nLEFT 90\nFORWARD 10"
                    in
                    Expect.false "Expect the result to be Err" (isOk (run tortoiseParser input))
            , test "Allow arbitrary tabs after command" <|
                \_ ->
                    let
                        input =
                            "FORWARD 10\t\t\t\nLEFT 90\nFORWARD 10"
                    in
                    Expect.true "Expect the result to be Ok" (isOk (run tortoiseParser input))
            ]
        ]
