module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import List exposing (append, head, length, map, tail, take)
import Parser exposing ((|.), (|=), Parser, ignore, int, keyword, oneOf, oneOrMore, repeat, succeed, symbol, zeroOrMore)
import String exposing (join, split)
import Svg exposing (..)
import Svg.Attributes exposing (..)


main : Program Never Model Msg
main =
    Html.program { init = init, view = view, update = update, subscriptions = subscription }



-- INIT + MODEL


type alias Vector =
    { x : Int, y : Int }


type alias Model =
    { heading : Int
    , position : Vector
    , line : List Vector
    , windowSize : Vector
    , input : Result Parser.Error (List Token)
    }


init : ( Model, Cmd msg )
init =
    ( Model 0 { x = 0, y = 0 } [] { x = 500, y = 400 } (Result.Ok []), Cmd.none )



-- UPDATE


type Msg
    = Rotate Int
    | Forward Int
    | Change String
    | Eval


takeSteps : Int -> Int -> Vector -> Vector
takeSteps steps heading oldpos =
    { x = round (toFloat steps * Basics.cos (degrees (toFloat heading))) + oldpos.x
    , y = round (toFloat steps * Basics.sin (degrees (toFloat heading))) + oldpos.y
    }


{-| Takes a newline seperated string and returns all lines except the last |
-}
ignoreLast : String -> String
ignoreLast input =
    let
        splitted =
            split "\n" input

        lines =
            length splitted
    in
    join "\n" (take (lines - 1) splitted)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Rotate n ->
            ( { model | heading = (model.heading + n) % 360 }, Cmd.none )

        Forward n ->
            ( { model | position = takeSteps n model.heading model.position, line = model.position :: model.line }, Cmd.none )

        Change inputString ->
            ( { model | input = Parser.run tortoiseParser inputString }, Cmd.none )

        Eval ->
            ( model, Cmd.none )



--( { model | output = Parser.run tortoiseParser (String.toUpper model.input) }, Cmd.none )
-- INTERPRETER


type Token
    = FORWARD Int
    | LEFT Int
    | RIGHT Int


space : Parser ()
space =
    ignore (Parser.Exactly 1) ((==) ' ')


newLine : Parser ()
newLine =
    ignore (Parser.Exactly 1) ((==) '\n')


tortoiseParser : Parser (List Token)
tortoiseParser =
    succeed Basics.identity
        |= repeat oneOrMore move


move : Parser Token
move =
    oneOf
        [ forwardParser
        , leftParser
        , rightParser
        ]
        |. newLine


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
        |. Parser.symbol " "
        |= int



-- DEBUG


printTokens : List Token -> String
printTokens =
    List.map tokenToText >> join ","


tokenToText : Token -> String
tokenToText token =
    case token of
        FORWARD n ->
            "FORWARD " ++ toString n

        LEFT n ->
            "LEFT " ++ toString n

        RIGHT n ->
            "RIGHT " ++ toString n



{- Should probably return list of strings -}


problemToHtml : Parser.Problem -> List (Html Msg)
problemToHtml problem =
    let
        problems =
            problemToText problem
    in
    List.map (\prob -> Html.p [] [ Html.text prob ]) problems


problemToText : Parser.Problem -> List String
problemToText problem =
    case problem of
        Parser.BadOneOf problems ->
            List.concat (List.map problemToText problems)

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



-- VIEW


htmlclass : String -> Html.Attribute msg
htmlclass =
    Html.Attributes.class


view : Model -> Html Msg
view model =
    let
        output =
            case model.input of
                Ok val ->
                    Html.p [] [ Html.text (printTokens val) ]

                Err val ->
                    Html.div []
                        [ Html.h5 [] (List.append [ Html.text "Errors: " ] (problemToHtml val.problem))
                        ]
    in
    div [ htmlclass "container" ]
        [ div [ htmlclass "row" ]
            [ div [ htmlclass "col s12" ]
                [ h4 [ htmlclass "center-align" ]
                    [ Html.text "Tortoise" ]
                ]
            , div [ htmlclass "col s12" ]
                [ p [ htmlclass "center-align" ]
                    [ Html.text "LOGO in the browser" ]
                ]
            ]
        , div [ htmlclass "row" ]
            [ div [ htmlclass "center-align" ]
                [ svg
                    [ Svg.Attributes.width (toString model.windowSize.x)
                    , Svg.Attributes.height (toString model.windowSize.y)
                    ]
                    [ Svg.rect
                        [ Svg.Attributes.width (toString model.windowSize.x)
                        , Svg.Attributes.height (toString model.windowSize.y)
                        , Svg.Attributes.fill "#eee"
                        ]
                        []
                    , drawTurtle model
                    ]
                ]
            ]
        , div [ htmlclass "row" ]
            [ div [ htmlclass "input-field col s12" ]
                [ Html.textarea [ htmlclass "materialize-textarea", onInput Change ] []
                ]
            ]
        , Html.button [ Html.Events.onClick Eval ]
            [ Html.text "eval" ]
        , output
        ]


drawTurtle : Model -> Svg Msg
drawTurtle model =
    Svg.rect
        [ Svg.Attributes.height "20"
        , Svg.Attributes.width "10"
        , Svg.Attributes.fill "#000"
        , Svg.Attributes.x (toString (model.position.x + model.windowSize.x // 2 - 5))
        , Svg.Attributes.y (toString (model.position.y + model.windowSize.y // 2 - 10))
        , Svg.Attributes.transform
            ("rotate("
                ++ toString (model.heading - 90)
                ++ " "
                ++ toString (model.position.x + model.windowSize.x // 2)
                ++ " "
                ++ toString (model.position.y + model.windowSize.y // 2)
                ++ ")"
            )
        ]
        []



-- SUBSCRIPTIONS


subscription : Model -> Sub Msg
subscription model =
    Sub.none
