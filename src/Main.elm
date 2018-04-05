module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Interpreter exposing (..)
import List exposing (append, head, length, map, tail, take)
import Parser exposing (Error)
import Renderer exposing (..)
import String exposing (join, split)
import Time exposing (..)
import TortoiseParser exposing (..)


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
    , input : String
    , output : Maybe (Result Parser.Error (List Token))
    , interpreter : State
    }


init : ( Model, Cmd msg )
init =
    ( Model 0 { x = 0, y = 0 } [] { x = 500, y = 400 } "" Nothing (Err ()), Cmd.none )



-- UPDATE


type Msg
    = Rotate Int
    | Forward Int
    | Change String
    | Eval
    | StepInterpreter Time


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
            ( { model | input = inputString }, Cmd.none )

        Eval ->
            let
                sanetizedInput =
                    String.toUpper model.input
            in
            ( { model | input = sanetizedInput, interpreter = Interpreter.initialize sanetizedInput }, Cmd.none )

        StepInterpreter _ ->
            case model.interpreter of
                Ok state ->
                    let
                        interpreterState =
                            runCommand state
                    in
                    ( { model | interpreter = interpreterState }, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )



-- VIEW


htmlclass : String -> Html.Attribute msg
htmlclass =
    Html.Attributes.class


view : Model -> Html Msg
view model =
    let
        output =
            case model.output of
                Just (Ok val) ->
                    Html.div []
                        (Html.h5 [] [ Html.text "Succes!: " ]
                            :: List.map (\tokenstring -> Html.p [ htmlclass "green-text text-darken-2" ] [ Html.text tokenstring ]) (printTokens val)
                        )

                Just (Err val) ->
                    Html.div []
                        (Html.h5 [] [ Html.text "Errors: " ]
                            :: List.map (\problemstring -> Html.p [ htmlclass "red-text text-darken-2" ] [ Html.text problemstring ]) (printProblems val.problem)
                        )

                Nothing ->
                    Html.div []
                        [ Html.p [] [ Html.text "Welcome to Tortoise. Eval some code to begin!" ]
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

        --, div [ htmlclass "row" ]
        --    [ div [ htmlclass "center-align" ]
        --        renderWorld model.
        --    ]
        , div [ htmlclass "row" ]
            [ div [ htmlclass "input-field col s12" ]
                [ Html.textarea [ htmlclass "materialize-textarea", onInput Change, Html.Attributes.value model.input ] []
                ]
            ]
        , Html.button [ htmlclass "btn waves-effect waves-light", Html.Events.onClick Eval ]
            [ Html.text "eval" ]
        , output
        ]



-- SUBSCRIPTIONS


subscription : Model -> Sub Msg
subscription model =
    case model.interpreter of
        Ok state ->
            if isDone state then
                Sub.none
            else
                Time.every second StepInterpreter

        Err _ ->
            Sub.none
