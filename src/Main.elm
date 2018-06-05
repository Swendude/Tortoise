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
    { input : String
    , interpreter : State
    }


init : ( Model, Cmd msg )
init =
    ( Model
        ""
        (initialize "")
    , Cmd.none
    )



-- UPDATE


type Msg
    = Change String
    | Eval
    | StepInterpreter Time


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
        Change inputString ->
            ( { model | input = inputString }, Cmd.none )

        Eval ->
            let
                sanetizedInput =
                    String.toUpper model.input
            in
            ( { model | input = sanetizedInput, interpreter = Interpreter.initialize sanetizedInput }, Cmd.none )

        StepInterpreter _ ->
            let
                interpreterState =
                    runCommand model.interpreter
            in
            case interpreterState of
                Ok state ->
                    ( { model | interpreter = state }, Cmd.none )

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
            case model.interpreter.commandList of
                Error val ->
                    Html.div [ htmlclass "col s12 center-align" ]
                        (Html.h5 [] [ Html.text ("Errors on " ++ printContexts val.context) ]
                            :: List.map (\problemstring -> Html.p [ htmlclass "red-text text-darken-2" ] [ Html.text problemstring ]) (printProblems val.problem)
                        )

                CommandList cl ->
                    Html.div [ htmlclass "col s12 center-align" ]
                        (Html.h5 [] [ Html.text "Succes!: " ]
                            :: List.map (\tokenstring -> Html.p [ htmlclass "green-text text-darken-2" ] [ Html.text tokenstring ]) (printTokens (cl.current :: cl.before))
                        )

        turtleStatus =
            Html.div []
                [ Html.text ("pos: " ++ toString model.interpreter.tortoiseWorld.position)
                , Html.text (" heading: " ++ toString model.interpreter.tortoiseWorld.heading)
                ]
    in
    div [ htmlclass "container" ]
        [ div [ htmlclass "row" ]
            [ div [ htmlclass "col s12" ]
                [ h4 [ htmlclass "center-align" ]
                    [ Html.text "Tortoise" ]
                ]
            ]
        , div [ htmlclass "row" ]
            [ div [ htmlclass "col s6" ]
                [ render model.interpreter.tortoiseWorld ]
            , div
                [ htmlclass "input-field col s4 push-s2 blue-grey lighten-5"
                , Html.Attributes.style [ ( "margin", "0" ) ]
                ]
                [ Html.textarea
                    [ htmlclass "materialize-textarea"
                    , Html.Attributes.style [ ( "height", "400px" ), ( "padding", "0" ) ]
                    , onInput Change
                    , Html.Attributes.value model.input
                    ]
                    []
                ]
            ]
        , div [ htmlclass "row" ]
            [ div [ htmlclass "right-align" ]
                [ Html.button
                    [ htmlclass "btn waves-effect waves-light"
                    , Html.Events.onClick Eval
                    ]
                    [ Html.text "eval" ]
                ]
            ]
        , div [ htmlclass "row center-align" ]
            [ turtleStatus
            , output
            ]
        ]



-- SUBSCRIPTIONS


subscription : Model -> Sub Msg
subscription model =
    if isDone model.interpreter then
        Sub.none
    else
        Time.every second StepInterpreter
