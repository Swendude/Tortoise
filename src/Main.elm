module Main exposing (..)

import Element exposing (..)
import Element.Attributes exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Interpreter exposing (..)
import List exposing (append, head, length, map, tail, take)
import Parser exposing (Error)
import Renderer exposing (..)
import String exposing (join, split)
import Style exposing (..)
import Style.Border as Border
import Style.Color as Color
import Style.Font as Font
import Style.Transition as Transition
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
    , speed : Float
    }


init : ( Model, Cmd msg )
init =
    ( Model
        ""
        (initialize "")
        10
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
-- STYLE


type Styles
    = None


stylesheet : Style.StyleSheet Styles variation
stylesheet =
    Style.styleSheet
        [ Style.style None []
        ]


view : Model -> Html Msg
view model =
    Element.viewport stylesheet <|
        Element.grid None
            [ Element.Attributes.width <| Element.Attributes.percent 100, Element.Attributes.height <| Element.Attributes.percent 100 ]
            { columns =
                [ Element.Attributes.percent 20
                , Element.Attributes.percent 20
                , Element.Attributes.percent 20
                , Element.Attributes.percent 20
                , Element.Attributes.percent 20
                ]
            , rows =
                [ Element.Attributes.percent 10
                , Element.Attributes.percent 5
                , Element.Attributes.percent 70
                , Element.Attributes.percent 15
                ]
            , cells =
                [ cell
                    { start = ( 0, 0 )
                    , width = 1
                    , height = 1
                    , content =
                        el None [] (Element.h1 None [] <| Element.text "Tortoise")
                    }
                , cell
                    { start = ( 1, 1 )
                    , width = 1
                    , height = 2
                    , content =
                        el None [] (Element.text "box")
                    }
                ]
            }



--view model =
--    let
--        output =
--            case model.interpreter.stateControl of
--                Error val ->
--                    Html.div [ htmlclass "col s12 center-align" ]
--                        (Html.h5 [] [ Html.text ("Errors on " ++ printContexts val.context) ]
--                            :: List.map (\problemstring -> Html.p [ htmlclass "red-text text-darken-2" ] [ Html.text problemstring ]) (printProblems val.problem)
--                        )
--                StateControl stateControl ->
--                    --case ( cl.before, cl.current ) of
--                    --    ( [], END ) ->
--                    --        Html.div [ htmlclass "col s12 center-align" ]
--                    --            [ Html.h5 [] [ Html.text "Waiting for input!" ]
--                    --            ]
--                    --    _ ->
--                    Html.div [ htmlclass "col s12 center-align" ]
--                        [ Html.h5 [] [ Html.text "Success!" ]
--                        --:: List.map (\tokenstring -> Html.p [ htmlclass "green-text text-darken-2" ] [ Html.text tokenstring ]) (printTokens (cl.current :: cl.before))
--                        ]
--        turtleStatus =
--            Html.div []
--                [ Html.text ("pos: " ++ toString model.interpreter.tortoiseWorld.position)
--                , Html.text (" heading: " ++ toString model.interpreter.tortoiseWorld.heading)
--                ]
--    in
--    div [ htmlclass "container" ]
--        [ div [ htmlclass "row" ]
--            [ div [ htmlclass "col s12" ]
--                [ h4 [ htmlclass "center-align" ]
--                    [ Html.text "Tortoise" ]
--                ]
--            ]
--        , div [ htmlclass "row" ]
--            [ div [ htmlclass "col s6" ]
--                [ render model.interpreter.tortoiseWorld ]
--            , div
--                [ htmlclass "input-field col s4 push-s2 blue-grey lighten-5"
--                , Html.Attributes.style [ ( "margin", "0" ) ]
--                ]
--                [ Html.textarea
--                    [ htmlclass "materialize-textarea"
--                    , Html.Attributes.style [ ( "height", "400px" ), ( "padding", "0" ) ]
--                    , onInput Change
--                    , Html.Attributes.value model.input
--                    ]
--                    []
--                ]
--            ]
--        , div [ htmlclass "row" ]
--            [ div [ htmlclass "right-align" ]
--                [ Html.button
--                    [ htmlclass "btn waves-effect waves-light"
--                    , Html.Events.onClick Eval
--                    ]
--                    [ Html.text "eval" ]
--                ]
--            ]
--        , div [ htmlclass "row center-align" ]
--            [ turtleStatus
--            , output
--            ]
--        ]
-- SUBSCRIPTIONS


subscription : Model -> Sub Msg
subscription model =
    if isDone model.interpreter then
        Sub.none
    else
        Time.every (model.speed * millisecond) StepInterpreter
