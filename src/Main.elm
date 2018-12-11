module Main exposing (Model, Msg(..), Styles(..), Vector, ignoreLast, init, main, stylesheet, subscription, update, view)

import Color exposing (..)
import Element exposing (..)
import Element.Attributes exposing (..)
import Html exposing (..)
import Interpreter exposing (..)
import List exposing (append, head, length, map, tail, take)
import Parser exposing (Error)
import Renderer exposing (..)
import String exposing (join, split)
import Style exposing (..)
import Style.Border as Border
import Style.Color as Color
import Style.Font as Font
import Style.Shadow as Shadow
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
    | HeaderBar
    | WorldScreen
    | TitelText



-- https://material.io/design/color/the-color-system.html#tools-for-picking-colors
-- P:262776
-- rgba 38 39 118 1
-- colorsheet : Dict String Color
-- colorsheet =
--     Dict.fromList
--         [ ( "Blue-900", Color.rgb 38 39 118 )
--         , ( "Blue-700", Color.rgb 58 66 152 )
--         ]


colorsheet =
    { blue_900 = Color.rgb 38 39 118
    , blue_700 = Color.rgb 58 66 152
    }


stylesheet : Style.StyleSheet Styles variation
stylesheet =
    Style.styleSheet
        [ Style.style HeaderBar
            [ Color.background colorsheet.blue_900
            , Shadow.box
                { offset = ( 0, 0 )
                , size = 2
                , blur = 2
                , color = colorsheet.blue_700
                }
            ]
        , Style.style TitelText
            [ Color.text white
            , Font.size 25
            , Font.typeface [ Font.font "Helvetica" ]
            , Font.light
            ]
        , Style.style WorldScreen []
        , Style.style None []
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
                [ Element.Attributes.percent 20
                , Element.Attributes.percent 20
                , Element.Attributes.percent 20
                , Element.Attributes.percent 20
                , Element.Attributes.percent 20
                ]
            , cells =
                [ cell
                    { start = ( 0, 0 )
                    , width = 5
                    , height = 1
                    , content =
                        row HeaderBar
                            [ padding 40, verticalCenter ]
                            [ Element.h1 TitelText [] <| Element.text "Tortoise" ]
                    }
                , cell
                    { start = ( 0, 1 )
                    , width = 3
                    , height = 3
                    , content =
                        Element.el WorldScreen [] <| Element.h1 None [] <| Element.text "WS"
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
