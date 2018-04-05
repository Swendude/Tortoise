module Renderer exposing (..)

import Html exposing (..)
import Interpreter exposing (State, TortoiseWorld)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Tuple exposing (..)


--        [ svg
--            [ Svg.Attributes.width (toString model.windowSize.x)
--            , Svg.Attributes.height (toString model.windowSize.y)
--            ]
--            [ Svg.rect
--                [ Svg.Attributes.width (toString model.windowSize.x)
--                , Svg.Attributes.height (toString model.windowSize.y)
--                , Svg.Attributes.fill "#eee"
--                ]
--                []
--            --, drawTurtle model
--            ]
--        ]
--drawTurtle : Model -> Svg Msg
--drawTurtle model =
--    Svg.rect
--        [ Svg.Attributes.height "20"
--        , Svg.Attributes.width "10"
--        , Svg.Attributes.fill "#000"
--        , Svg.Attributes.x (toString (model.position.x + model.windowSize.x // 2 - 5))
--        , Svg.Attributes.y (toString (model.position.y + model.windowSize.y // 2 - 10))
--        , Svg.Attributes.transform
--            ("rotate("
--                ++ toString (model.heading - 90)
--                ++ " "
--                ++ toString (model.position.x + model.windowSize.x // 2)
--                ++ " "
--                ++ toString (model.position.y + model.windowSize.y // 2)
--                ++ ")"
--            )
--        ]
--        []
--type alias TortoiseWorld =
--    { worldDimensions : ( Int, Int )
--    , position : ( Int, Int )
--    , heading : Int
--    }


render : TortoiseWorld -> Html msg
render tw =
    svg
        [ width (toString (first tw.worldDimensions))
        , height (toString (second tw.worldDimensions))
        ]
        [ rect
            [ width (toString (first tw.worldDimensions))
            , height (toString (second tw.worldDimensions))
            , fill "#eee"
            ]
            []
        , rect
            [ Svg.Attributes.height "10"
            , Svg.Attributes.width "20"
            , Svg.Attributes.fill "#000"
            , Svg.Attributes.x (toString (first tw.position + first tw.worldDimensions // 2 - 10))
            , Svg.Attributes.y (toString (second tw.position + second tw.worldDimensions // 2 - 5))
            , Svg.Attributes.transform
                ("rotate("
                    ++ toString tw.heading
                    ++ " "
                    ++ toString (first tw.position + first tw.worldDimensions // 2)
                    ++ " "
                    ++ toString (second tw.position + second tw.worldDimensions // 2)
                    ++ ")"
                )
            ]
            []
        ]
