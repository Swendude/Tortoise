module Renderer exposing (..)

import Interpreter exposing (State, TortoiseWorld)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Tuple exposing (..)


--type alias TortoiseWorld =
--    { worldDimensions : ( Int, Int )
--    , position : ( Int, Int )
--    , heading : Int
--    }
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


render : Result () State -> Svg msg
render state =
    svg
        [ width ((toString << first) tw.worldDimensions)
        , height ((toString << second) tw.worldDimensions)
        ]
        [ rect
            [ width ((toString << first) tw.worldDimensions)
            , height ((toString << second) tw.worldDimensions)
            , fill "#eee"
            ]
            []
        ]
