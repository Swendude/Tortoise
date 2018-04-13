module Renderer exposing (..)

import Html exposing (..)
import Interpreter exposing (State, TortoiseWorld)
import String exposing (join)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Tuple exposing (..)


render : TortoiseWorld -> Html msg
render tw =
    let
        screenWidth =
            first tw.worldDimensions

        screenHeight =
            second tw.worldDimensions

        minX =
            toString
                (negate (screenWidth // 2))

        minY =
            toString
                (negate (screenHeight // 2))

        maxX =
            toString (screenWidth // 2)

        maxY =
            toString (screenHeight // 2)

        vb =
            join ", "
                [ minX
                , minY
                , maxX
                , maxY
                ]
    in
    svg
        [ width (toString screenWidth)
        , height (toString screenHeight)
        ]
        [ g [ transform "translate(200, 200) scale(1, -1)" ]
            [ rect
                [ x minX
                , y minY
                , width (toString screenWidth)
                , height (toString screenHeight)
                , fill "#eee"
                ]
                []
            , axis
            , renderTurtle tw
            ]
        ]


renderTurtle : TortoiseWorld -> Svg msg
renderTurtle tw =
    let
        --negation for SVG coordinate system
        rotationTransform =
            "rotate (" ++ toString (negate tw.heading) ++ ")"

        translateTransform =
            "translate ("
                ++ toString (first tw.position)
                ++ ","
                ++ toString (second tw.position)
                ++ ")"

        transformString =
            join " " [ translateTransform, rotationTransform ]
    in
    g [ transform transformString ]
        [ polygon [ points (triangle 16 20 3), fill "#000" ] [] ]


triangle : Int -> Int -> Int -> String
triangle w h i =
    let
        coords =
            [ ( 0, h // 2 )
            , ( -w // 2, -h // 2 )
            , ( 0, (-h // 2) + i )
            , ( w // 2, -h // 2 )
            ]
    in
    join " " (List.map (\tup -> toString (first tup) ++ "," ++ toString (second tup)) coords)



--"0,0 5,10, 10,0"


axis : Svg msg
axis =
    g []
        [ line
            [ x1 "0"
            , y1 "0"
            , x2 "200"
            , y2 "0"
            , strokeWidth "1"
            , stroke "rgb(0,0,0)"
            , strokeDasharray "2,4"
            , strokeOpacity "0.6"
            ]
            []
        , line
            [ x1 "0"
            , y1 "0"
            , x2 "0"
            , y2 "200"
            , strokeWidth "1"
            , stroke "rgb(0,0,0)"
            , strokeDasharray "2,4"
            , strokeOpacity "0.6"
            ]
            []
        , line
            [ x1 "0"
            , y1 "0"
            , x2 "-200"
            , y2 "0"
            , strokeWidth "1"
            , stroke "rgb(0,0,0)"
            , strokeDasharray "2,4"
            , strokeOpacity "0.6"
            ]
            []
        , line
            [ x1 "0"
            , y1 "0"
            , x2 "0"
            , y2 "-200"
            , strokeWidth "1"
            , stroke "rgb(0,0,0)"
            , strokeDasharray "2,4"
            , strokeOpacity "0.6"
            ]
            []
        ]



--rect
--    [ Svg.Attributes.height "10"
--    , Svg.Attributes.width "20"
--    , Svg.Attributes.fill "#000"
--    , Svg.Attributes.x (toString (first tw.position + first tw.worldDimensions // 2 - 10))
--    , Svg.Attributes.y (toString (second tw.position + second tw.worldDimensions // 2 - 5))
--    , Svg.Attributes.transform
--        ("rotate("
--            ++ toString tw.heading
--            ++ " "
--            ++ toString (first tw.position + first tw.worldDimensions // 2)
--            ++ " "
--            ++ toString (second tw.position + second tw.worldDimensions // 2)
--            ++ ")"
--        )
--    ]
--    []
