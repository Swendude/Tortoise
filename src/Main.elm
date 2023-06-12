module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import TortoiseParser exposing (parse)
import Utils.Stringifiers exposing (tokensToString)


main =
    Browser.element { init = init, view = view, update = update, subscriptions = subscriptions }



-- SUBSCRIBTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- INIT + MODEL


type alias Model =
    { code : String
    , result : Maybe (Result String String)
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model
        ""
        Nothing
    , Cmd.none
    )



-- UPDATE


type Msg
    = UpdateCode String
    | ParseCode


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateCode s ->
            ( { model | code = s }, Cmd.none )

        ParseCode ->
            ( { model | result = Just <| Result.map tokensToString (parse model.code) }, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ textarea
            [ onInput UpdateCode
            , value model.code
            , rows 20
            , cols 60
            ]
            []
        , button [ onClick ParseCode ] [ text "parse" ]
        , outputView model.result
        ]


outputView : Maybe (Result String String) -> Html Msg
outputView maybeResult =
    case maybeResult of
        Just result ->
            case result of
                Ok res ->
                    div []
                        [ p [] [ text "Succes" ]
                        , p [] [ text res ]
                        ]

                Err err ->
                    div []
                        [ p [] [ text "Error" ]
                        , p [] [ text err ]
                        ]

        Nothing ->
            p [] []
