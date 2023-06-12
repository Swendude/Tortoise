module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import TortoiseParser exposing (parse)
import Utils.Stringifiers exposing (deadEndsToString, tokensToString)


main =
    Browser.element { init = init, view = view, update = update, subscriptions = subscriptions }



-- SUBSCRIBTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- INIT + MODEL


type alias Model =
    { code : String
    , result : Maybe (Result (List String) String)
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
            ( { model
                | result =
                    Just <|
                        Result.mapError deadEndsToString <|
                            Result.map tokensToString <|
                                parse model.code
              }
            , Cmd.none
            )



-- VIEW


view : Model -> Html Msg
view model =
    div [ class "container" ]
        [ textarea
            [ onInput UpdateCode
            , value model.code
            , spellcheck False
            ]
            []
        , button [ onClick ParseCode ] [ text "parse" ]
        , outputView model.result
        ]


outputView : Maybe (Result (List String) String) -> Html Msg
outputView maybeResult =
    case maybeResult of
        Just result ->
            case result of
                Ok res ->
                    div []
                        [ p [] [ text "Succes" ]
                        , p [] [ text res ]
                        ]

                Err errs ->
                    div [ class "errors" ] <| List.map error errs

        Nothing ->
            p [] []


error : String -> Html Msg
error err =
    p [ class "error" ] [ text err ]
