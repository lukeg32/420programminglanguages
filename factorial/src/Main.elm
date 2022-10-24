module Main exposing (..)


import Browser
import Html exposing (Html, button, div, text, textarea, Attribute)
import Html.Attributes exposing (width, height, style)
import Html.Events exposing (onClick)
import Dict exposing (..)
import List exposing (..)
import Dict exposing (Dict)

-- MAIN


main : Program () Model Msg
main =
  Browser.element
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


-- MODEL

type alias Model =
    { cur_value : Int
    , output : Int
    , input : String
    }


init : () -> ( Model, Cmd Msg )
init flags =
    (Model 0 0 "1"
    , Cmd.none)


-- UPDATE

type Msg
    = Exec
    | Change String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Exec -> ({model | output = factorial <| unMaybe <| String.toInt model.input}, Cmd.none)
        Change str -> ({ model | input = str }, Cmd.none)

unMaybe : Maybe Int -> Int
unMaybe int =
    case int of
        Just x -> x
        Nothing -> -1


factorial : Int -> Int
factorial value =
    if value == 1 then
        1
    else
        value * factorial (value - 1)


-- VIEW

view : Model -> Html Msg
view model =
  div [ style "backgroundColor" "black"]
  [ getText model
  , button [ onClick Exec ] [ text "run" ]
  , Html.p [ style "color" "white" ] [ text <| String.fromInt model.output ]
  ]


getText : Model -> Html Msg
getText model =
    div [] [
        textarea [ style "height" "20px"
                 , style "width" "100px"
                 , Html.Events.onInput Change
                 ]
                 [ text model.input
                 ]
            ]
