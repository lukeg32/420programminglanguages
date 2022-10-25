module Main exposing (..)


import Browser
import Html exposing (Html, button, div, text, textarea, Attribute)
import Html.Attributes exposing (width, height, style)
import Html.Events exposing (onClick)
import Dict exposing (..)
import List exposing (..)
import Dict exposing (Dict)
import Regex

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
    { output : String
    , input : String
    }

type alias Person =
    { name : String
    , age : Int
    }


init : () -> ( Model, Cmd Msg )
init flags =
    (Model "" "luke, 10\nmatt, 11\nkebvin, 34\ndylan, 24" , Cmd.none)


-- UPDATE

type Msg
    = Exec
    | Change String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Exec -> ({ model | output = getStats <| getPeople model.input }, Cmd.none)
        Change str -> ({ model | input = str }, Cmd.none)

unMaybe : Maybe Int -> Int
unMaybe int =
    case int of
        Just x -> x
        Nothing -> -1

unMaybeStr : Maybe String -> String
unMaybeStr int =
    case int of
        Just x -> x
        Nothing -> "oh no"

-- gets each line of the "file"
lineRegex : Regex.Regex
lineRegex =
    Maybe.withDefault Regex.never <|
        Regex.fromString "\\w+, \\d+"

-- splits the string and makes a person
becomePerson : String -> Person
becomePerson value =
    let
        result = String.split ", " value

        -- get the name from the list
        name = List.head result
               |> unMaybeStr

        -- get age and make it an int
        age = List.reverse result
              |> List.head
              |> unMaybeStr
              |> String.toInt
              |> unMaybe
    in
    Person name age



getPeople : String -> List Person
getPeople str =
    Regex.find lineRegex str
    |> List.map .match
    |> List.map becomePerson


getStats : List Person -> String
getStats people =
    let
        names = List.map .name people
                |> List.sort

        ages = List.map .age people
        numAges = List.length ages
        aveAge = (toFloat (List.foldr (+) 0 ages) / toFloat numAges)
    in
    String.join "\n" names ++ "\nAverage Age: " ++ (String.fromFloat aveAge)




-- VIEW

view : Model -> Html Msg
view model =
  div [ style "backgroundColor" "black"]
  [ getText model
  , button [ onClick Exec ] [ text "run" ]
  ]


getText : Model -> Html Msg
getText model =
    div [] [
        textarea [ style "height" "400px"
                 , style "width" "400px"
                 , Html.Events.onInput Change
                 ]
                 [ text model.input
                 ],
        textarea [ style "height" "400px"
                 , style "width" "620px"
                 ]
                 [ text model.output
                 ]
            ]
