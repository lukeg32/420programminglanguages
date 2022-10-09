module Main exposing (..)

-- Press buttons to increment and decrement a counter.
--
-- Read how it works:
--   https://guide.elm-lang.org/architecture/buttons.html
--


import Browser
import Html exposing (Html, button, div, text, textarea, Attribute)
import Html.Attributes exposing (width, height, style)
import Html.Events exposing (onClick)
import Parser exposing (..)
import Dict exposing (..)
import List exposing (..)



-- MAIN


main =
  Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Model =
    { value : Int
    , output : String
    , input : String
    }



init : Model
init =
    Model 0 "Hi" "crap\nmore\n\n{\n<aname>\nlineone;\nlinetwo;\n}\n\n{\n<2name>\n2lineone;\n2linetwo;\n}"

type alias Point =
    { x : Float
    , y : Float
    }

type alias Name = 
    { thing : String
    , line : String
    }

-- UPDATE

--lines = Dict.empty String List(String)
a = Dict.empty
    --{a | a = Dict.insert "asdf" ["asdf","fdsa"] a}

type Msg
    = Increment
    | Decrement


update : Msg -> Model -> Model
update msg model =
    case msg of
        Increment -> { model | value = model.value + 1 }

        Decrement -> { model | value = model.value - 1 }


getText : Model -> Html msg
getText model =
    textarea [ style "height" "400px"
             , style "width" "400px"
             ]
             [ text model.input
             ]


whitespace : Parser ()
whitespace =
    chompWhile (\c -> c == ' ' || c == '\n')


nme : Parser Name
nme =
    succeed Name
        |. rmCrap
        |. symbol "{\n"
        |. symbol "<"
        |= (getChompedString <| chompUntil ">")
        |. symbol ">"
        |. whitespace
        |= (getChompedString <| chompUntil ";")
        |. symbol ";"
        |. whitespace


rmCrap : Parser ()
rmCrap =
    chompUntil "{"

--areaCode : Parser String
--areaCode =
        --succeed 
            --|. symbol "<"
            --|= (getChompedString <| chompUntil ">")
            --|. symbol ">"


-- VIEW


view : Model -> Html Msg
view model =
  div []
    [ button [ onClick Decrement ] [ text "-" ]
    , div [] [ text (String.fromInt model.value) ]
    , button [ onClick Increment ] [ text "+" ]
    , div [] []
    , text <| Debug.toString <| (Parser.run nme model.input )
    , div [] []
    , getText model
    , div [] [ text (model.output) ]
    ]
