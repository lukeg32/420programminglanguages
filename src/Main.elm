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
import Regex
import Dict exposing (Dict)
import Random
import Debug
import Time
import Task



-- MAIN


main : Program () Model Msg
main =
  Browser.element
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- MODEL


type alias Model =
    { value : Int
    , output : String
    , input : String
    , seed : Random.Seed
    , cur_parse : String
    , tokens : Tokens
    }



init : () -> ( Model, Cmd Msg )
init flags =
    (Model 0 "Hi" "crap\nmore\n\n{\n<aname>\nlineone;\nlinetwo;\n}\n\n{\n<2name>\n2lineone;\n2linetwo;\n}" (Random.initialSeed 111111) "a" Dict.empty
    , Task.perform GetTime Time.now)


type alias Tokens = Dict String (List String)

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
    --{a | a = Dict.insert "asdf" ["asdf","fdsa"] a}

type Msg
    = Increment
    | Decrement
    | Exec
    | Change String
    | GetTime Time.Posix
    | RandomNum
    | Parse

noMaybe : Maybe (List String) -> 
noMaybe


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Increment -> ({ model | value = model.value + 1 }, Cmd.none)

        Decrement -> ({ model | value = model.value - 1 }, Cmd.none)

        Exec ->
            ({ model | cur_parse = (Dict.get "<start>" model.tokens) |> noMaybe, output = Dict.get "<start>" model.tokens}
             , Cmd.none)

        Parse ->
            ({model | tokens = makeStruct model.input}, Cmd.none)

        Change new -> ({ model | input = new }, Cmd.none)

        GetTime timeNow ->
            ( { model | seed  = Random.initialSeed <| Time.posixToMillis timeNow }, Cmd.none )

        RandomNum ->
            let
                result = Random.step (Random.int 0 100) model.seed
                newSeed = Tuple.second result
            in
            ( { model | seed  =  newSeed }, Cmd.none )




whitespace : Parser ()
whitespace =
    chompWhile (\c -> c == ' ' || c == '\n')


-- nme : Parser Name
-- nme =
    -- succeed Name
        -- |. rmCrap
        -- |. symbol "{\n"
        -- |. symbol "<"
        -- |= (getChompedString <| chompUntil ">")
        -- |. symbol ">"
        -- |. whitespace
        -- |= (getChompedString <| chompUntil ";")
        -- |. symbol ";"
        -- |. whitespace


-- gets each chunk surrounded by {}
getChunks : Regex.Regex
getChunks =
    Maybe.withDefault Regex.never <|
        Regex.fromString "{[^}{]+}"

-- gets each bit a nonterminal or the terminal part
getBits : Regex.Regex
getBits =
    Maybe.withDefault Regex.never <|
        Regex.fromString "[^}\\n{;]+"

-- regex for nontermianl
nonTerminal : Regex.Regex
nonTerminal =
    Maybe.withDefault Regex.never <|
        Regex.fromString "<[^<>]+>"

-- takes a chunk and turns it into a list of strings
runRegex : Regex.Match -> List String
runRegex string =
    Regex.find getBits string.match |> List.map .match

-- remove the maybe
unMaybe : Maybe String -> String
unMaybe str =
    case str of
        Just y -> y
        Nothing -> "error: bad maybe"

-- takes the list of strings and turns it into a token object, which is just a dictionary
turnToTokens : List String -> Tokens
turnToTokens lstr =
    Dict.fromList [(List.head lstr |> unMaybe, List.drop 1 lstr)]

-- takes the input string and turns it into the Tokens struct
makeStruct : String -> Tokens
makeStruct raw =
    Regex.find getChunks raw
    |> List.map runRegex
    |> List.map turnToTokens
    |> List.foldr Dict.union Dict.empty

hasTerminal : String -> Bool
hasTerminal str =
    Regex.contains nonTerminal str


-- getRandom : List String -> Int
-- getRandom arr =
    -- let
        -- --gen = Random.List.choose arr
        -- (i, value) = Random.generate New (Random.int 0 10)
        -- elem = List.drop i arr |> List.head |> unMaybe
    -- in
    -- elem

getTerminal : String -> String
getTerminal input =
    case (Regex.find nonTerminal input |> List.head) of
        Just value -> value.match
        Nothing -> "Uh oh"

type alias Inside = List String


--List.drop (Random.generate Random.int 0 (List.length value - 1)) value |> List.head |> unMaybe
replaceTerminal : Tokens -> String -> String
replaceTerminal toks input =
    let key = getTerminal input in
    case (Dict.get key toks) of
        Just value -> Regex.replaceAtMost 1 nonTerminal (\_ -> List.head value |> unMaybe) input
        Nothing -> "error: bad key"

replaceNonTerminals : Tokens -> String -> String
replaceNonTerminals toks input =
    if (hasTerminal input) then
            --Debug.log "asdf"
            replaceTerminal toks input |> replaceNonTerminals toks
    else
        input


-- turns the toks and the input into an actual statment
generateStatment : Tokens -> String -> String
generateStatment toks input =

    replaceTerminal toks "asdf <aname> fdsa <2name>"


getNumber : Model -> Int
getNumber model =
    let
        result = Random.step (Random.int 0 100) model.seed
        num = Tuple.first result
    in
    num




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
                 , style "width" "400px"
                 ]
                 [ text model.output
                 ]
            ]

-- VIEW


view : Model -> Html Msg
view model =
  div [ style "backgroundColor" "black"]
    [ button [ onClick Decrement ] [ text "-" ]
    , button [ onClick RandomNum ] [ text "random" ]
    , button [ onClick Parse ] [ text "Make Parse Structure" ]
    , div [] [ text (String.fromInt model.value) ]
    , button [ onClick Increment ] [ text "+" ]
    , button [ onClick Exec ] [ text "run" ]
    , div [] []
    -- , text <| Debug.toString <| (Parser.run nme model.input )
    , div [] []
    , div [ style "color" "white" ] [ text <| Debug.toString <| (model.tokens) ]
    , div [ style "color" "white" ] [ text <| Debug.toString <| (getNumber model) ]
    , div [] []
    , getText model
    , div [] [ text (model.output) ]
    ]
