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
    (Model 0 "Hi" polition (Random.initialSeed 111111) "a" Dict.empty
    , Task.perform GetTime Time.now)


type alias Tokens = Dict String (List String)


-- UPDATE

type Msg
    =
    | Exec
    | Change String
    | GetTime Time.Posix
    | RandomNum
    | Terminal

noMaybe : Maybe (List String) -> List String
noMaybe this =
    case this of
        Just a -> a
        Nothing -> ["failed", "Failed"]



update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Exec ->
            let
                mm = { model | tokens = makeStruct model.input, cur_parse = "", output = ""}
            in
            update Terminal { mm | cur_parse = (Dict.get "<start>" mm.tokens) |> noMaybe |> List.head |> unMaybe}

        Terminal ->
            if hasTerminal model.cur_parse then
                update Terminal (replaceTerminal model)
            else
                ({ model | output = model.cur_parse }, Cmd.none)

        Change new -> ({ model | input = new }, Cmd.none)

        GetTime timeNow ->
            ( { model | seed  = Random.initialSeed <| Time.posixToMillis timeNow }, Cmd.none )

        RandomNum ->
            let
                result = Random.step (Random.int 0 100) model.seed
                newSeed = Tuple.second result
            in
            ( { model | seed  =  newSeed }, Cmd.none )






-- gets each chunk surrounded by {}
getChunks : Regex.Regex
getChunks =
    Maybe.withDefault Regex.never <|
        Regex.fromString "{[^}{]+}"

-- gets each bit a nonterminal or the terminal part
getBits : Regex.Regex
getBits =
    Maybe.withDefault Regex.never <|
        Regex.fromString "[^}{;]+"
        --Regex.fromString "((?(?=.*?({\\n<[^<>]+>\\n).*?)\\2)[^{};]+)"

-- regex for nontermianl
nonTerminal : Regex.Regex
nonTerminal =
    Maybe.withDefault Regex.never <|
        Regex.fromString "<[^<>]+>"

-- "parser" will take a couple newlines just for fun, remove those
notJustaNewline : String -> Bool
notJustaNewline str =
    if str == "\n" then
        False
    else
        True

-- removes a front newline, because the "parser" leaves them in
removeFrontNewline : String -> String
removeFrontNewline str =
    if (String.left 1 str) == "\n" then
        String.dropLeft 1 str
    else
        str

extract : List String -> List String
extract list =
    let
        first = List.head list |> unMaybe
        -- extract the terminal name from the first element
        term = Regex.find nonTerminal first |> List.map .match |> List.head |> unMaybe
        rest = Regex.replaceAtMost 1 nonTerminal (\_ -> "") first |> String.dropLeft 2
        newList = term :: rest :: (List.drop 1 list) |> List.filter notJustaNewline |> List.map removeFrontNewline
    in
    newList

-- takes a chunk and turns it into a list of strings
runRegex : Regex.Match -> List String
runRegex string =
    Regex.find getBits string.match |> List.map .match |> extract

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


getTerminal : String -> String
getTerminal input =
    case (Regex.find nonTerminal input |> List.head) of
        Just value -> value.match
        Nothing -> "Uh oh"

type alias Inside = List String

getLength : String -> Tokens -> Int
getLength key toks =
    case (Dict.get key toks) of
        Just value -> List.length value
        Nothing -> -1

getList : String -> Tokens -> List String
getList key toks =
    case (Dict.get key toks) of
        Just value -> value
        Nothing -> ["uh oh"]

getAt : List String -> Int -> String
getAt list index =
    List.drop index list |> List.head |> unMaybe

--List.drop (Random.generate Random.int 0 (List.length value - 1)) value |> List.head |> unMaybe
replaceTerminal : Model -> Model
replaceTerminal model =
    let
        key = getTerminal model.cur_parse
        list = getList key model.tokens
        len = getLength key model.tokens
        values = Random.step (Random.int 0 (len - 1)) model.seed
        replaceWith = getAt list (Tuple.first values)
    in
    case (Dict.get key model.tokens) of
        Just value -> {model | seed = (Tuple.second values), cur_parse = Regex.replaceAtMost 1 nonTerminal (\_ -> replaceWith) model.cur_parse}
        Nothing -> model



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
                 , style "width" "620px"
                 ]
                 [ text model.output
                 ]
            ]

-- VIEW


view : Model -> Html Msg
view model =
  div [ style "backgroundColor" "black"]
    [ --button [ onClick Decrement ] [ text "-" ]
    --, button [ onClick RandomNum ] [ text "random" ]
    --, button [ onClick Parse ] [ text "Make Parse Structure" ]
    --, div [] [ text (String.fromInt model.value) ]
    --, button [ onClick Increment ] [ text "+" ]
    button [ onClick Exec ] [ text "run" ]
    , div [] []
    -- , text <| Debug.toString <| (Parser.run nme model.input )
    , div [] []
    --, div [ style "color" "white" ] [ text <| Debug.toString <| (getNumber model) ]
    , div [] []
    , getText model
    --, div [] [ text (model.output) ]
    --, div [ style "color" "white" ] [ text <| Debug.toString <| (model.tokens) ]
    ]



polition = """
# a test grammer
{
<start>
Dear <descriptor> Voter,

    I, <politician>, am running for <position>.  <argument>
        Please <supportive_verb> me by <means_of_support>.

    Thank you and <pc_salutation>;
}

{
<descriptor>
; ; ; ;
Conscientious;
Conscientious;
Conscientious;
Intelligent;
Intelligent;
Intelligent;
Supportive;
Supportive;
Supportive;
Attentive;
Attentive;
Attentive;
<party>;
<party>;
Foolish Mortal, I mean ... <descriptor>;
Incompetent Boob, I mean ... <descriptor>;
Stupid;
Non-;
Undecided;
}

{
<politician>
<politician>, a <party>;
Stanley P. Statesman;
Connie L. Congress;
Leslie O. Legislature;
Jordan H. Judge;
Bob;
Arnold Schwartzenegger;
John Kerry;
Bill Clinton;
George Bush Sr.;
George W. Bush;
G-Dubbya;
Ron Wyden;
Matthew Cox;
Dr. David M. Hansen;
Mr. Brent Wilson;
Joe Bruce - Emperor of the World;
George Foreman;
George Washington;
Abraham Lincoln;
Mahatma Gandhi;
Siddhartha Gautama;
Moses;
Satan;
Spongebob Squarepants;
George Fox;
Ralph Nader;
Homer Simpson;
Oscar Meyer;
}

{
<party>
Republican;
Democrat;
Whig;
Federalist;
Constitutionalist;
Fundamentalist;
Nazi;
PANSI Green-party member;
Gay Republican;
Libertarian;
Communist;
Socialist;
Quaker;
}

{
<position>
<office>;
<office> of <political_body>;
}

{
<office>
Janitor;
Dictator;
Senator;
Congressperson;
Legislator;
Supreme Court Judge;
Evil Oppressor;
President;
Vice President;
Treasurer;
Governor;
Prime Minister;
Owner;
General Representative;
King;
}

{
<political_body>
the world;
<country>;
<state>;
<county> County;
<city>;
<school>;
<minority_group>;
the ACLU;
MADD;
Microsoft;
}

{
<country>
the United States;
Canada;
Mexico;
Micronesia;
Japan;
China;
Germany;
India;
Israel;
England;
}

{
<state>
Oregon;
Washington;
California;
New York;
Texas;
Utah;
Florida;
Virginia;
Massachusetts;
Alaska;
}

{
<county>
Lane;
Washington;
Multnomah;
Yamhill;
Linn;
Clark;
Podunk;
Clickatat;
}

{
<city>
Portland;
Newberg;
Beaverton;
Salem;
Eugene;
Roseburg;
Seattle;
Spokane;
Corvallis;
New York;
Miami;
Houston;
Berlin;
Paris;
Jerusalem;
Beijing;
Nairobi;
Springfield;
}

{
<school>
<politician> University;
University of <place>;
<state> State University;
<politician> <level> School;
<place> <level> School;
<place> Graduate Institute;
Institute of <place>;
}

{
<level>
High;
Middle;
Elementary;
Cosmetic;
Technical;
}

{
<place>
<country>;
<state>;
<county> County;
<city>;
}

{
<minority_group>
Hispanics;
Blacks;
Women;
Homosexuals;
Asians;
the League of People Too Racist Against Whites to Admit It;
Programmers Who Produce Good Microsoft Products;
}

{
<argument>
<argument> <argument>;
<argument> <argument>;
<argument> <argument>;
I am <feeling> <issue>.;
My opponent, <politician>, is <feeling> <issue>.;
If <event>, I will <outlandish_claim>.;
If <event>, my opponent will <outlandish_claim>.;
If <event>, <politician> will <outlandish_claim>.;
}

{
<feeling>
outraged by;
excited about;
happy about;
sad about;
disheartened by;
encouraged by;
unable to believe;
shocked by;
hopeful for;
impressed with;
}

{
<issue>
<issue> and <issue>;
<issue> and <issue>;
the use of partial-birth abortion;
the legalization of gay marriage;
the war in <country>;
reinstating the draft;
a harsher penalty for parole violators;
world peace;
world hunger;
the AIDS epidemic;
grammar parsing;
grammar generation;
the cost of tuition;
the use of sauerkraut on hot dogs;
the use of ejector seats in helicopters;
the creation of glow-in-the-dark sunglasses;
slanderous political campaigning;
the use of oil;
the use of electricity;
the building of nuclear power plants;
the spanking of children;
}

{
<event>
I am elected;
my opponent is elected;
Hell freezes over;
we do not remain strong;
you do not believe me;
you do not vote for me;
you vote for me;
I vote for you;
you don\'t get on that plane today;
}

{
<outlandish_claim>
<outlandish_claim> and <outlandish_claim>;
<outlandish_claim> and <outlandish_claim>;
begin <issue>;
end <issue>;
see to it that <issue> continues;
see to it that <issue> stops;
put <object> in every <building> and <object> on every doorstep;
never support <issue>;
always support <issue>;
never support <minority_group>;
always support <minority_group>;
tell the truth;
}

{
<object>
<object> or <object>;
<object> or <object>;
<object> and <object>;
<object> and <object>;
a car;
a dog;
a cat;
two cats;
five cats;
a lifetime supply of cats;
a child;
air freshener;
undue sums of money;
<amount>;
a newspaper;
a pillow;
a warm bed;
a home-cooked meal;
a rake;
a ping-pong table;
a foosball table;
a foosball;
a computer;
an Internet connection;
a set of headphones;
a license for all Microsoft products;
a telephone;
a toilet;
a department store;
a purple database (it has the most RAM);
}

{
<building>
home;
garage;
office;
fast food restaurant;
corporate office;
small-town Mom and Pop store;
official state building;
church;
paintball arena;
Porta-potty;
science center named after Edwards and Holman;
student union building;
dog house;
railway station;
}

{
<supportive_verb>
support;
join;
take up arms with;
move forward with;
do the right thing with;
trash my opponent with;
guarantee success for;
}

{
<means_of_support>
<means_of_support> and <means_of_support>;
donating <amount> to my campaign fund;
voting for me on election day;
sending out junk mail to all <party>\'s;
registering to vote;
bench-pressing twice your body weight on national television;
firing off <weapon>;
firing <weapon> into the air;
preventing all <party>\'s from voting;
encouraging all <party>\'s to vote;
inciting a revolution;
inciting a revolution using <weapon>;
burning the flag of <country>;
}

{
<amount>
<amount> or <amount>;
<amount> and <amount>;
$4.73;
$50;
$100;
$1,000;
$1,000,000;
a lot;
your cat;
your dog;
your children;
your mom;
everything you have;
your very life;
}
    
{
<weapon>
<weapon> and <weapon>;
fireworks;
semi-automatic machine guns;
rocket launchers;
handguns;
poison darts;
dangerous chemicals;
mortars;
surface-to-air missiles;
Spam;
M16s;
}

{
<pc_salutation>
God bless.;
remember, a vote for me is a vote for <issue>.;
I\'ll see you on election day.;
remember, you don\'t have a choice.;
remember, I know where you live.;
save those pennies!;
remember, buy all our playsets and toys!;
Big Brother is watching...;
}
"""
