port module Main exposing (..)

import Bootstrap.Button as Button
import Bootstrap.Card as Card
import Bootstrap.Card.Block as Block
import Bootstrap.Form.Input as Input
import Bootstrap.Grid as Grid
import Bootstrap.Navbar as Navbar
import Browser exposing (..)
import Browser.Events as Events
import Browser.Navigation as Navigation
import Html exposing (..)
import Html.Attributes exposing (..)
import Time exposing (..)
import Url
import Url.Builder as Builder
import Url.Parser exposing ((</>), Parser, oneOf, parse, string)



-- Helpers


chooseOne : Bool -> a -> a -> a
chooseOne test true false =
    if test then
        true

    else
        false



-- URL Parsing


type Route
    = HomeRoute
    | RoomRoute String
    | MemberRoute String String


matchers : Parser (Route -> a) a
matchers =
    oneOf
        [ Url.Parser.map HomeRoute Url.Parser.top
        , Url.Parser.map RoomRoute string
        , Url.Parser.map MemberRoute (string </> string)
        ]


parseLocation : Url.Url -> Route
parseLocation location =
    case parse matchers location of
        Just route ->
            route

        Nothing ->
            HomeRoute



-- Ports


port console : String -> Cmd msg


port join : { roomname : String, membername : String } -> Cmd msg


port wantTea : () -> Cmd msg


port notea : () -> Cmd msg


port notify : { message : String, onclick : Bool } -> Cmd msg


port connect : (() -> msg) -> Sub msg


port disconnect : (() -> msg) -> Sub msg


port joined : (People -> msg) -> Sub msg


port roundstarted : (RoundStartedArgs -> msg) -> Sub msg


port wantingtea : (WantingTeaArgs -> msg) -> Sub msg


port yourid : (Person -> msg) -> Sub msg


port roundcomplete : (RoundCompleteArgs -> msg) -> Sub msg


port notificatinClicked : (() -> msg) -> Sub msg



-- MODEL


type alias RoundCompleteArgs =
    { teamaker : Maybe Person
    , teafor : People
    }


type alias WantingTeaArgs =
    { wantingtea : People
    , timeleft : Int
    }


type alias RoundStartedArgs =
    { timeleft : Int
    }


type alias Model =
    { makingTea : Maybe Person
    , peopleInRoom : People
    , peopleInRound : People
    , me : Maybe Person
    , name : String
    , room : String
    , state : State
    , timeLeft : Int
    , inRound : Bool
    , teaFor : People
    , teamaker : Maybe Person
    , host : String
    , screenSize : ScreenSize
    , navState : Navbar.State
    , navKey : Navigation.Key
    }


type State
    = NotConnected
    | PreJoined
    | Joined


type alias People =
    List Person


type alias Person =
    { id : String
    , name : String
    }


getRoomName : Route -> String
getRoomName route =
    case route of
        HomeRoute ->
            ""

        RoomRoute room ->
            room

        MemberRoute room _ ->
            room


getMemberName : Route -> String
getMemberName route =
    case route of
        HomeRoute ->
            ""

        RoomRoute _ ->
            ""

        MemberRoute _ name ->
            name


init : () -> Url.Url -> Navigation.Key -> ( Model, Cmd Msg )
init _ location key =
    let
        route =
            parseLocation location

        room =
            getRoomName route

        name =
            getMemberName route

        protocol =
            case location.protocol of
                Url.Http ->
                    "http"

                Url.Https ->
                    "https"

        host =
            protocol ++ "//" ++ location.host ++ "/"

        ( navState, navCmd ) =
            Navbar.initialState NavMsg
    in
    ( Model Nothing [] [] Nothing name room NotConnected 0 False [] Nothing host Desktop navState key
    , navCmd
    )



-- UPDATE


type ScreenSize
    = Phone
    | Tablet
    | Desktop
    | BigDesktop


getScreenSize : Int -> Int -> ScreenSize
getScreenSize width _ =
    if width <= 600 then
        Phone

    else if width <= 1200 then
        Tablet

    else if width <= 1800 then
        Desktop

    else
        BigDesktop


type Msg
    = NoOp
    | NoOp1 Bool
    | ChangeName String
    | ChangeRoom String
    | Connected ()
    | DisConnect ()
    | JoinRoom
    | JoinedRoom People
    | WaterMe
    | NoThanks
    | NotificatinClicked ()
    | ProcessSocket String
    | RoundStarted RoundStartedArgs
    | WantingTea WantingTeaArgs
    | YourId Person
    | RoundComplete RoundCompleteArgs
    | Tick Time.Posix
    | OnLocationChange Url.Url
    | SetScreenSize Int Int
    | KeyPressed Int
    | NavMsg Navbar.State
    | UrlChange UrlRequest


canJoin : { a | name : String, room : String } -> Bool
canJoin model =
    String.length model.name > 0 && String.length model.room > 0


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        NavMsg state ->
            ( { model | navState = state }
            , Cmd.none
            )

        NoOp1 _ ->
            ( model, Cmd.none )

        OnLocationChange location ->
            let
                route =
                    parseLocation location

                room =
                    getRoomName route

                name =
                    getMemberName route

                me =
                    { name = name, id = "" }

                protocol =
                    case location.protocol of
                        Url.Http ->
                            "http"

                        Url.Https ->
                            "https"

                host =
                    protocol ++ "//" ++ location.host ++ "/"

                newModel =
                    { model | room = room, name = name, me = Just me, host = host }

                cmd =
                    if canJoin newModel then
                        join { roomname = model.room, membername = model.name }

                    else
                        Cmd.none
            in
            ( newModel, cmd )

        ChangeName name ->
            ( { model | name = name }, Cmd.none )

        ChangeRoom room ->
            ( { model | room = room }, Cmd.none )

        Connected _ ->
            let
                me =
                    { name = model.name, id = "" }

                cmd =
                    if canJoin model then
                        join { roomname = model.room, membername = model.name }

                    else
                        Cmd.none

                newMod =
                    if canJoin model then
                        { model | state = PreJoined, me = Just me }

                    else
                        { model | state = PreJoined }
            in
            ( newMod, cmd )

        DisConnect _ ->
            ( { model | state = NotConnected }, Cmd.none )

        JoinRoom ->
            let
                me =
                    { name = model.name, id = "" }

                cmd =
                    if canJoin model then
                        Navigation.pushUrl model.navKey <| Builder.absolute [ model.room, model.name ] []

                    else
                        Cmd.none

                newMod =
                    if canJoin model then
                        { model | me = Just me }

                    else
                        model
            in
            ( newMod, cmd )

        JoinedRoom people ->
            ( { model | peopleInRoom = people, state = Joined }, Cmd.none )

        WaterMe ->
            ( { model | inRound = True }, wantTea () )

        NoThanks ->
            ( { model | inRound = True }, notea () )

        ProcessSocket _ ->
            ( model, Cmd.none )

        RoundStarted info ->
            let
                cmd =
                    notify { message = "A new round of tea has started.", onclick = True }
            in
            ( { model | timeLeft = info.timeleft, inRound = False, peopleInRound = [] }, cmd )

        WantingTea info ->
            let
                inRound =
                    isChecked (getId model.me) info.wantingtea

                newModel =
                    { model | inRound = inRound, timeLeft = info.timeleft, peopleInRound = info.wantingtea }
            in
            ( newModel, Cmd.none )

        YourId person ->
            ( { model | me = Just person }, Cmd.none )

        RoundComplete info ->
            ( { model | timeLeft = 0, inRound = False, teaFor = info.teafor, peopleInRound = [], teamaker = info.teamaker }, notify { message = "Tea made by : " ++ getName info.teamaker, onclick = False } )

        Tick _ ->
            let
                newModel =
                    if model.timeLeft > 0 then
                        { model | timeLeft = model.timeLeft - 1000 }

                    else
                        model
            in
            ( newModel, Cmd.none )

        NotificatinClicked _ ->
            ( model, wantTea () )

        SetScreenSize width height ->
            ( { model | screenSize = getScreenSize width height }, Cmd.none )

        KeyPressed key ->
            let
                cmd =
                    if key == 13 && canJoin model then
                        Navigation.pushUrl model.navKey <| Builder.absolute [ model.room, model.name ] []

                    else
                        Cmd.none
            in
            ( model, cmd )

        UrlChange urlRequest ->
            case urlRequest of
                Internal url ->
                    ( model
                    , Navigation.pushUrl model.navKey (Url.toString url)
                    )

                External url ->
                    ( model
                    , Navigation.load url
                    )



-- VIEW
-- type Styles
--     = None
--     | Main
--     | NavOption
--     | PreJoinedStyle
--     | FieldDark
--     | Field
--     | DisabledButton
--     | Button
--     | MaterialFont
--     | PersonStyle
--     | H1
--     | H2
-- materialFont : List Font.Font
-- materialFont =
--     [ Font.typeface "Material Icons"
--     ]
-- sansSerif : List Font.Font
-- sansSerif =
--     [ Font.typeface "helvetica"
--     , Font.typeface "arial"
--     , Font.typeface "sans-serif"
--     ]
-- styleNone =
--     []
-- -- It's handy to have a blank style
-- styleMain =
--     [ Border.width 1 -- set all border widths to 1 px.
--     , text darkCharcoal
--     , Background.color white
--     , Border.color lightgrey
--     , Font.typeface sansSerif
--     , Font.size 16
--     ]
-- styleNavOption =
--     [ Font.size 16
--     , Font.typeface sansSerif
--     ]
-- stylePreJoinedStyle =
--     [ Border.width 1 -- set all border widths to 1 px.
--     , text darkCharcoal
--     , Background.color lightblue
--     , Border.color lightgrey
--     , Font.typeface sansSerif
--     , Font.size 16
--     ]
-- styleField =
--     [ Border.rounded 5
--     , Border.width 1
--     , Border.solid
--     , Border.color lightgrey
--     ]
-- styleFieldDark =
--     [ Border.rounded 5
--     , Border.width 1
--     , Border.solid
--     , Border.color darkgray
--     ]
-- styleH2 =
--     [ Font.typeface sansSerif
--     , Font.size 20
--     ]
-- styleH1 =
--     [ Font.typeface sansSerif
--     , Font.size 24
--     ]
-- styleDisabledButton =
--     [ Border.rounded 5
--     , Border.width 0
--     , Border.solid
--     , Border.color lightblue
--     , Background.color lightblue
--     ]
-- styleButton =
--     [ Border.rounded 5
--     , Border.width 1
--     , Border.solid
--     , Border.color lightgreen
--     , Background.color lightgreen
--     ]
-- styleMaterialFont =
--     [ Font.typeface materialFont
--     , Font.size 24
--     , text.color lightgreen
--     ]
-- stylePersonStyle =
--     [ Border.rounded 5
--     , Border.width 1
--     , Border.solid
--     , Border.color lightgreen
--     , Background.color lightgreen
--     , text.color black
--     ]
-- onKeyUp tagger =
--     on "keyup" (Json.map tagger keyCode)


empty : Html msg
empty =
    text ""


view : Model -> Document Msg
view model =
    Document ""
        [ div []
            [ navigation model
            , mainView model
            ]
        ]


isLoggedin : Model -> a -> a -> a
isLoggedin model loggedin loggedout =
    case model.state of
        Joined ->
            loggedin

        _ ->
            loggedout


navigation : Model -> Html Msg
navigation model =
    Navbar.config NavMsg
        |> Navbar.withAnimation
        |> Navbar.container
        |> Navbar.brand [ href "#" ] [ img [ src "/img/logo.png", style "width" "60px", style "width" "60px" ] [ text "Tea Round" ] ]
        |> Navbar.customItems [ isLoggedin model (showShare model) (Navbar.customItem (login model)) ]
        |> Navbar.view model.navState



-- showShare : Model -> CustomItem Msg


showShare : { a | state : State, host : String, room : String } -> Navbar.CustomItem msg
showShare model =
    case model.state of
        Joined ->
            Navbar.textItem
                [ attribute "data-clipboard-text" (String.append model.host model.room)
                ]
                [ text "Share Room" ]

        _ ->
            Navbar.textItem
                [ attribute "data-clipboard-text" (String.append model.host model.room)
                ]
                [ text "" ]



-- mainView : Model -> Model -> Html Msg


mainView : Model -> Html Msg
mainView model =
    case model.state of
        NotConnected ->
            Grid.container []
                [ Grid.row []
                    [ Grid.col []
                        [ text "Waiting for Connection"
                        ]
                    ]
                ]

        PreJoined ->
            Grid.container []
                [ Grid.row []
                    [ Grid.col []
                        [ blurb
                        ]
                    ]
                ]

        Joined ->
            waterMe model


blurb : Html Msg
blurb =
    Grid.container []
        [ Grid.row []
            [ Grid.col []
                [ h1 [] [ text "Who's tea round is it?" ]
                , p [] [ text "Have you ever worked in an office where everyone is suppose to take turns in making the drinks?" ]
                , p [] [ text "Are you scared seeing if anyone else wants a drink as it'll mean you will have to make everyone drinks again? So you don't ask and slowly dehydrate" ]
                , h2 [] [ text "Then Tea Round is for you." ]
                , p []
                    [ text "Just login with your name and a unique name for the room. Then click on "
                    , text "Share"
                    , text " In the top right corner, which will copy your room's address to the clipboard, you can then paste this to the rest of your office workers (Vie email, Snapchat, Skype etc)"
                    ]
                , p []
                    [ text "When it's time for a drink, just Hit"
                    , text " Time for tea "
                    , text "and everyone in the room will be notified it's time for a drink. "
                    , text "Everyone who wants a drink clicks "
                    , text " Time for tea "
                    , text "Once the timer reaches zero, One of the people who want tea will be randomaly selected to go make it."
                    ]
                , p []
                    [ text "This is a public server with no actual logins. So make your room name random." ]
                ]
            ]
        ]


login : { a | name : String, room : String } -> Html Msg
login model =
    Grid.container []
        [ Grid.row []
            [ Grid.col []
                [ Input.text
                    [ Input.id "room"
                    , Input.onInput ChangeRoom
                    , Input.value model.room
                    , Input.placeholder "Room"
                    ]
                ]
            , Grid.col []
                [ Input.text
                    [ Input.id "name"
                    , Input.onInput ChangeName
                    , Input.value model.name
                    , Input.placeholder "Name"
                    ]
                ]
            , Grid.col []
                [ Button.button
                    [ Button.onClick JoinRoom ]
                    [ text "Join" ]
                ]
            ]
        ]


showTimer : Int -> Html Msg
showTimer timeleft =
    if timeleft > 0 then
        span [] [ text ("Time Left : " ++ String.fromInt (floor (toFloat timeleft / 1000)) ++ " seconds") ]

    else
        empty


waterMe : Model -> Html Msg
waterMe model =
    Grid.container []
        [ Grid.row []
            [ Grid.col []
                [ Card.deck
                    [ Card.config
                        [ Card.outlineInfo ]
                        |> Card.headerH3 []
                            [ text ("Room: " ++ model.room)
                            , showTimer model.timeLeft
                            ]
                        |> Card.block []
                            [ Block.quote []
                                [ showPeople
                                    model.peopleInRound
                                    model.peopleInRoom
                                ]
                            ]
                    , Card.config
                        [ Card.outlineInfo ]
                        |> Card.headerH3 []
                            [ text ("Member: " ++ model.name) ]
                        |> Card.block []
                            [ Block.quote []
                                [ callToAction model ]
                            ]
                    , Card.config
                        [ Card.outlineInfo ]
                        |> Card.headerH3 []
                            [ text ("Tea Made by: " ++ getName model.teamaker) ]
                        |> Card.block []
                            [ Block.quote []
                                [ showRound model ]
                            ]
                    ]
                ]
            ]
        ]


isChecked : String -> People -> Bool
isChecked person people =
    List.member person (List.map .id people)


getName : Maybe Person -> String
getName maybePerson =
    case maybePerson of
        Just person ->
            person.name

        Nothing ->
            "Unknown"


getId : Maybe Person -> String
getId maybePerson =
    case maybePerson of
        Just person ->
            person.id

        Nothing ->
            ""


callToAction : { a | inRound : Bool } -> Html Msg
callToAction model =
    let
        choose =
            chooseOne model.inRound
    in
    Button.button
        [ Button.onClick (choose NoThanks WaterMe), Button.large ]
        [ text (choose "I don't want tea" "Time for tea") ]


showRound :
    { a | teaFor : List Person, teamaker : Maybe Person }
    -> Html Msg
showRound model =
    let
        makerElement =
            case model.teamaker of
                Just person ->
                    div []
                        [ text "The lucky winner is : "
                        , showPersonName person
                        ]

                Nothing ->
                    empty
    in
    div []
        [ makerElement
        , div []
            (List.map showPersonName model.teaFor)
        ]



-- showPersonName : Person -> Element Styles variation msg


showPersonName : { a | name : String } -> Html msg
showPersonName person =
    span [] [ text (.name person) ]



-- showPeople : People -> People -> Element Styles variation msg


showPeople : People -> List { a | id : String, name : String } -> Html msg
showPeople peopleInRound peopleInRoom =
    div
        []
        (List.map (showPerson peopleInRound) peopleInRoom)



-- showPerson : People -> Person -> Element Styles variation msg


showPerson : People -> { a | id : String, name : String } -> Html msg
showPerson people person =
    div []
        [ text (chooseOne (isChecked person.id people) (String.fromChar '✔') "")
        , showPersonName person
        ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ joined JoinedRoom
        , roundstarted RoundStarted
        , wantingtea WantingTea
        , yourid YourId
        , roundcomplete RoundComplete
        , connect Connected
        , disconnect DisConnect
        , notificatinClicked NotificatinClicked
        , Time.every 1000 Tick
        , Events.onResize SetScreenSize
        , Navbar.subscriptions model.navState NavMsg
        ]



-- main : Program Flags Model Msg


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlChange = OnLocationChange
        , onUrlRequest = UrlChange
        }
