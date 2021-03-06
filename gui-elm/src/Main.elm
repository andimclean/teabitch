port module Main exposing (..)

import Color
import Element exposing (..)
import Element.Attributes exposing (..)
import Element.Input as Input
import Element.Events exposing (..)
import Html
import Json.Decode as Json
import Navigation exposing (..)
import Style exposing (..)
import Style.Border as Border
import Style.Color as Color
import Style.Font as Font
import Time exposing (..)
import UrlParser exposing (..)
import Window exposing (resizes)


-- Helpers

chooseOne: Bool -> a -> a -> a
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
        [ UrlParser.map HomeRoute UrlParser.top
        , UrlParser.map RoomRoute (string)
        , UrlParser.map MemberRoute (string </> string)
        ]

parseLocation : Location -> Route
parseLocation location =
    case (parsePath matchers location) of
        Just route ->
            route

        Nothing ->
            HomeRoute    

-- Ports
port console: String -> Cmd msg

port join : {roomname: String, membername: String} -> Cmd msg
port wantTea : () ->Cmd msg
port notea: () -> Cmd msg
port notify: {message: String , onclick: Bool} -> Cmd msg

port connect : ( () -> msg) -> Sub msg
port disconnect : ( () -> msg) -> Sub msg
port joined: ( People -> msg) -> Sub msg
port roundstarted: ( RoundStartedArgs -> msg ) -> Sub msg
port wantingtea: (WantingTeaArgs -> msg) -> Sub msg
port yourid: (Person -> msg) -> Sub msg
port roundcomplete: (RoundCompleteArgs -> msg) -> Sub msg
port notificatinClicked: ( () -> msg) -> Sub msg

-- MODEL

type alias RoundCompleteArgs =
  {
    teamaker: Maybe Person
  , teafor: People
  }
type alias WantingTeaArgs =
  {
    wantingtea: People
  , timeleft: Int
  }

type alias RoundStartedArgs = 
  {
    timeleft: Int
  }

type alias Model =
    { makingTea : Maybe Person
    , peopleInRoom : People
    , peopleInRound : People
    , me : Maybe Person
    , name :  String
    , room :  String
    , state : State
    , timeLeft : Int
    , inRound : Bool
    , teaFor: People
    , teamaker: Maybe Person
    , host : String
    , screenSize : ScreenSize
    }

type State 
    = NotConnected
    | PreJoined
    | Joined

type alias People = 
    List Person

type alias Person = 
    { id : String 
    , name: String
    }


getRoomName: Route -> String
getRoomName route = 
  case route of
    HomeRoute -> ""
    RoomRoute room -> room
    MemberRoute room name -> room
      
getMemberName: Route -> String
getMemberName route = 
  case route  of
    HomeRoute -> ""
    RoomRoute room -> ""
    MemberRoute room name -> name
      

init :Navigation.Location -> ( Model, Cmd Msg )
init location =
    let
      route = parseLocation location
      room = getRoomName route
      name = getMemberName route
      host = location.protocol ++ "//" ++ location.host ++ "/"
    in
        
    ( Model Nothing [] [] Nothing name room NotConnected 0 False [] Nothing host Desktop
    , Cmd.none
    )


-- UPDATE
type ScreenSize
    = Phone
    | Tablet
    | Desktop
    | BigDesktop

getScreenSize : Window.Size -> ScreenSize
getScreenSize size =
    if size.width <= 600 then
        Phone
    else if size.width <= 1200 then
        Tablet
    else if size.width <= 1800 then
        Desktop
    else
        BigDesktop

type Msg
    = NoOp
    | NoOp1 Bool
    | ChangeName String
    | ChangeRoom String
    | Connected  ()
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
    | Tick Time
    | OnLocationChange Navigation.Location
    | SetScreenSize Window.Size
    | KeyPressed Int

canJoin : { a | name : String, room : String } -> Bool
canJoin model = 
    String.length model.name > 0 && String.length model.room > 0 

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )
        NoOp1 val ->
            ( model, Cmd.none )
        OnLocationChange location ->
          let
            route = parseLocation location
            room = getRoomName route
            name = getMemberName route
            me = {name = name, id = ""}
            host = location.protocol ++ "//" ++ location.host ++ "/"
            newModel = {model | room = room , name = name, me = Just me , host = host}
            cmd = 
            if canJoin newModel then
              join {roomname = model.room, membername = model.name}
             else 
               Cmd.none
         in
            ( newModel , cmd )
        ChangeName name -> 
            ( { model | name = name} , Cmd.none)
        ChangeRoom room ->
            ( { model | room = room} , Cmd.none)
        Connected _ ->
        let
          me = {name = model.name, id = ""}
          cmd = 
            if canJoin model then
              join {roomname = model.room, membername = model.name}
            else 
              Cmd.none
          newMod = 
            if canJoin model then
              {model | state = PreJoined, me = Just me}
            else 
              {model | state = PreJoined}
        in
          (newMod , cmd)
        DisConnect _ ->
           ( {model | state = NotConnected}, Cmd.none)
        JoinRoom ->
          let
            me = {name = model.name, id = ""}
            cmd = 
              if canJoin model then
                Navigation.newUrl <| "/"++ model.room ++ "/"++ model.name
              else 
                Cmd.none
            newMod = 
              if canJoin model then
                {model | me = Just me}
              else 
                model
          in
            (newMod , cmd)
        JoinedRoom people ->
         ( { model | peopleInRoom = people , state = Joined} , Cmd.none)
        WaterMe ->
          ( { model | inRound = True}, wantTea ())
        NoThanks -> 
          ( { model | inRound = True}, notea ())
        ProcessSocket message ->
          ( model, Cmd.none )
        RoundStarted info -> 
          let
              cmd = 
                notify {message = "A new round of tea has started.", onclick = True}
          in              
            ({model | timeLeft = info.timeleft , inRound = False , peopleInRound = [] }, cmd)
        WantingTea info ->
            let
                inRound = isChecked (getId model.me) info.wantingtea 
                newModel = {model | inRound = inRound, timeLeft = info.timeleft, peopleInRound = info.wantingtea}
            in                             
                (newModel, Cmd.none)
        YourId person ->
            ( { model | me = Just person}, Cmd.none )
        RoundComplete info -> 
            ( { model | timeLeft = 0, inRound = False, teaFor = info.teafor , peopleInRound = [], teamaker = info.teamaker}, notify {message = ("Tea made by : " ++ (getName info.teamaker))  , onclick = False})
        Tick _ ->
          let
            newModel =
              if model.timeLeft > 0 then
                {model | timeLeft = (model.timeLeft) - 1000 }
              else
                model
          in
            (newModel, Cmd.none)
        NotificatinClicked _ ->
          ( model, wantTea () )
        SetScreenSize size ->
          ({ model | screenSize = getScreenSize size } , Cmd.none)
        KeyPressed key ->
          let
              cmd = if (key == 13 && canJoin model) then
                  Navigation.newUrl <| "/"++ model.room ++ "/"++ model.name
                else
                  Cmd.none
          in
              
            ( model, cmd )

-- VIEW

type Styles
    = None
    | Main 
    | NavOption
    | PreJoinedStyle
    | FieldDark
    | Field
    | DisabledButton
    | Button
    | MaterialFont
    | PersonStyle
    | H1
    | H2

materialFont: List Font
materialFont = 
  [
      Font.font "Material Icons"
  ]

sansSerif : List Font
sansSerif = 
    [ Font.font "helvetica"
    , Font.font "arial"
    , Font.font "sans-serif"
    ]

stylesheet : StyleSheet Styles variation
stylesheet =
    Style.styleSheet
        [ style None [] -- It's handy to have a blank style
        , style Main
            [ Border.all 1 -- set all border widths to 1 px.
            , Color.text Color.darkCharcoal
            , Color.background Color.white
            , Color.border Color.lightGrey
            , Font.typeface sansSerif
            , Font.size 16
            , Font.lineHeight 1.3 -- line height, given as a ratio of current font size.
            ]
        , style NavOption
            [ Font.size 16
            , Font.typeface sansSerif
            ]
        , style PreJoinedStyle
            [ Border.all 1 -- set all border widths to 1 px.
            , Color.text Color.darkCharcoal
            , Color.background Color.lightBlue
            , Color.border Color.lightGrey
            , Font.typeface sansSerif
            , Font.size 16
            , Font.lineHeight 1.3 
            ]
        , style Field
            [ Border.rounded 5
            , Border.all 1
            , Border.solid
            , Color.border Color.lightGrey
            ]
        , style FieldDark
            [ Border.rounded 5
            , Border.all 1
            , Border.solid
            , Color.border Color.darkGray
            ]
        , style H2
           [
              Font.typeface sansSerif
            , Font.size 20
           ]
        , style H1
           [
              Font.typeface sansSerif
            , Font.size 24
           ]
        , style DisabledButton
            [ Border.rounded 5
            , Border.all 0
            , Border.solid
            , Color.border Color.lightBlue
            , Color.background Color.lightBlue
            ]
        , style Button
            [ Border.rounded 5
            , Border.all 1
            , Border.solid
            , Color.border Color.lightGreen
            , Color.background Color.lightGreen
            ]
        , style MaterialFont
           [
             Font.typeface materialFont
           , Font.size 24
           , Font.lineHeight 1
           , Color.text Color.lightGreen
           ]
        , style PersonStyle
          [
            Border.rounded 5
          , Border.all 1
          , Border.solid
          , Color.border Color.lightGreen
          , Color.background Color.lightGreen
          , Color.text Color.black
          ]        
        ]


onKeyUp tagger =
    on "keyup" (Json.map tagger keyCode)

view : Model -> Html.Html Msg
view model =
    Element.layout stylesheet <|
        column None
            [ height (percent 100) , width (percent 100) ]
            [ navigation model
            , el None [ center , width (percent 100) ] <|
              column None 
                [spacing 30, paddingTop 50, paddingBottom 50, height (percent 100), width (percent 100)]
                ( List.concat [mainView model])
            ]

navigation : Model -> Element Styles variation msg
navigation model = 
    row None
        [  spread, paddingXY 80 20]
        [  image None [width (px 60), height (px 60)] {src = "/img/logo.png", caption = ""}
        ,  row None 
            [ spacing 20, alignBottom]
            [ (showShare model)
            ]
        ]

showShare : Model -> Element Styles variation msg
showShare model =
  case model.state of
    Joined -> 
      button None 
        [Element.Attributes.class "copy-button" , Element.Attributes.attribute "data-clipboard-text" (toString (model.host ++ model.room))]
        (el NavOption [] (text "Share Room"))
    _ -> empty
        
  

mainView : Model-> List (Element Styles variation Msg)
mainView model =
    case model.state of
        NotConnected -> 
            [el None [center, width (px 400)] <|
              row None
                [center , verticalCenter]
                [
                    Element.button None
                      []
                      (el None [] (text "Waiting for Connection"))
                ]
            ]
        PreJoined ->
            [el None [center] <|
              row None
                [verticalSpread]
                [(login model)
                , (blurb)]
            ]
        Joined -> waterMe model

blurb : Element Styles variation msg
blurb = 
    column None
    [paddingLeft 30,  spacing 20]
    [
      Element.h1 H1 [] (bold "Who's tea round is it?")
    , Element.paragraph None [] [(text "Have you ever worked in an office where everyone is suppose to take turns in making the drinks?")]
    , Element.paragraph None [] [(text "Are you scared seeing if anyone else wants a drink as it'll mean you will have to make everyone drinks again? So you don't ask and slowly dehydrate")]
    , Element.h2 H2 [] (bold "Then Tea Round is for you.")
    , Element.paragraph None []
          [(text "Just login with your name and a unique name for the room. Then click on ")
          , (bold "Share")
          , (text " In the top right corner, which will copy your room's address to the clipboard, you can then paste this to the rest of your office workers (Vie email, Snapchat, Skype etc)")]
    , Element.paragraph None [] [(text "When it's time for a drink, just Hit")
      , (bold " Time for tea ")
      , (text "and everyone in the room will be notified it's time for a drink. ")
      , (text "Everyone who wants a drink clicks ")
      , (bold " Time for tea ")
      , (text "Once the timer reaches zero, One of the people who want tea will be randomal selected to go make it.")
      ]
    , Element.paragraph None [] [(text "This is a public server with no actual logins. So make your room name random.")]
    ]


login : { a | name : String, room : String } -> Element Styles variation Msg
login model = 
    column None
    [spacing 20]
    [Input.text Field 
        [padding 10, onKeyUp KeyPressed]
        { onChange = ChangeName
        , value = model.name
        , label =
            Input.placeholder
            { label = Input.labelAbove (el None [verticalCenter] (text "Name"))
            , text = "Name"
            }
        , options = []
        }
    ,Input.text Field 
        [padding 10 , onKeyUp KeyPressed]
        { onChange = ChangeRoom
        , value = model.room
        , label =
            Input.placeholder
            { label = Input.labelAbove (el None [verticalCenter] (text "Room"))
            , text = "Room"
            }
        , options = []
        }
    , Element.button (chooseOne (canJoin model) Button DisabledButton)
        [ padding 10,  onClick (chooseOne (canJoin model) JoinRoom NoOp)]
            (el None [] (text "Join"))
    ]


showTimer : Int -> Element Styles variation msg
showTimer timeleft =
  if timeleft > 0 then
    el None [] (text ("Time Left : " ++ (toString (floor((toFloat timeleft) / 1000))) ++ " seconds"))
  else
    empty
      

waterMe: Model -> List (Element Styles variation Msg)
waterMe model = 
    [ grid None
        [ spacing 20, height (percent 100), width (percent 100)]
        { columns = [ percent 20 , fill]
        , rows = 
            [ px 40 
            , fill 
            , fill
            , px 40
            ]
        , cells = 
            [ cell 
                { start = ( 0, 0)
                , width = 2
                , height = 1
                , content = row None 
                    [spread]
                    [ el None [] (text ("Room: " ++ model.room))
                    , showTimer model.timeLeft
                    ]
                }
            , cell 
                { start = ( 1, 0)
                , width = 2
                , height = 1
                , content = el None [] (text ("Member: " ++ model.name))
                }
            , cell 
                { start  = ( 1, 1)
                , width  =1
                , height = 1
                , content = (callToAction model)
                }
            , cell 
                { start  = ( 0, 1)
                , width  =1
                , height = 1
                , content = (showPeople model.peopleInRound model.peopleInRoom)
                }
            , cell
                { start = (0,2)
                , width = 2
                , height = 1
                , content = (showRound model)

                }
            ]
        }
    ]

isChecked: String -> People -> Bool
isChecked person people = List.member person (List.map .id people)

getName: Maybe Person -> String
getName maybePerson =
  case maybePerson of
    Just person -> person.name
    Nothing -> "Unknown"

getId: Maybe Person -> String
getId maybePerson = 
    case maybePerson of
        Just person -> person.id            
        Nothing -> ""
            
callToAction : { a | inRound : Bool } -> Element Styles variation Msg
callToAction model = 
    let
      choose = chooseOne (model.inRound)
    in        
      Element.button (choose DisabledButton Button)
        [onClick (choose NoThanks WaterMe), height (percent 100)]
        (el None [] (text ( choose "I don't want tea" "Time for tea")))

showRound
    : { a | teaFor : List Person, teamaker : Maybe Person }
    -> Element Styles variation msg
showRound model = 
    let
        makerElement = 
          case model.teamaker of
              Just person ->
                row None
                  []
                  [
                    el None [] (text "The lucky winner is : ")
                  , showPersonName person
                  ]
              Nothing ->
                empty
    in
      column None 
        []
        [
          makerElement,
          Element.wrappedRow None
            []
            (List.map showPersonName model.teaFor)
        ]
showPersonName: Person -> Element Styles variation msg
showPersonName person = 
    el PersonStyle [paddingLeft 5, paddingRight 5, width (px 100), height (px 20)] (text (.name person))

showPeople: People -> People -> Element Styles variation msg
showPeople peopleInRound peopleInRoom = 
    column None
      []
      ( List.map (showPerson peopleInRound) peopleInRoom )

showPerson: People -> Person ->  Element Styles variation msg
showPerson people person =
    row None
        [spacing 20]
        [ el MaterialFont [spacing 10 , center, verticalCenter, width (px 30), height (px 30), paddingLeft 5] (text (chooseOne (isChecked person.id people) "check" ""))
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
    , every second Tick
    , Window.resizes SetScreenSize
    ]

main : Program Never Model Msg
main =
    Navigation.program OnLocationChange
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }