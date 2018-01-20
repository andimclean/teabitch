module Main exposing (..)

import Color
import WebSocket exposing (..)
import Element exposing (..)
import Element.Attributes exposing (..)
import Element.Input as Input
import Element.Events exposing (..)
import Json.Encode exposing (..)
import Json.Decode exposing (..)
import Html
import Style exposing (..)
import Style.Border as Border
import Style.Color as Color
import Style.Font as Font

-- Helpers

chooseOne: Bool -> a -> a -> a
chooseOne test true false =
    if test then
      true
    else
      false

-- MODEL



type alias Model =
    {  makingTea : Maybe Person
    ,  peopleInRoom : People
    ,  peopleInRound : People
    ,  me : Maybe Person
    ,  name :  String
    ,  room :  String
    ,  state : State
    
    }

type State 
    = PreJoined
    | Joined
    | CallForTea
    | BitchChoosen

type alias People = 
    List Person

type alias Person = 
    { id : String 
    , name: String
    }

init : ( Model, Cmd Msg )
init =
    ( Model Nothing [] [] Nothing "" "" PreJoined
    , Cmd.none
    )

socketUrl : String
socketUrl = "ws://http://192.168.86.34:3000/"


-- UPDATE


type Msg
    = NoOp
    | NoOp1 Bool
    | ChangeName String
    | ChangeRoom String
    | JoinRoom
    | WaterMe
    | ProcessSocket String

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
        ChangeName name -> 
            ( { model | name = name} , Cmd.none)
        ChangeRoom room ->
            ( { model | room = room} , Cmd.none)
        JoinRoom ->
          let
            me = {name = model.name}
            cmd = 
              if canJoin model then
                send socketUrl JS
              else 
                Cmd.none
            newMod = 
              if canJoin model then
                {model | me = Just me, state = Joined , peopleInRoom = [me]}
              else 
                model
          in
            (newMod , cmd)
        WaterMe ->
         let
          newInRound = 
            case model.me of
              Nothing -> model.peopleInRound
              Just me -> me :: model.peopleInRound
         in             
          ( {model | peopleInRound = newInRound}, Cmd.none)
        ProcessSocket message ->
          ( model, Cmd.none )
         

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
        , style DisabledButton
            [ Border.rounded 5
            , Border.all 1
            , Border.solid
            , Color.border Color.lightGrey
            , Color.background Color.lightGrey
            ]
        , style Button
            [ Border.rounded 5
            , Border.all 1
            , Border.solid
            , Color.border Color.lightBlue
            , Color.background Color.lightBlue
            ]
        ]

view : Model -> Html.Html Msg
view model =
    Element.layout stylesheet <|
        column None
            [ height (percent 100) ]
            [ navigation
            , el None [ center, width (px 800), height (percent 100)] <|
              column Main 
                [spacing 30, paddingTop 50, paddingBottom 50, height (percent 100)]
                ( List.concat [mainView model])
            ]

navigation : Element Styles variation msg
navigation = 
    row None
        [  spread, paddingXY 80 20]
        [  image None [width (px 40), height (px 40)] {src = "img/logo.png", caption = ""}
        ,  row None 
            [ spacing 20, alignBottom]
            [ el NavOption [] (text "share")
            , el NavOption [] (text "about")
            , el NavOption [] (text "user profile")
            ]
        ]

mainView : Model-> List (Element Styles variation Msg)
mainView model =
    case model.state of
        PreJoined ->
            [el None [center, width (px 400)] <|
              column None
               [spacing 20]
               [Input.text Field 
                    [padding 10]
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
                    [padding 10]
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
            ]
        Joined -> waterMe model            
        CallForTea -> waterMe model
        BitchChoosen -> []


waterMe model = 
    [ grid None
        [ spacing 20, height (percent 100)]
        { columns = [ percent 20 , fill]
        , rows = 
            [ px 40 
            , fill 
            , px 40
            ]
        , cells = 
            [ cell 
                { start = ( 0, 0)
                , width = 2
                , height = 1
                , content = el None [] (text ("Room: " ++ model.room))
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
                , content = (showPeople model)
                }
            ]
        }
    ]

isChecked: String -> People -> Bool
isChecked person people = List.member person (List.map .name people)

callToAction model = 
    Element.button ( chooseOne (isChecked model.name model.peopleInRound) DisabledButton Button)
        [onClick WaterMe, height (percent 100)]
        (el None [] (text "Water me"))


showPeople: Model -> Element Styles variation msg
showPeople model = 
    column None
      []
      ( List.map (showPerson model.peopleInRound) model.peopleInRoom )

showPerson: People -> Person ->  Element Styles variation msg
showPerson people person =
    row None
        [spacing 20]
        [ el FieldDark [ width (px 20), height (px 20)] (text (chooseOne (isChecked person.name people) "x" ""))
        , el None [] (text (.name person))
        ]
-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    listen socketUrl ProcessSocket


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
