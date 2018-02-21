module Main exposing (..)

import Html exposing (Html, div, button, text)
import Html.Events exposing (onClick)
import Time exposing (Time, second)
import Maybe exposing (withDefault)
import Window exposing (Size, size)
import Svg exposing (svg, circle, line, polyline)
import Svg.Attributes exposing (..)
import Task exposing (perform)
import Slider exposing (..)


main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { part : Particle
    , dt : Time
    , nextDt : Time
    , t : Time
    , status : Status
    , wWidth : Int
    , wHeight : Int
    }


type Status
    = Idle
    | Running


type alias Position =
    Float


type alias Velocity =
    Float


type alias Particle =
    { pos : List Position, vel : List Velocity }


getX : Particle -> Position
getX p =
    withDefault 0 <| List.head <| .pos p


getV : Particle -> Velocity
getV p =
    withDefault 0 <| List.head <| .vel p



-- INIT


init : ( Model, Cmd Msg )
init =
    ( Model (Particle [ 0.5 ] [ 0 ]) 0.05 0.05 0 Idle 0 0, getSize )


getSize : Cmd Msg
getSize =
    perform GetSize size



-- UPDATE


type Msg
    = Start
    | Stop
    | Tick Time
    | GetSize Size
    | SliderUpdate Float
    | Drag


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Start ->
            ( { model
                | status = Running
                , part = Particle [ 0.5 ] [ -1 ]
                , t = 0
                , dt = model.nextDt
              }
            , Cmd.none
            )

        Stop ->
            ( { model | status = Idle }, Cmd.none )

        Tick time ->
            case model.status of
                Idle ->
                    ( model, Cmd.none )

                Running ->
                    if model.t > 5 then
                        ( { model | status = Idle }, Cmd.none )
                    else
                        ( { model
                            | part = evolve model.part model.t model.dt
                            , t = model.t + model.dt
                          }
                        , getSize
                        )

        GetSize s ->
            ( { model | wWidth = s.width, wHeight = s.height }, Cmd.none )

        SliderUpdate dt ->
            ( { model | nextDt = dt }, Cmd.none )

        Drag ->
            ( model, Cmd.none )


diffEq : Position -> Velocity -> Time -> Time -> ( Position, Velocity )
diffEq x v t dt =
    ( x + v * dt, -2 * x )


evolve : Particle -> Time -> Time -> Particle
evolve p t dt =
    let
        ( x, v ) =
            diffEq (getX p) (getV p) t dt
    in
        { p | pos = x :: p.pos, vel = v :: p.vel }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Time.every (model.dt * second) Tick ]



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ button [ onClick Start ] [ text "Start" ]
        , button [ onClick Stop ] [ text "Stop" ]
        , viewSlider
        , div [] [ text <| "y = " ++ toString (getX model.part) ]
        , div [] [ text <| "v = " ++ toString (getV model.part) ]
        , div [] [ text <| "t = " ++ toString model.t ]
        , div [] [ text <| "dt = " ++ toString model.nextDt ]
        , svg
            [ width (toString model.wWidth)
            , height (toString model.wHeight)
            , stroke "black"
            ]
            [ circleParam model
            , line
                [ x1 "0"
                , x2 (toString model.wWidth)
                , y1 (toString (model.wHeight // 2))
                , y2 (toString (model.wHeight // 2))
                ]
                []
            , polyline
                [ points (getPath model), fill "none" ]
                []
            ]
        ]


viewSlider : Html Msg
viewSlider =
    props2view [ MinVal 0, MaxVal 1, Step 0.01, onChange SliderUpdate ]


circleParam : Model -> Html Msg
circleParam model =
    let
        ( h, w ) =
            ( toFloat model.wHeight, toFloat model.wWidth )
    in
        circle
            [ cy (toString <| (\x -> h / 2 * (1 - x / 3)) <| getX model.part)
            , cx (toString <| (\t -> w * t / 5) <| model.t)
            , r "10"
            ]
            []


getPath : Model -> String
getPath m =
    let
        ( h, w ) =
            ( toFloat m.wHeight, toFloat m.wWidth )

        comb x ( t, s ) =
            ( t - m.dt, s ++ toString (w * t / 5) ++ "," ++ toString (h / 2 * (1 - x / 3)) ++ " " )
    in
        Tuple.second <| List.foldl comb ( m.t, "" ) m.part.pos
