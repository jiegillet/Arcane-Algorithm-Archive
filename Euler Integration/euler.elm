module Main exposing (..)

import Html exposing (Html, div, button, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Time exposing (Time, second)
import Maybe exposing (withDefault)
import Window exposing (Size, size)
import Svg exposing (svg, circle, line, polyline)
import Svg.Attributes exposing (width, height, stroke, x1, x2, y1, y2, cx, cy, r, points, fill)
import Task exposing (perform)
import Slider exposing (..)
import Hex


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
    , history : List ( Time, Time, Particle )
    }


type alias Position =
    Float


type alias Velocity =
    Float


type alias Particle =
    { pos : List Position, vel : List Velocity }


type Status
    = Idle
    | Running


getX : Particle -> Position
getX p =
    withDefault 0 <| List.head <| .pos p


getV : Particle -> Velocity
getV p =
    withDefault 0 <| List.head <| .vel p



-- INIT


init : ( Model, Cmd Msg )
init =
    ( Model (Particle [ 1 ] [ 0 ]) 0.5 0.5 0 Idle 0 0 [], getSize )


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
                , part = Particle [ 2 ] [ -4 ]
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
                        ( { model
                            | status = Idle
                            , part = evolve model.part model.t model.dt
                            , t = model.t + model.dt
                            , history = ( model.dt, model.t, model.part ) :: model.history
                          }
                        , Cmd.none
                        )
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
    ( x + (-2 * x) * dt, -2 * x )


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
        , div [ style [ ( "color", gradient model.nextDt ) ] ]
            [ text <| "dt = " ++ toString model.nextDt ]
        , svg
            [ width (toString model.wWidth)
            , height (toString model.wHeight)
            , stroke "black"
            ]
            ([ line
                [ x1 "0"
                , x2 (toString model.wWidth)
                , y1 (toString (model.wHeight // 2))
                , y2 (toString (model.wHeight // 2))
                ]
                []
             , line
                [ x1 (toString (model.wWidth // 20))
                , x2 (toString (model.wWidth // 20))
                , y1 "0"
                , y2 (toString model.wHeight)
                ]
                []
             , viewCircle model
             ]
                ++ (plotHistory model)
            )
        ]


viewSlider : Html Msg
viewSlider =
    props2view [ MinVal 0, MaxVal 1, Step 0.01, onChange SliderUpdate ]


scaleX : Int -> Position -> String
scaleX h x =
    toString (toFloat h / 2 * (1 - x / 3))


scaleT : Int -> Time -> String
scaleT w t =
    toString (toFloat w * (0.05 + t / 5))


viewCircle : Model -> Html Msg
viewCircle m =
    circle
        [ cy (scaleX m.wHeight <| withDefault 0 <| List.head <| .pos <| m.part)
        , cx (scaleT m.wWidth m.t)
        , r "10"
        ]
        []


plotPath : Int -> Int -> ( Time, Time, Particle ) -> String
plotPath w h ( dt, tf, particle ) =
    let
        comb x ( t, s ) =
            ( t - dt, s ++ (scaleT w t) ++ "," ++ (scaleX h x) ++ " " )
    in
        Tuple.second <| List.foldl comb ( tf, "" ) particle.pos


plotHistory : Model -> List (Html Msg)
plotHistory m =
    let
        ( w, h ) =
            ( m.wWidth, m.wHeight )
    in
        List.map
            (\( dt, t, p ) ->
                polyline
                    [ stroke "black"
                    , fill "none"
                    , stroke (gradient dt)
                    , points (plotPath w h ( dt, t, p ))
                    ]
                    []
            )
            (( m.dt, m.t, m.part ) :: m.history)


gradient : Time -> String
gradient dt =
    let
        ( r, g, b ) =
            ( round (255 * dt), 0, round (255 * (1 - dt)) )

        col =
            Hex.toString (256 * (256 * r + g) + b)
    in
        if String.length col < 6 then
            "#" ++ String.repeat (6 - String.length col) "0" ++ col
        else
            "#" ++ col
