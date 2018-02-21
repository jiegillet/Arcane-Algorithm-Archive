module Slider
    exposing
        ( Model
        , init
        , Msg(..)
        , update
        , view
        , Property(..)
        , props2view
        , onChange
        )

{-| Convenience wrapper around <input type="range"> HTML elements.

# Model
@docs Model, init

# Controller
@docs Msg, update

# View
@docs view, Property, props2view, onChange
-}

import Html exposing (..)
import Html.Attributes as HA exposing (..)
import Html.Events exposing (onInput, on)
import Json.Decode as Decode
import Unwrap


{-| Model that holds slider data
-}
type alias Model =
    { val : Float, minVal : Float, maxVal : Float, step : Float }


{-| Standard init in elm-style pattern
-}
init : ( Model, Cmd Msg )
init =
    default ! []


default : Model
default =
    { val = 50, minVal = 0, maxVal = 100, step = 1 }


{-| Messages related to slider updates
-}
type Msg
    = UpdateVal Float


{-| Standard elm-style update
-}
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateVal val ->
            { model | val = val } ! []


{-| Properties specific to this slider component.
Used to render the view correctly.

MinVal -> the inclusive lower bound of the input range element.

MaxVal -> the inclusive upper bound of the input range element.

Val -> the current value of the input range element.

Step -> the smallest possible increment by input in the input range element.

OnChange -> the inclusive lower bound of the input range element.

Attr -> Custom Html attributes to be applied to the input element.
-}
type Property msg
    = MinVal Float
    | MaxVal Float
    | Val Float
    | Step Float
    | OnChange (List (Attribute msg))
    | Attr (Attribute msg)


{-| Standard view function when using the standard component-driven API
-}
view : Model -> Html Msg
view model =
    model |> model2props |> props2view


{-| If you don't want to treat the slider as a component, and only care about
the 'current value', it may be simpler to use this to define your own slider.

For example:

    import Slider exposing (MinVal, MaxVal, Step, onChange, props2view)

    type Msg = NoOp | CustomSliderUpdate Float

    viewSlider : Html Msg
        [MinVal 10, MaxVal 20, Step = 0.5, onChange CustomSliderUpdate]
            |> props2view
-}
props2view : List (Property msg) -> Html msg
props2view props =
    input ([ type_ "range" ] ++ (props |> props2attrs)) []


{-| Handles on "input" and on "change" events conveniently
-}
onChange : (Float -> msg) -> Property msg
onChange msgType =
    OnChange
        [ onInput (String.toFloat >> Unwrap.result >> msgType)
        , on "change" (Decode.float |> Decode.map msgType)
        ]


{-| Converts a list of slider properties to a list of Html Attributes.
-}
props2attrs : List (Property msg) -> List (Attribute msg)
props2attrs props =
    props |> List.foldl prop2attrs []


{-| Converts a single slider property to update a list pf Html Attributes
-}
prop2attrs : Property msg -> List (Attribute msg) -> List (Attribute msg)
prop2attrs prop oldAttrs =
    let
        newAttrs =
            case prop of
                MinVal val ->
                    [ HA.min (val |> toString) ]

                MaxVal val ->
                    [ HA.max (val |> toString) ]

                Val val ->
                    [ HA.value (val |> toString) ]

                Step val ->
                    [ HA.step (val |> toString) ]

                OnChange attrs ->
                    attrs

                Attr attr ->
                    [ attr ]
    in
        oldAttrs ++ newAttrs


{-| Converts a model to a list of properties
-}
model2props : Model -> List (Property Msg)
model2props model =
    [ MinVal model.minVal
    , MaxVal model.maxVal
    , Val model.val
    , Step model.step
    , onChange UpdateVal
    ]
