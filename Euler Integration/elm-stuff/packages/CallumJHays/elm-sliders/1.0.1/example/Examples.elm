module Examples exposing (main)

import Html as H exposing (..)
import Html.Attributes exposing (..)
import Tuple as T
import EveryDict
import Slider
import Unwrap
import Css as C exposing (..)


type alias Model =
    { sliders : EveryDict.EveryDict SliderOption Slider.Model }


type SliderOption
    = X
    | Y
    | Z


getVal : Model -> SliderOption -> Float
getVal model sliderOption =
    model.sliders
        |> EveryDict.get sliderOption
        |> Unwrap.maybe
        |> .val


type Msg
    = UpdateSliderOpt SliderOption Slider.Msg


init : ( Model, Cmd Msg )
init =
    { sliders =
        EveryDict.fromList
            [ ( X, T.first Slider.init )
            , ( Y, T.first Slider.init )
            , ( Z, T.first Slider.init )
            ]
    }
        ! []


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateSliderOpt sliderOption msg ->
            { model
                | sliders =
                    model.sliders
                        |> EveryDict.update
                            sliderOption
                            (Maybe.map ((Slider.update msg) >> T.first))
            }
                ! []


view : Model -> Html Msg
view model =
    let
        sliderViews =
            model.sliders
                |> EveryDict.toList
                |> List.map
                    (\( sliderOption, sliderModel ) ->
                        div []
                            [ label [] [ H.text ((sliderOption |> toString) ++ ": ") ]
                            , H.map (UpdateSliderOpt sliderOption) (Slider.view sliderModel)
                            , H.text (" = " ++ (sliderModel.val |> toString))
                            ]
                    )

        ( x, y, z ) =
            ( getVal model X
            , getVal model Y
            , getVal model Z
            )

        funcStr =
            "= "
                ++ (x |> toString)
                ++ " * "
                ++ (y |> toString)
                ++ " + "
                ++ (z |> toString)

        funcRes =
            x * y + z

        colAttrs =
            [ styles [ display inlineBlock, margin (px 10) ] ]
    in
        div [ styles [ fontFamily sansSerif ] ]
            [ div colAttrs ((h4 [] [ H.text "Sliders" ]) :: sliderViews)
            , div colAttrs
                [ h4 [] [ H.text "= X * Y + Z" ]
                , h4 [] [ H.text funcStr ]
                , h4 [] [ H.text ("= " ++ (funcRes |> toString)) ]
                ]
            ]


styles : List Mixin -> Attribute msg
styles =
    C.asPairs >> style


main : Program Never Model Msg
main =
    program
        { init = init
        , update = update
        , view = view
        , subscriptions = always Sub.none
        }
