module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import List.Extra exposing (lift2)


type alias Model =
    { dots : List Point
    }


type alias Edge =
    ( Point, Point )


type Msg
    = Draw Int


type alias Point =
    ( Int, Int )


init : ( Model, Cmd msg )
init =
    ( Model [], Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Draw dimension ->
            ( { model
                | dots = generateDots dimension
              }
            , Cmd.none
            )



-- generateEdges : Int -> List Edge
-- generateEdges dimension =
--     List.foldl (\point ->
--         if (point.x + 1 <= dimension)
--         ))  [] <|
--         generateDots dimension


generateDots : Int -> List Point
generateDots dimension =
    List.Extra.lift2 (,) (List.range 1 <| dimension + 1) (List.range 1 <| dimension + 1)


(=>) : a -> b -> ( a, b )
(=>) =
    (,)


view : Model -> Html Msg
view model =
    div []
        [ button [ onClick <| Draw 3 ] [ text "3 X 3" ]
        , button [ onClick <| Draw 4 ] [ text "4 X 4" ]
        , hr [] []
        , div [] <|
            List.map
                (\dot ->
                    Html.span
                        [ style
                            [ "background-color" => "#3C8D2F"
                            , "width" => "5px"
                            , "height" => "5px"
                            , "position" => "absolute"
                            , "margin-left" => ((toString (Tuple.first dot * 35)) ++ "px")
                            , "margin-top" => ((toString (Tuple.second dot * 35)) ++ "px")
                            ]
                        ]
                        []
                )
                model.dots
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
