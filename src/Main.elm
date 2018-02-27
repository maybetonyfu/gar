module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import List.Extra exposing (lift2)
import List exposing (..)
import Tuple exposing (..)


type alias Model =
    { dots : List Point
    }


type alias Edge =
    ( Point, Point )


type Msg
    = Draw Int


type alias Point =
    ( Int, Int )


(=>) : a -> b -> ( a, b )
(=>) =
    (,)


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


generateEdges : Int -> List Edge
generateEdges dimension =
    generateDots dimension
        |> foldl
            (\dot edges ->
                dot => getBottomDot dot :: dot => getRightDot dot :: edges
            )
            []
        |> filter (\edge -> (isInRange dimension <| first edge) && (isInRange dimension <| second edge))


isInRange : Int -> Point -> Bool
isInRange dimension dot =
    first dot <= dimension + 1 && second dot <= dimension + 1


getRightDot : Point -> Point
getRightDot dot =
    (first dot + 1) => second dot


getBottomDot : Point -> Point
getBottomDot dot =
    first dot => (second dot + 1)


generateDots : Int -> List Point
generateDots dimension =
    lift2 (,) (range 1 <| dimension + 1) (range 1 <| dimension + 1)


view : Model -> Html Msg
view model =
    div []
        [ button [ onClick <| Draw 3 ] [ text "3 X 3" ]
        , button [ onClick <| Draw 4 ] [ text "4 X 4" ]
        , hr [] []
        , div [] <|
            List.map
                (\dot ->
                    span
                        [ style
                            [ "background-color" => "#3C8D2F"
                            , "width" => "5px"
                            , "height" => "5px"
                            , "position" => "absolute"
                            , "margin-left" => (toString (first dot * 35) ++ "px")
                            , "margin-top" => (toString (second dot * 35) ++ "px")
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
