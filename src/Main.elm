module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import List.Extra exposing (lift2)
import List exposing (..)
import Tuple exposing (..)
import Set


type alias Model =
    { points : List Point
    }


type alias Line =
    Set.Set Point


type alias Square =
    Set.Set Point


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
                | points = generatePoints dimension
              }
            , Cmd.none
            )


generateLines : Int -> List Line
generateLines dimension =
    generatePoints dimension
        |> foldl
            (\point lines ->
                Set.fromList [ point, getBottomPoint point ] :: Set.fromList [ point, getRightPoint point ] :: lines
            )
            []
        |> filter (lineInRange (dimension))


generateSquares : Int -> List Square
generateSquares dimension =
    generatePoints dimension
        |> filter (pointInRange <| dimension - 1)
        |> foldl
            (\point squares ->
                (Set.fromList
                    [ point
                    , getRightPoint point
                    , getBottomPoint point
                    , getBottomRightPoint point
                    ]
                )
                    :: squares
            )
            []


pointInRange : Int -> Point -> Bool
pointInRange dimension point =
    first point <= dimension + 1 && second point <= dimension + 1


lineInRange : Int -> Line -> Bool
lineInRange maxiumLineCoordinate line =
    Set.toList line
        |> List.all (pointInRange maxiumLineCoordinate)


getRightPoint : Point -> Point
getRightPoint point =
    (first point + 1) => second point


getBottomPoint : Point -> Point
getBottomPoint point =
    first point => (second point + 1)


getBottomRightPoint : Point -> Point
getBottomRightPoint point =
    (first point + 1) => (second point + 1)


generatePoints : Int -> List Point
generatePoints dimension =
    lift2 (,) (range 1 <| dimension + 1) (range 1 <| dimension + 1)


view : Model -> Html Msg
view model =
    div []
        [ button [ onClick <| Draw 3 ] [ text "3 X 3" ]
        , button [ onClick <| Draw 4 ] [ text "4 X 4" ]
        , hr [] []
        , div [] <|
            List.map
                (\point ->
                    span
                        [ style
                            [ "background-color" => "#3C8D2F"
                            , "width" => "5px"
                            , "height" => "5px"
                            , "position" => "absolute"
                            , "margin-left" => (toString (first point * 35) ++ "px")
                            , "margin-top" => (toString (second point * 35) ++ "px")
                            ]
                        ]
                        []
                )
                model.points
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
