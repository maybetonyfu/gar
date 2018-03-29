module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import List.Extra exposing (lift2)
import List exposing (..)
import Tuple exposing (..)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Set exposing (..)


type alias Model =
    { points : List Point
    , lines : List Line
    , squares : List Square
    }


type alias Line =
    List Point


type alias Square =
    List Point


type Msg
    = Draw Int


type alias Point =
    ( Int, Int )


init : ( Model, Cmd msg )
init =
    ( Model [] [] [], Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Draw dimension ->
            ( { model
                | points = generatePoints dimension
                , lines = generateLines dimension
                , squares = generateSquares dimension
              }
            , Cmd.none
            )


generateLines : Int -> List Line
generateLines dimension =
    let
        maxLineCoordinate : Int
        maxLineCoordinate =
            dimension
    in
        generatePoints dimension
            |> List.foldl
                (\point lines ->
                    [ point, getBottomPoint point ]
                        :: [ point, getRightPoint point ]
                        :: lines
                )
                []
            |> List.filter (lineInRange maxLineCoordinate)


generateSquares : Int -> List Square
generateSquares dimension =
    let
        maxTopLeftCornerCoordinate : Int
        maxTopLeftCornerCoordinate =
            dimension - 1
    in
        generatePoints dimension
            |> List.filter (pointInRange maxTopLeftCornerCoordinate)
            |> List.foldl
                (\point squares ->
                    [ point
                    , getRightPoint point
                    , getBottomPoint point
                    , getBottomRightPoint point
                    ]
                        :: squares
                )
                []


pointInRange : Int -> Point -> Bool
pointInRange maxPoinCoordinate point =
    Tuple.first point <= maxPoinCoordinate && Tuple.second point <= maxPoinCoordinate


lineInRange : Int -> Line -> Bool
lineInRange maxiumLineCoordinate line =
    List.all (pointInRange maxiumLineCoordinate) line


getRightPoint : Point -> Point
getRightPoint point =
    ( (Tuple.first point + 1), Tuple.second point )


getBottomPoint : Point -> Point
getBottomPoint point =
    ( Tuple.first point, (Tuple.second point + 1) )


getBottomRightPoint : Point -> Point
getBottomRightPoint point =
    ( (Tuple.first point + 1), (Tuple.second point + 1) )


generatePoints : Int -> List Point
generatePoints dimension =
    let
        maxPointCoordinate : Int
        maxPointCoordinate =
            dimension
    in
        lift2 (,) (range 0 maxPointCoordinate) (range 0 maxPointCoordinate)


view : Model -> Html Msg
view model =
    div []
        [ button [ onClick (Draw 3) ]
            [ Html.text "3 X 3" ]
        , button
            [ onClick (Draw 4) ]
            [ Html.text "4 X 4" ]
        , svg
            [ width "500"
            , height "500"
            , viewBox "0 0 120 120"
            , fill "white"
            , stroke "black"
            , strokeWidth "3"
            , Html.Attributes.style [ ( "padding-left", "20px" ) ]
            ]
            (List.concat
                [ List.map squareToSvg model.squares
                , List.map lineToSvg model.lines
                , List.map pointToSvg model.points
                ]
            )
        ]


pointToSvg : Point -> Html Msg
pointToSvg point =
    let
        toLeft : Int
        toLeft =
            10 + (first point) * 20

        toTop : Int
        toTop =
            10 + (second point) * 20
    in
        circle [ toString toLeft |> cx, toString toTop |> cy, r "3", stroke "none", fill "#663399" ] []


getLeftMostX : List Point -> Int
getLeftMostX list =
    List.map first list
        |> List.minimum
        |> Maybe.withDefault 0


getRightMostX : List Point -> Int
getRightMostX list =
    List.map first list
        |> List.maximum
        |> Maybe.withDefault 0


getTopMostY : List Point -> Int
getTopMostY list =
    List.map second list
        |> List.minimum
        |> Maybe.withDefault 0


getBottomMostY : List Point -> Int
getBottomMostY list =
    List.map second list
        |> List.maximum
        |> Maybe.withDefault 0


lineToSvg : Line -> Html Msg
lineToSvg line =
    let
        padLeft : Int
        padLeft =
            10

        lineLength : Int
        lineLength =
            20

        getScreenPosition : Int -> Int
        getScreenPosition coordinate =
            coordinate * lineLength + padLeft

        xLeft : Line -> Int
        xLeft line =
            getLeftMostX line |> getScreenPosition

        xRight : Line -> Int
        xRight line =
            getRightMostX line |> getScreenPosition

        yTop : Line -> Int
        yTop line =
            getTopMostY line |> getScreenPosition

        yBottom : Line -> Int
        yBottom line =
            getBottomMostY line |> getScreenPosition
    in
        Svg.line
            [ xLeft line |> toString |> x1
            , xRight line |> toString |> x2
            , yTop line |> toString |> y1
            , yBottom line |> toString |> y2
            ]
            []


squareToSvg : Square -> Html Msg
squareToSvg square =
    let
        padLeft : Int
        padLeft =
            10

        squareSize : Int
        squareSize =
            20
    in
        Svg.rect
            [ getLeftMostX square |> (*) squareSize |> (+) padLeft |> toString |> x
            , getTopMostY square |> (*) squareSize |> (+) padLeft |> toString |> y
            , toString squareSize |> width
            , toString squareSize |> height
            , stroke "none"
            , fill "#663399"
            ]
            []


isLineHorizontal : Line -> Bool
isLineHorizontal line =
    List.map first line
        |> Set.fromList
        |> Set.size
        |> (==) 1


isLineVertical : Line -> Bool
isLineVertical line =
    List.map second line
        |> Set.fromList
        |> Set.size
        |> (==) 1


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
