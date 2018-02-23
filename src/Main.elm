module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)


type alias Model =
    { dots : List Point
    }


type alias Edge =
    ( Point, Point )


type Msg
    = Draw Int


type alias Point =
    { x : Int
    , y : Int
    }


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
--     List.fold (\point ->
--         if ()
--         ))  [] <|
--         generateDots dimension


generateDots : Int -> List Point
generateDots dimension =
    List.map (generateDot dimension) <|
        List.range 0 <|
            (dimension + 1)
                ^ 2
                - 1


generateDot : Int -> Int -> Point
generateDot dimension index =
    { x = index // (dimension + 1), y = index % (dimension + 1) }


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
                    span
                        [ style
                            [ "background-color" => "#3C8D2F"
                            , "width" => "5px"
                            , "height" => "5px"
                            , "position" => "absolute"
                            , "margin-left" => ((toString (dot.x * 35)) ++ "px")
                            , "margin-top" => ((toString (dot.y * 35)) ++ "px")
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
