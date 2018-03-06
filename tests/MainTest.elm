module MainTest exposing (..)

import Expect exposing (Expectation)
import Test exposing (..)
import Main exposing (..)


generatePoints : Test
generatePoints =
    describe "Generate points from given dimension"
        [ test "Generate 3 X 3 points" <|
            \() ->
                Expect.equal (List.length <| Main.generatePoints 3) 16
        , test "Generate 4 X 4 points" <|
            \() ->
                Expect.equal (List.length <| Main.generatePoints 4) 25
        ]


generateLines : Test
generateLines =
    describe "Generate lines from given dimension"
        [ test "Generate 3 X 3 lines" <|
            \() ->
                Expect.equal (List.length <| Main.generateLines 3) 24
        , test "Generate 4 X 4 lines" <|
            \() ->
                Expect.equal (List.length <| Main.generateLines 4) 40
        , test "Every line has exactly 2 points" <|
            \() ->
                Main.generateLines 3
                    |> List.all (\line -> (List.length line) == 2)
                    |> Expect.true "Expect size == 2 to be true"
        , test "Check whether line is horizontal" <|
            \() ->
                Main.isLineHorizontal [ ( 0, 0 ), ( 0, 1 ) ] |> Expect.true "Line is indeed horizontal"
        , test "Check whether line is not horizontal" <|
            \() ->
                Main.isLineHorizontal [ ( 0, 0 ), ( 1, 1 ) ] |> Expect.false "Line is not horizontal"
        , test "Check whether line is vertical" <|
            \() ->
                Main.isLineVertical [ ( 0, 0 ), ( 1, 0 ) ] |> Expect.true "Line is indeed vertical"
        , test "Check whether line is not vertical" <|
            \() ->
                Main.isLineVertical [ ( 0, 0 ), ( 1, 1 ) ] |> Expect.false "Line is not vertical"
        ]


generateSquares : Test
generateSquares =
    describe "Generate squares from given dimension"
        [ test "Generate 3 X 3 squares" <|
            \() ->
                Expect.equal (List.length <| Main.generateSquares 3) 9
        , test "Generate 4 X 4 squares" <|
            \() ->
                Expect.equal (List.length <| Main.generateSquares 4) 16
        , test "Every square has exactly 4 points" <|
            \() ->
                Main.generateSquares 3
                    |> List.all (\square -> (List.length square) == 4)
                    |> Expect.true "Expect size == 4 to be true"
        ]
