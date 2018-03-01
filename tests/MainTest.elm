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
        ]
