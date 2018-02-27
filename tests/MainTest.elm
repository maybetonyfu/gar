module MainTest exposing (..)

import Expect exposing (Expectation)
import Test exposing (..)
import Main exposing (..)


generateDots : Test
generateDots =
    describe "Generate vertice from given dimension"
        [ test "Generate 3 X 3 dots" <|
            \() ->
                Expect.equal (List.length <| Main.generateDots 3) 16
        , test "Generate 4 X 4 dots" <|
            \() ->
                Expect.equal (List.length <| Main.generateDots 4) 25
        ]


generateEdges : Test
generateEdges =
    describe "Generate edges from given dimension"
        [ test "Generate 3 X 3 edges" <|
            \() ->
                Expect.equal (List.length <| Main.generateEdges 3) 24
        , test "Generate 4 X 4 edges" <|
            \() ->
                Expect.equal (List.length <| Main.generateEdges 4) 40
        ]
