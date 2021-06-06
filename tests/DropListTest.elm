module DropListTest exposing (..)

import DropList
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)


suite : Test
suite =
    describe "DropList"
        [ fuzz (list int) "builds from list and maps to the same list" <|
            \someList ->
                someList
                |> DropList.fromList
                |> DropList.toValueList
                |> Expect.equalLists someList
        ]

