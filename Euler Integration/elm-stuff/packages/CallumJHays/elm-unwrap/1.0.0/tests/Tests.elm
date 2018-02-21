module Tests exposing (..)

import Test exposing (..)
import Expect exposing (Expectation)
import Fuzz exposing (list, int, float, string)
import Unwrap


all : Test
all =
    describe "Unwrap Test Suite"
        [ describe "Unwrap.maybe"
            [ describe "Success"
                [ fuzz float "Float" testUnwrapMaybeSuccess
                , fuzz string "String" testUnwrapMaybeSuccess
                , fuzz (list int) "List Int" testUnwrapMaybeSuccess
                ]
            ]
        , describe "Unwrap.result"
            [ describe "Success"
                [ fuzz float "Float" testUnwrapResultSuccess
                , fuzz string "String" testUnwrapResultSuccess
                , fuzz (list int) "List Int" testUnwrapResultSuccess
                ]
            ]
        ]


testUnwrapMaybeSuccess : val -> Expectation
testUnwrapMaybeSuccess val =
    Expect.equal (Unwrap.maybe (Just val)) val


testUnwrapResultSuccess : val -> Expectation
testUnwrapResultSuccess val =
    Expect.equal (Unwrap.result (Ok val)) val



-- Note: There is no way to test for crashing in Elm-Test :(
