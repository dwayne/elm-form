module Example exposing (suite)

import Expect
import Test exposing (Test, test)


suite : Test
suite =
    test "example" <|
        \_ ->
            1 |> Expect.equal 1
