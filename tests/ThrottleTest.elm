module ThrottleTest exposing (..)

import Expect exposing (Expectation)
import Test exposing (..)
import Utils.Throttle as Throttle


getRateTest =
    test "getRate"
        (\_ -> Expect.equal 30 (Throttle.getRate (Throttle.init 30)))


calculateIntervalRateTest =
    describe "calculateIntervalRateTest" <|
        List.map
            (\{ given, expected } ->
                test (String.fromInt given) <|
                    \_ ->
                        Expect.equal expected (Throttle.calculateIntervalRate given)
            )
            [ { given = 10, expected = ( 12000, 2 ) }
            , { given = 53, expected = ( 60000, 53 ) }
            , { given = 20, expected = ( 12000, 4 ) }
            , { given = 30, expected = ( 12000, 6 ) }
            , { given = 80, expected = ( 12000, 16 ) }
            , { given = 90, expected = ( 12000, 18 ) }
            , { given = 100, expected = ( 12000, 20 ) }
            ]
