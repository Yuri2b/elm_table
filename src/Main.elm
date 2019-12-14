module Main exposing (main)

import Browser exposing (element)
import Model exposing (init, subscriptions, update)
import Template exposing (view)



-- MAIN


main =
    element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }
