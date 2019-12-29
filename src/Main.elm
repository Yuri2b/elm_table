module Main exposing (main)

import Browser exposing (element)
import Json.Decode as JD
import Model exposing (Model, Msg, init, update)
import Template exposing (view)



-- MAIN


main : Program JD.Value Model Msg
main =
    element
        { init = init
        , update = update
        , subscriptions = \_ -> Sub.none
        , view = view
        }
