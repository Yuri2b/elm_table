module Flags exposing (Flags, defaultColumns, defaultSortOrder, flagsDecoder)

import Json.Decode as JD exposing (Decoder)
import Json.Decode.Pipeline exposing (optional)
import Url exposing (Url)


type alias Flags =
    { url : Maybe Url
    , columns : List String
    }


defaultUrl : Maybe Url
defaultUrl =
    Url.fromString "http://www.filltext.com/?rows=100&id=%7Bnumber%7C1000%7D&firstName=%7BfirstName%7D&lastName=%7BlastName%7D&email=%7Bemail%7D&phone=%7Bphone%7C(xxx)xxx-xx-xx%7D&adress=%7BaddressObject%7D&description=%7Blorem%7C32%7D"


defaultColumns : List String
defaultColumns =
    [ "id", "name", "surname", "email", "phone", "address" ]


defaultSortOrder : ( String, String )
defaultSortOrder =
    ( "id", "asc" )


urlDecoder : String -> Decoder (Maybe Url)
urlDecoder url =
    JD.succeed (Url.fromString url)


flagsDecoder : Decoder Flags
flagsDecoder =
    JD.succeed Flags
        |> optional "url" (JD.string |> JD.andThen urlDecoder) defaultUrl
        |> optional "columns" (JD.list JD.string) defaultColumns
