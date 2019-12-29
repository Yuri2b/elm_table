module Lib.TypedRecord exposing
    ( TypedRecord
    , attrIntValDecoder
    , attrRecordDecoder
    , attrStringValDecoder
    , attrToString
    , filteredBy
    , getAttrByKey
    , sortedBy
    )

import Json.Decode as JD
import Tuple exposing (first, second)


type alias TR =
    TypedRecord


type alias TypedRecord =
    List Attr


type alias Attr =
    ( String, AttrValue )


type AttrValue
    = String String
    | Int Int
    | Record (List Attr)


getAttrByKey : String -> TypedRecord -> Maybe Attr
getAttrByKey searchKey item =
    -- imitation of searching for attributes likewise in Record
    let
        checkAttrKey =
            \k attr ->
                first attr == k
    in
    case String.split "." searchKey of
        [ key ] ->
            List.filter (checkAttrKey key) item |> List.head

        key :: rest ->
            case List.filter (checkAttrKey key) item |> List.head of
                Just attr ->
                    case attr of
                        ( _, Record subAttr ) ->
                            getAttrByKey (String.join "." rest) subAttr

                        ( _, _ ) ->
                            Nothing

                Nothing ->
                    Nothing

        [] ->
            Nothing


attrToString : Maybe Attr -> String
attrToString is_attr =
    case is_attr of
        Nothing ->
            ""

        Just attr ->
            case second attr of
                String value ->
                    value

                Int value ->
                    String.fromInt value

                Record attrs ->
                    List.map (\a -> Just a) attrs
                        |> List.map (\a -> attrToString a)
                        |> String.join " "


sortedBy : ( String, String ) -> List TR -> List TR
sortedBy ( byKey, withOrder ) items =
    let
        sortFunc : String -> (TR -> Maybe Attr) -> TR -> TR -> Order
        sortFunc =
            \order func a b ->
                case compare (func a |> attrToString) (func b |> attrToString) of
                    LT ->
                        if order == "asc" then
                            LT

                        else
                            GT

                    EQ ->
                        EQ

                    GT ->
                        if order == "dsc" then
                            LT

                        else
                            GT

        sortByOrder =
            sortFunc withOrder
    in
    List.sortWith
        (getAttrByKey byKey |> sortByOrder)
        items


filteredBy : List String -> String -> List TR -> List TR
filteredBy filteredKeys queryString items =
    -- filter only by certain attributes
    let
        containsQueryString =
            \attrValue ->
                String.contains (String.toLower queryString) (String.toLower attrValue)

        onlyDisplayedAttrs =
            \attr ->
                List.member (first attr) filteredKeys
    in
    List.filter
        (\item ->
            List.filter onlyDisplayedAttrs item
                |> List.map (\a -> Just a)
                |> List.map attrToString
                |> List.any containsQueryString
        )
        items


buildAttr : String -> AttrValue -> JD.Decoder Attr
buildAttr attrName attr =
    JD.succeed ( attrName, attr )


attrIntValDecoder : String -> JD.Decoder Attr
attrIntValDecoder attrName =
    JD.map Int JD.int |> JD.andThen (buildAttr attrName)


attrStringValDecoder : String -> JD.Decoder Attr
attrStringValDecoder attrName =
    JD.map String JD.string |> JD.andThen (buildAttr attrName)


attrRecordDecoder : String -> JD.Decoder TR -> JD.Decoder Attr
attrRecordDecoder attrName nestedRecordDecoder =
    JD.map Record (JD.lazy (\_ -> nestedRecordDecoder)) |> JD.andThen (buildAttr attrName)
