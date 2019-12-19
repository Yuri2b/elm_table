module Model exposing
    ( Customer
    , Data(..)
    , Model
    , Msg(..)
    , currentPageCustomers
    , explainLoadError
    , filteredCustomersCount
    , getAttrValue
    , init
    , pagesCount
    , subscriptions
    , update
    )

import Array
import Http
import Json.Decode as JD
import List
import String
import Tuple exposing (first, second)


type AttrValue
    = String String
    | Int Int
    | Record (List Attr)


type alias Attr =
    ( String, AttrValue )


type alias Customer =
    List Attr


type Data
    = Success (List Customer)
    | Failure Http.Error
    | Loading


type alias Model =
    { customers : Data
    , currentPage : Int
    , perPage : Int
    , columns : List String
    , orderBy : ( String, String )
    , query : String
    , searchInput : String
    , selectedCustomer : Maybe Customer
    }


getAttrValue : String -> Customer -> String
getAttrValue key customer =
    let
        is_attr =
            getAttrByKey key customer
    in
    case is_attr of
        Just attr ->
            stringFromAttr attr

        Nothing ->
            ""


getAttrByKey : String -> Customer -> Maybe Attr
getAttrByKey searchKey customer =
    -- imitation of searching for attributes like in Record
    let
        checkAttrKey =
            \k attr ->
                first attr == k
    in
    case String.split "." searchKey of
        [ key ] ->
            List.head <| List.filter (checkAttrKey key) customer

        key :: rest ->
            case List.head <| List.filter (checkAttrKey key) customer of
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


stringFromAttr : Attr -> String
stringFromAttr attr =
    case second attr of
        String value ->
            value

        Int value ->
            String.fromInt value

        Record attrs ->
            String.join " " <| List.map (\a -> stringFromAttr a) attrs


pagedItems : Int -> Int -> List a -> List a
pagedItems currentPage perPage customers =
    let
        from =
            (currentPage - 1) * perPage

        to =
            currentPage * perPage

        customersArray =
            Array.fromList customers
    in
    Array.toList <|
        Array.slice from to customersArray


sortedItems : ( String, String ) -> List Customer -> List Customer
sortedItems ( byColumn, withOrder ) customers =
    let
        sortFunc : String -> (Customer -> comparable) -> Customer -> Customer -> Order
        sortFunc =
            \order func a b ->
                case compare (func a) (func b) of
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
        (sortByOrder <| getAttrValue byColumn)
        customers


filteredItems : List String -> String -> List Customer -> List Customer
filteredItems displayedColumns queryString customers =
    -- filter from only displayed attributes
    let
        containsQueryString =
            \attrValue ->
                String.contains (String.toLower queryString) (String.toLower attrValue)

        onlyDisplayedAttrs =
            \attr ->
                List.member (first attr) displayedColumns
    in
    List.filter
        (\customer ->
            List.any containsQueryString <| List.map stringFromAttr <| List.filter onlyDisplayedAttrs customer
        )
        customers


filteredCustomers : Model -> List Customer
filteredCustomers model =
    let
        queryString =
            model.query

        displayedColumns =
            model.columns
    in
    case model.customers of
        Success customers ->
            filteredItems displayedColumns queryString customers
                |> sortedItems model.orderBy

        _ ->
            []


currentPageCustomers : Model -> List Customer
currentPageCustomers model =
    let
        currentPage =
            model.currentPage

        perPage =
            model.perPage
    in
    case model.customers of
        Success customers ->
            pagedItems currentPage perPage <|
                filteredCustomers model

        _ ->
            []


filteredCustomersCount : Model -> Int
filteredCustomersCount model =
    List.length <| filteredCustomers model


pagesCount : Model -> Int
pagesCount model =
    let
        customersCount =
            filteredCustomersCount model

        perPage =
            model.perPage
    in
    if customersCount == 0 then
        1

    else if modBy perPage customersCount == 0 then
        customersCount // perPage

    else
        (customersCount // perPage) + 1


explainLoadError : Http.Error -> String
explainLoadError error =
    case error of
        Http.BadUrl message ->
            String.concat [ "BadUrl Error: ", message ]

        Http.BadBody message ->
            String.concat [ "Response body parsing problem: ", message ]

        Http.BadStatus code ->
            String.concat [ "Error status code: ", String.fromInt code ]

        _ ->
            "Some network problems..."


init : (List String) -> ( Model, Cmd Msg )
init columnsFromFlags =
    ( { customers = Loading
      , currentPage = 1
      , perPage = 25
      , columns = columnsFromFlags
      , orderBy = ( "id", "asc" )
      , query = ""
      , searchInput = ""
      , selectedCustomer = Nothing
      }
    , getData
    )



-- UPDATE


type Msg
    = LoadData
    | Loaded (Result Http.Error (List Customer))
    | PageChanged Int
    | OrderChanged ( String, String )
    | FilterBySearch
    | ChangeSearchInput String
    | SelectCustomer Customer


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LoadData ->
            ( { model | customers = Loading }, getData )

        Loaded result ->
            case result of
                Ok newCustomers ->
                    ( { model | customers = Success newCustomers }, Cmd.none )

                Err error ->
                    ( { model | customers = Failure error }, Cmd.none )

        PageChanged newPage ->
            ( { model | currentPage = newPage }, Cmd.none )

        OrderChanged ( by, direction ) ->
            ( { model | orderBy = ( by, direction ) }, Cmd.none )

        FilterBySearch ->
            ( { model | query = model.searchInput, currentPage = 1, selectedCustomer = Nothing }, Cmd.none )

        ChangeSearchInput inputValue ->
            ( { model | searchInput = inputValue }, Cmd.none )

        SelectCustomer customer ->
            ( { model | selectedCustomer = Just customer }, Cmd.none )


getData : Cmd Msg
getData =
    Http.get
        { url = "http://www.filltext.com/?rows=100&id=%7Bnumber%7C1000%7D&firstName=%7BfirstName%7D&lastName=%7BlastName%7D&email=%7Bemail%7D&phone=%7Bphone%7C(xxx)xxx-xx-xx%7D&adress=%7BaddressObject%7D&description=%7Blorem%7C32%7D"

        -- { url = "http://localhost:4000/data.json"
        , expect = Http.expectJson Loaded (JD.list customerDecoder)
        }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- JSON Decoder


buildAttr : String -> AttrValue -> JD.Decoder Attr
buildAttr attrName attr =
    JD.succeed ( attrName, attr )


attrValueDecoder : String -> JD.Decoder Attr
attrValueDecoder attrName =
    -- Int attributes
    if List.member attrName [ "id" ] then
        JD.map Int JD.int |> JD.andThen (buildAttr attrName)
        -- Record attributes

    else if List.member attrName [ "address" ] then
        JD.map Record (JD.lazy (\_ -> customerAddressDecoder)) |> JD.andThen (buildAttr attrName)
        -- String attributes

    else
        JD.map String JD.string |> JD.andThen (buildAttr attrName)


customerDecoder : JD.Decoder Customer
customerDecoder =
    -- Mapping and renaming fields
    JD.map7 (\a b c d e f g -> [ a, b, c, d, e, f, g ])
        (JD.field "id" <| attrValueDecoder "id")
        (JD.field "firstName" <| attrValueDecoder "name")
        (JD.field "lastName" <| attrValueDecoder "surname")
        (JD.field "email" <| attrValueDecoder "email")
        (JD.field "phone" <| attrValueDecoder "phone")
        (JD.field "adress" <| attrValueDecoder "address")
        (JD.field "description" <| attrValueDecoder "description")


customerAddressDecoder : JD.Decoder (List Attr)
customerAddressDecoder =
    -- Mapping fields in prefered order
    JD.map4 (\a b c d -> [ a, b, c, d ])
        (JD.field "city" <| attrValueDecoder "city")
        (JD.field "state" <| attrValueDecoder "state")
        (JD.field "streetAddress" <| attrValueDecoder "streetAddress")
        (JD.field "zip" <| attrValueDecoder "zip")
