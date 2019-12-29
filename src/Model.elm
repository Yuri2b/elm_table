module Model exposing
    ( Customer
    , Data(..)
    , Model
    , Msg(..)
    , currentPageCustomers
    , explainAppError
    , filteredCustomersCount
    , init
    , pagesCount
    , update
    )

import Array
import Flags exposing (defaultColumns, defaultSortOrder, flagsDecoder)
import Http
import Json.Decode as JD
import Json.Decode.Pipeline exposing (required)
import Lib.TypedRecord as TR exposing (TypedRecord)
import List
import String
import Url


type alias Customer =
    TypedRecord


type AppError
    = FetchingError Http.Error
    | FlagsParseError String


type Data
    = Success (List Customer)
    | Failure AppError
    | Loading


type alias Model =
    { customers : Data

    -- , dataUrl : String
    , currentPage : Int
    , perPage : Int
    , columns : List String
    , orderBy : ( String, String )
    , query : String
    , searchInput : String
    , selectedCustomer : Maybe Customer
    }


pagedItems : Int -> Int -> List a -> List a
pagedItems currentPage perPage items =
    let
        from =
            (currentPage - 1) * perPage

        to =
            currentPage * perPage

        itemsArray =
            Array.fromList items
    in
    Array.toList <|
        Array.slice from to itemsArray


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
            TR.filteredBy displayedColumns queryString customers
                |> TR.sortedBy model.orderBy

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
        Success _ ->
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


explainAppError : AppError -> String
explainAppError error =
    case error of
        FetchingError (Http.BadUrl message) ->
            String.concat [ "BadUrl Error: ", message ]

        FetchingError (Http.BadBody message) ->
            String.concat [ "Response body parsing problem: ", message ]

        FetchingError (Http.BadStatus code) ->
            String.concat [ "Error status code: ", String.fromInt code ]

        FlagsParseError message ->
            String.concat [ "Initial flags problem: ", message ]

        _ ->
            "Some network problems..."


init : JD.Value -> ( Model, Cmd Msg )
init inputFlags =
    let
        defaultModel =
            { customers = Success []
            , currentPage = 1
            , perPage = 25
            , columns = defaultColumns
            , orderBy = defaultSortOrder
            , query = ""
            , searchInput = ""
            , selectedCustomer = Nothing
            }
    in
    case JD.decodeValue flagsDecoder inputFlags of
        Err _ ->
            ( { defaultModel | customers = Failure (FlagsParseError "Elm flags parsing error") }, Cmd.none )

        Ok flags ->
            case flags.url of
                Just url ->
                    let
                        dataUrl =
                            Url.toString url
                    in
                    ( { defaultModel | customers = Loading, columns = flags.columns }, getData dataUrl )

                Nothing ->
                    ( { defaultModel | customers = Failure (FlagsParseError "Incorrect url in flags") }, Cmd.none )



-- UPDATE


type
    Msg
    -- = LoadData -- for loading on demand
    = Loaded (Result Http.Error (List Customer))
    | PageChanged Int
    | OrderChanged ( String, String )
    | FilterBySearch
    | ChangeSearchInput String
    | SelectCustomer Customer


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        -- LoadData ->
        --     ( { model | customers = Loading }, getData )
        Loaded result ->
            case result of
                Ok newCustomers ->
                    ( { model | customers = Success newCustomers }, Cmd.none )

                Err error ->
                    ( { model | customers = Failure (FetchingError error) }, Cmd.none )

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


getData : String -> Cmd Msg
getData dataUrl =
    Http.get
        { url = dataUrl
        , expect = Http.expectJson Loaded (JD.list customerDecoder)
        }



-- JSON Decoders for Customer


customerDecoder : JD.Decoder Customer
customerDecoder =
    JD.succeed (\a b c d e f g -> [ a, b, c, d, e, f, g ])
        |> required "id" (TR.attrIntValDecoder "id")
        |> required "firstName" (TR.attrStringValDecoder "name")
        |> required "lastName" (TR.attrStringValDecoder "surname")
        |> required "email" (TR.attrStringValDecoder "email")
        |> required "phone" (TR.attrStringValDecoder "phone")
        |> required "description" (TR.attrStringValDecoder "description")
        |> required "adress" (TR.attrRecordDecoder "address" customerAddressDecoder)


customerAddressDecoder : JD.Decoder TypedRecord
customerAddressDecoder =
    -- Mapping fields in prefered order
    JD.succeed (\a b c d -> [ a, b, c, d ])
        |> required "city" (TR.attrStringValDecoder "city")
        |> required "state" (TR.attrStringValDecoder "state")
        |> required "streetAddress" (TR.attrStringValDecoder "streetAddress")
        |> required "zip" (TR.attrStringValDecoder "zip")
