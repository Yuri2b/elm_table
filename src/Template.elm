module Template exposing (view)

import Html exposing (Html, a, button, div, input, li, p, span, table, tbody, td, text, th, thead, tr, ul)
import Html.Attributes exposing (class, href, id, placeholder)
import Html.Events exposing (onClick, onInput)
import Model
    exposing
        ( Customer
        , Data(..)
        , Model
        , Msg(..)
        , currentPageCustomers
        , customerAttrToString
        , explainAppError
        , filteredCustomersCount
        , pagesCount
        )



-- VIEW


view : Model -> Html Msg
view model =
    div [ class "main-wrapper" ]
        [ searchBox model
        , pagination model
        , div [ class "table-wrapper" ]
            [ table [ class "uk-table uk-table-stripped uk-table-hover uk-table-divider uk-table-responsive uk-table-small" ]
                [ thead [] <| tableHead model
                , tbody [] <| tableBody model
                ]
            , loadingState model
            ]
        , selectedCustomerCard model
        ]


tableHead : Model -> List (Html Msg)
tableHead model =
    let
        ( orderClmn, direction ) =
            model.orderBy

        newDirection =
            if direction == "asc" then
                "dsc"

            else
                "asc"
    in
    [ tr [] <|
        List.map
            (\clmn ->
                th [ onClick <| OrderChanged ( clmn, newDirection ) ]
                    [ text clmn
                    , if orderClmn == clmn then
                        span [ class ("arrow " ++ direction) ] []

                      else
                        text ""
                    ]
            )
            model.columns
    ]


tableBody : Model -> List (Html Msg)
tableBody model =
    let
        customers =
            currentPageCustomers model
    in
    List.map
        (\customer ->
            partial_customer model.columns customer
        )
        customers


pagination : Model -> Html Msg
pagination model =
    let
        firstPage =
            1

        currentPage =
            model.currentPage

        pCount =
            pagesCount model
    in
    div [ class "pagination-wrapper" ]
        [ ul [ class "uk-pagination uk-flex-center" ] <|
            partial_pageLinks firstPage currentPage pCount
        ]


loadingState : Model -> Html Msg
loadingState model =
    case model.customers of
        Loading ->
            div [ class "spinner" ] []

        Failure error ->
            div [ class "alert-wrapper uk-alert uk-alert-danger uk-margin-large" ]
                [ p [ class "uk-margin-remove" ]
                    [ text "Во время загрузки данных произошла ошибка:"
                    ]
                , span [ class "uk-text-small" ]
                    [ text <| explainAppError error
                    ]
                ]

        _ ->
            text ""


searchBox : Model -> Html Msg
searchBox model =
    div [ class "search-box uk-margin-left" ]
        [ input
            [ id "searchInput"
            , class "uk-input uk-form-width-medium uk-form-small"
            , Html.Attributes.type_ "text"
            , Html.Attributes.name "search"
            , placeholder "что искать.."
            , onInput ChangeSearchInput
            ]
            []
        , button
            [ id "searchButton"
            , class "uk-button uk-button-default uk-button-small"
            , onClick <| FilterBySearch
            ]
            [ text "Найти"
            ]
        , p
            []
            [ text <|
                "Всего записей: "
                    ++ (String.fromInt <| filteredCustomersCount model)
            ]
        ]


partial_pageLink : Int -> Int -> Html Msg
partial_pageLink pNumber pCurrent =
    if pNumber == pCurrent then
        li [ class "uk-active" ]
            [ span []
                [ text <| String.fromInt pNumber
                ]
            ]

    else
        li []
            [ a [ href "#", onClick <| PageChanged pNumber ]
                [ text <| String.fromInt pNumber
                ]
            ]


partial_pageLinks : Int -> Int -> Int -> List (Html Msg)
partial_pageLinks pNumber pCurrent pCount =
    let
        pl =
            partial_pageLink pNumber pCurrent
    in
    if pNumber == pCount then
        [ pl ]

    else
        pl :: partial_pageLinks (pNumber + 1) pCurrent pCount


partial_customer : List String -> Customer -> Html Msg
partial_customer columns customer =
    let
        getAttr =
            customerAttrToString customer

        dataCells =
            List.map (\clmn -> getAttr clmn) columns
    in
    tr [ onClick <| SelectCustomer customer ]
        (List.map
            (\d ->
                td [] [ text d ]
            )
            dataCells
        )


selectedCustomerCard : Model -> Html Msg
selectedCustomerCard model =
    case model.selectedCustomer of
        Just customer ->
            let
                getAttr =
                    customerAttrToString customer
            in
            div [ class "item-wrapper uk-flex uk-flex-center@m" ]
                [ div [ class "uk-card uk-card-default uk-card-body uk-width-1-2@m" ]
                    [ p [ class "uk-card-title" ]
                        [ text <|
                            String.join " "
                                [ "Пользователь"
                                , getAttr "name"
                                , getAttr "surname"
                                ]
                        ]
                    , span [] [ text "Описание" ]
                    , p [ class "uk-text-emphasis uk-margin-remove-top" ]
                        [ text <| getAttr "description"
                        ]
                    , p []
                        [ span [] [ text "Адрес проживания" ]
                        , span [ class "uk-text-bold uk-display-block" ]
                            [ text <| getAttr "address.streetAddress"
                            ]
                        ]
                    , p []
                        [ text "Город"
                        , span [ class "uk-text-bold uk-display-block" ]
                            [ text <| getAttr "address.city"
                            ]
                        ]
                    , p []
                        [ text "Провинция/Штат"
                        , span [ class "uk-text-bold uk-display-block" ]
                            [ text <| getAttr "address.state"
                            ]
                        ]
                    , p []
                        [ text "Индекс"
                        , span [ class "uk-text-bold uk-display-block" ]
                            [ text <| getAttr "address.zip"
                            ]
                        ]
                    ]
                ]

        Nothing ->
            text ""
