module Template exposing (view)

import Html exposing (..)
import Html.Attributes exposing (attribute, class, href, id, placeholder, value)
import Html.Events exposing (onClick, onInput)
import Model exposing (..)



-- VIEW


view : Model -> Html Msg
view model =
    div [ class "main-wrapper" ]
        [ searchBox model
        , pagination model
        , div [ class "table-wrapper" ]
            [ table [ class "uk-table uk-table-stripped uk-table-hover uk-table-divider uk-table-responsive uk-table-small" ]
                [ thead [] <|
                    tableHead model
                , tbody [] <|
                    tableBody model
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
                    [ text <| explainLoadError error
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
        dataCells =
            List.map
                (\clmn ->
                    getAttrValue clmn customer
                )
                columns
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
            div [ class "item-wrapper uk-flex uk-flex-center@m" ]
                [ div [ class "uk-card uk-card-default uk-card-body uk-width-1-2@m" ]
                    [ p [ class "uk-card-title" ]
                        [ text <|
                            String.join " " [ "Пользователь", getAttrValue "name" customer, getAttrValue "surname" customer ]
                        ]
                    , span [] [ text "Описание" ]
                    , p [ class "uk-text-emphasis uk-margin-remove-top" ]
                        [ text <| getAttrValue "description" customer ]
                    , p []
                        [ span [] [ text <| "Адрес проживания" ]
                        , span [ class "uk-text-bold uk-display-block" ]
                            [ text <| getAttrValue "address.streetAddress" customer ]
                        ]
                    , p []
                        [ text "Город"
                        , span [ class "uk-text-bold uk-display-block" ]
                            [ text <| getAttrValue "address.city" customer ]
                        ]
                    , p []
                        [ text "Провинция/Штат"
                        , span [ class "uk-text-bold uk-display-block" ]
                            [ text <| getAttrValue "address.state" customer ]
                        ]
                    , p []
                        [ text "Индекс"
                        , span [ class "uk-text-bold uk-display-block" ]
                            [ text <| getAttrValue "address.zip" customer ]
                        ]
                    ]
                ]

        Nothing ->
            text ""
