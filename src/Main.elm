port module Main exposing (main)

import Browser
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as D exposing (Decoder)
import Json.Encode as E
import Set exposing (Set)
import Url


-- PORTS


port saveFeedUrls : E.Value -> Cmd msg


port saveReadArticles : E.Value -> Cmd msg



-- MAIN


main : Program D.Value Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }



-- MODEL


type alias Model =
    { feedUrls : List String
    , articles : List Article
    , feedTitles : Dict String String
    , inputUrl : String
    , loading : Set String
    , errors : Dict String String
    , readArticles : Set String
    , activeFilter : Maybe String
    , sidebarOpen : Bool
    }


type alias Article =
    { title : String
    , link : String
    , description : String
    , pubDate : String
    , feedTitle : String
    , author : String
    , feedUrl : String
    }


init : D.Value -> ( Model, Cmd Msg )
init flags =
    let
        feedUrls =
            flags
                |> D.decodeValue (D.field "feedUrls" (D.list D.string))
                |> Result.withDefault []

        readArticles =
            flags
                |> D.decodeValue (D.field "readArticles" (D.list D.string))
                |> Result.withDefault []
                |> Set.fromList
    in
    ( { feedUrls = feedUrls
      , articles = []
      , feedTitles = Dict.empty
      , inputUrl = ""
      , loading = Set.fromList feedUrls
      , errors = Dict.empty
      , readArticles = readArticles
      , activeFilter = Nothing
      , sidebarOpen = True
      }
    , Cmd.batch (List.map fetchFeed feedUrls)
    )



-- UPDATE


type Msg
    = InputChanged String
    | AddFeed
    | RemoveFeed String
    | GotFeed String (Result Http.Error RssFeedResponse)
    | MarkRead String
    | MarkAllRead
    | SetFilter (Maybe String)
    | RefreshAll
    | RefreshFeed String
    | ToggleSidebar


type alias RssFeedResponse =
    { feedTitle : String
    , feedUrl : String
    , items : List RssItem
    }


type alias RssItem =
    { title : String
    , link : String
    , description : String
    , pubDate : String
    , author : String
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        InputChanged url ->
            ( { model | inputUrl = url }, Cmd.none )

        AddFeed ->
            let
                url =
                    String.trim model.inputUrl
            in
            if String.isEmpty url || List.member url model.feedUrls then
                ( { model | inputUrl = "" }, Cmd.none )

            else
                let
                    newUrls =
                        model.feedUrls ++ [ url ]
                in
                ( { model
                    | feedUrls = newUrls
                    , inputUrl = ""
                    , loading = Set.insert url model.loading
                  }
                , Cmd.batch
                    [ fetchFeed url
                    , saveFeedUrls (E.list E.string newUrls)
                    ]
                )

        RemoveFeed url ->
            let
                newUrls =
                    List.filter ((/=) url) model.feedUrls

                newFilter =
                    if model.activeFilter == Just url then
                        Nothing

                    else
                        model.activeFilter
            in
            ( { model
                | feedUrls = newUrls
                , articles = List.filter (\a -> a.feedUrl /= url) model.articles
                , feedTitles = Dict.remove url model.feedTitles
                , loading = Set.remove url model.loading
                , errors = Dict.remove url model.errors
                , activeFilter = newFilter
              }
            , saveFeedUrls (E.list E.string newUrls)
            )

        GotFeed url result ->
            case result of
                Ok response ->
                    let
                        newArticles =
                            List.map
                                (\item ->
                                    { title = item.title
                                    , link = item.link
                                    , description = item.description
                                    , pubDate = item.pubDate
                                    , feedTitle = response.feedTitle
                                    , author = item.author
                                    , feedUrl = url
                                    }
                                )
                                response.items

                        allArticles =
                            List.filter (\a -> a.feedUrl /= url) model.articles
                                ++ newArticles
                                |> sortArticles
                    in
                    ( { model
                        | articles = allArticles
                        , feedTitles = Dict.insert url response.feedTitle model.feedTitles
                        , loading = Set.remove url model.loading
                        , errors = Dict.remove url model.errors
                      }
                    , Cmd.none
                    )

                Err err ->
                    ( { model
                        | loading = Set.remove url model.loading
                        , errors = Dict.insert url (httpErrorToString err) model.errors
                      }
                    , Cmd.none
                    )

        MarkRead link ->
            let
                newRead =
                    Set.insert link model.readArticles
            in
            ( { model | readArticles = newRead }
            , saveReadArticles (E.list E.string (Set.toList newRead))
            )

        MarkAllRead ->
            let
                newRead =
                    filteredArticles model
                        |> List.map .link
                        |> Set.fromList
                        |> Set.union model.readArticles
            in
            ( { model | readArticles = newRead }
            , saveReadArticles (E.list E.string (Set.toList newRead))
            )

        SetFilter filter ->
            ( { model | activeFilter = filter }, Cmd.none )

        RefreshAll ->
            ( { model | loading = Set.fromList model.feedUrls }
            , Cmd.batch (List.map fetchFeed model.feedUrls)
            )

        RefreshFeed url ->
            ( { model | loading = Set.insert url model.loading }
            , fetchFeed url
            )

        ToggleSidebar ->
            ( { model | sidebarOpen = not model.sidebarOpen }, Cmd.none )



-- HELPERS


sortArticles : List Article -> List Article
sortArticles =
    List.sortWith (\a b -> compare b.pubDate a.pubDate)


filteredArticles : Model -> List Article
filteredArticles model =
    case model.activeFilter of
        Nothing ->
            model.articles

        Just url ->
            List.filter (\a -> a.feedUrl == url) model.articles


httpErrorToString : Http.Error -> String
httpErrorToString err =
    case err of
        Http.BadUrl u ->
            "Bad URL: " ++ u

        Http.Timeout ->
            "Request timed out"

        Http.NetworkError ->
            "Network error (check CORS or feed URL)"

        Http.BadStatus status ->
            "Server error: " ++ String.fromInt status

        Http.BadBody body ->
            "Parse error: " ++ String.left 120 body


stripHtml : String -> String
stripHtml html =
    String.split "<" html
        |> List.indexedMap
            (\i part ->
                if i == 0 then
                    part

                else
                    case String.split ">" part of
                        [] ->
                            ""

                        [ _ ] ->
                            ""

                        _ :: rest ->
                            String.join ">" rest
            )
        |> String.join ""
        |> String.replace "&amp;" "&"
        |> String.replace "&lt;" "<"
        |> String.replace "&gt;" ">"
        |> String.replace "&quot;" "\""
        |> String.replace "&#39;" "'"
        |> String.replace "&nbsp;" " "
        |> String.trim


truncateText : Int -> String -> String
truncateText maxLen text =
    if String.length text > maxLen then
        String.left maxLen text ++ "…"

    else
        text


shortenUrl : String -> String
shortenUrl url =
    url
        |> String.replace "https://" ""
        |> String.replace "http://" ""
        |> String.replace "www." ""
        |> String.split "/"
        |> List.head
        |> Maybe.withDefault url


formatDate : String -> String
formatDate date =
    -- rss2json returns "YYYY-MM-DD HH:MM:SS"
    let
        parts =
            String.split "-" (String.left 10 date)
    in
    case parts of
        [ year, month, day ] ->
            monthName month
                ++ " "
                ++ (day |> String.toInt |> Maybe.withDefault 0 |> String.fromInt)
                ++ ", "
                ++ year

        _ ->
            String.left 10 date


monthName : String -> String
monthName m =
    case m of
        "01" -> "Jan"
        "02" -> "Feb"
        "03" -> "Mar"
        "04" -> "Apr"
        "05" -> "May"
        "06" -> "Jun"
        "07" -> "Jul"
        "08" -> "Aug"
        "09" -> "Sep"
        "10" -> "Oct"
        "11" -> "Nov"
        "12" -> "Dec"
        _ -> m



-- HTTP


fetchFeed : String -> Cmd Msg
fetchFeed url =
    Http.get
        { url = "https://api.rss2json.com/v1/api.json?rss_url=" ++ Url.percentEncode url
        , expect = Http.expectJson (GotFeed url) feedDecoder
        }


feedDecoder : Decoder RssFeedResponse
feedDecoder =
    D.map3 RssFeedResponse
        (D.at [ "feed", "title" ] D.string)
        (D.at [ "feed", "url" ] D.string)
        (D.field "items" (D.list itemDecoder))


itemDecoder : Decoder RssItem
itemDecoder =
    D.map5 RssItem
        (D.field "title" D.string)
        (D.field "link" D.string)
        (D.oneOf [ D.field "description" D.string, D.succeed "" ])
        (D.oneOf [ D.field "pubDate" D.string, D.succeed "" ])
        (D.oneOf [ D.field "author" D.string, D.succeed "" ])



-- VIEW


onEnter : Msg -> Attribute Msg
onEnter msg =
    on "keydown"
        (D.field "key" D.string
            |> D.andThen
                (\key ->
                    if key == "Enter" then
                        D.succeed msg

                    else
                        D.fail "not enter"
                )
        )


view : Model -> Html Msg
view model =
    div [ classList [ ( "app", True ), ( "sidebar-collapsed", not model.sidebarOpen ) ] ]
        [ viewSidebar model
        , viewMainContent model
        ]


viewSidebar : Model -> Html Msg
viewSidebar model =
    aside [ class "sidebar" ]
        [ div [ class "sidebar-header" ]
            [ h1 [ class "logo" ] [ text "RSS View" ]
            , button
                [ class "btn-icon"
                , onClick RefreshAll
                , disabled (not (Set.isEmpty model.loading))
                , title "Refresh all feeds"
                ]
                [ text "↻" ]
            , button
                [ class "btn-icon"
                , onClick ToggleSidebar
                , title "Collapse sidebar"
                ]
                [ text "‹" ]
            ]
        , div [ class "add-feed" ]
            [ input
                [ type_ "url"
                , placeholder "RSS feed URL…"
                , value model.inputUrl
                , onInput InputChanged
                , onEnter AddFeed
                , class "feed-input"
                ]
                []
            , button
                [ class "btn-add"
                , onClick AddFeed
                , disabled (String.isEmpty (String.trim model.inputUrl))
                ]
                [ text "Add" ]
            ]
        , nav [ class "feed-nav" ]
            (viewAllFeedsItem model :: List.map (viewFeedNavItem model) model.feedUrls)
        ]


viewAllFeedsItem : Model -> Html Msg
viewAllFeedsItem model =
    let
        unreadCount =
            List.filter (\a -> not (Set.member a.link model.readArticles)) model.articles
                |> List.length

        isActive =
            model.activeFilter == Nothing
    in
    button
        [ classList [ ( "feed-nav-item", True ), ( "active", isActive ) ]
        , onClick (SetFilter Nothing)
        ]
        [ span [ class "feed-nav-icon" ] [ text "●" ]
        , span [ class "feed-nav-title" ] [ text "All Feeds" ]
        , if unreadCount > 0 then
            span [ class "badge" ] [ text (String.fromInt unreadCount) ]

          else
            text ""
        ]


viewFeedNavItem : Model -> String -> Html Msg
viewFeedNavItem model url =
    let
        feedTitle =
            Dict.get url model.feedTitles
                |> Maybe.withDefault (shortenUrl url)

        unreadCount =
            List.filter (\a -> a.feedUrl == url && not (Set.member a.link model.readArticles)) model.articles
                |> List.length

        isLoading =
            Set.member url model.loading

        hasError =
            Dict.member url model.errors

        isActive =
            model.activeFilter == Just url
    in
    div [ classList [ ( "feed-nav-item-wrapper", True ), ( "is-active", isActive ) ] ]
        [ button
            [ classList
                [ ( "feed-nav-item", True )
                , ( "active", isActive )
                , ( "has-error", hasError )
                ]
            , onClick (SetFilter (Just url))
            ]
            [ span [ class "feed-nav-icon" ]
                [ if isLoading then
                    span [ class "spinner" ] [ text "⟳" ]

                  else if hasError then
                    span [ class "error-icon" ] [ text "!" ]

                  else
                    text "○"
                ]
            , span [ class "feed-nav-title", title url ] [ text feedTitle ]
            , if unreadCount > 0 then
                span [ class "badge" ] [ text (String.fromInt unreadCount) ]

              else
                text ""
            ]
        , button
            [ class "btn-remove"
            , onClick (RemoveFeed url)
            , title ("Remove \"" ++ feedTitle ++ "\"")
            ]
            [ text "×" ]
        ]


viewMainContent : Model -> Html Msg
viewMainContent model =
    main_
        [ class "main" ]
        [ viewMainHeader model
        , viewErrors model
        , viewArticleList model
        ]


viewMainHeader : Model -> Html Msg
viewMainHeader model =
    let
        articles =
            filteredArticles model

        unreadCount =
            List.filter (\a -> not (Set.member a.link model.readArticles)) articles
                |> List.length

        pageTitle =
            case model.activeFilter of
                Nothing ->
                    "All Articles"

                Just url ->
                    Dict.get url model.feedTitles
                        |> Maybe.withDefault (shortenUrl url)
    in
    div [ class "main-header" ]
        [ if not model.sidebarOpen then
            button
                [ class "btn-icon"
                , onClick ToggleSidebar
                , title "Expand sidebar"
                ]
                [ text "›" ]

          else
            text ""
        , h2 [ class "main-title" ] [ text pageTitle ]
        , div [ class "main-actions" ]
            [ if unreadCount > 0 then
                button
                    [ class "btn-ghost"
                    , onClick MarkAllRead
                    ]
                    [ text ("Mark " ++ String.fromInt unreadCount ++ " read") ]

              else
                text ""
            ]
        ]


viewErrors : Model -> Html Msg
viewErrors model =
    if Dict.isEmpty model.errors then
        text ""

    else
        div [ class "error-banner" ]
            (model.errors
                |> Dict.toList
                |> List.map
                    (\( url, errMsg ) ->
                        div [ class "error-item" ]
                            [ span [ class "error-label" ] [ text (shortenUrl url) ]
                            , span [] [ text (": " ++ errMsg) ]
                            , button
                                [ class "btn-retry"
                                , onClick (RefreshFeed url)
                                ]
                                [ text "Retry" ]
                            ]
                    )
            )


viewArticleList : Model -> Html Msg
viewArticleList model =
    let
        articles =
            filteredArticles model
    in
    if List.isEmpty articles && Set.isEmpty model.loading then
        viewEmptyState model

    else if List.isEmpty articles then
        div [ class "loading-state" ]
            [ div [ class "loading-spinner" ] [ text "⟳" ]
            , p [] [ text "Loading articles…" ]
            ]

    else
        div [ class "article-list" ]
            (List.map (viewArticle model) articles)


viewEmptyState : Model -> Html Msg
viewEmptyState model =
    if List.isEmpty model.feedUrls then
        div [ class "empty-state" ]
            [ div [ class "empty-icon" ] [ text "📰" ]
            , h3 [] [ text "Add your first RSS feed" ]
            , p [] [ text "Paste an RSS feed URL in the sidebar to start reading." ]
            , div [ class "suggestions" ]
                [ p [ class "suggestions-title" ] [ text "Try these feeds:" ]
                , ul []
                    [ li [] [ text "https://blog.latch.bio/feed" ]
                    , li [] [ text "https://news.ycombinator.com/rss" ]
                    , li [] [ text "https://feeds.bbci.co.uk/news/rss.xml" ]
                    ]
                ]
            ]

    else
        div [ class "empty-state" ]
            [ div [ class "empty-icon" ] [ text "✓" ]
            , h3 [] [ text "All caught up!" ]
            , p [] [ text "No articles to show." ]
            ]


viewArticle : Model -> Article -> Html Msg
viewArticle model art =
    let
        isRead =
            Set.member art.link model.readArticles

        description =
            art.description
                |> stripHtml
                |> truncateText 280
    in
    article
        [ classList [ ( "article-card", True ), ( "is-read", isRead ) ] ]
        [ div [ class "article-meta" ]
            [ span [ class "article-feed-name" ] [ text art.feedTitle ]
            , if art.pubDate /= "" then
                span [ class "article-date" ] [ text (formatDate art.pubDate) ]

              else
                text ""
            , if art.author /= "" then
                span [ class "article-author" ] [ text art.author ]

              else
                text ""
            ]
        , a
            [ href art.link
            , target "_blank"
            , rel "noopener noreferrer"
            , class "article-title"
            , onClick (MarkRead art.link)
            ]
            [ text art.title ]
        , if description /= "" then
            p [ class "article-description" ] [ text description ]

          else
            text ""
        , if not isRead then
            button
                [ class "btn-mark-read"
                , onClick (MarkRead art.link)
                ]
                [ text "Mark as read" ]

          else
            text ""
        ]
