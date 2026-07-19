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


port renderContent : E.Value -> Cmd msg


port printArticle : E.Value -> Cmd msg


port saveTopics : E.Value -> Cmd msg


port saveFavourites : E.Value -> Cmd msg


port saveArticleLabels : E.Value -> Cmd msg


port saveCustomFeedTitles : E.Value -> Cmd msg



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


type FeedFilter
    = AllFeeds
    | ByFeed String
    | ByLabel String


type alias Model =
    { feedUrls : List String
    , articles : List Article
    , feedTitles : Dict String String
    , inputUrl : String
    , loading : Set String
    , errors : Dict String String
    , readArticles : Set String
    , activeFilter : FeedFilter
    , sidebarMini : Bool
    , openTabs : List Article
    , activeTabLink : Maybe String
    , topics : Dict String (List String)
    , collapsedTopics : Set String
    , favourites : Set String
    , articleLabels : Dict String (List String)
    , newTopicInput : String
    , labelInput : String
    , customFeedTitles : Dict String String
    , editingFeedName : Maybe String
    , feedNameInput : String
    }


type alias Article =
    { title : String
    , link : String
    , description : String
    , content : String
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

        topics =
            flags
                |> D.decodeValue (D.field "topics" (D.dict (D.list D.string)))
                |> Result.withDefault Dict.empty

        favourites =
            flags
                |> D.decodeValue (D.field "favourites" (D.list D.string))
                |> Result.withDefault []
                |> Set.fromList

        articleLabels =
            flags
                |> D.decodeValue (D.field "articleLabels" (D.dict (D.list D.string)))
                |> Result.withDefault Dict.empty

        customFeedTitles =
            flags
                |> D.decodeValue (D.field "customFeedTitles" (D.dict D.string))
                |> Result.withDefault Dict.empty
    in
    ( { feedUrls = feedUrls
      , articles = []
      , feedTitles = Dict.empty
      , inputUrl = ""
      , loading = Set.fromList feedUrls
      , errors = Dict.empty
      , readArticles = readArticles
      , activeFilter = AllFeeds
      , sidebarMini = False
      , openTabs = []
      , activeTabLink = Nothing
      , topics = topics
      , collapsedTopics = Set.empty
      , favourites = favourites
      , articleLabels = articleLabels
      , newTopicInput = ""
      , labelInput = ""
      , customFeedTitles = customFeedTitles
      , editingFeedName = Nothing
      , feedNameInput = ""
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
    | SetFilter FeedFilter
    | RefreshAll
    | RefreshFeed String
    | ToggleSidebar
    | SelectArticle Article
    | CloseTab String
    | ActivateTab (Maybe String)
    | PrintArticle
    | NewTopicInputChanged String
    | CreateTopic
    | DeleteTopic String
    | AssignFeedToTopic String String
    | ToggleTopic String
    | ToggleFavourite String
    | LabelInputChanged String
    | AddLabel String
    | RemoveLabel String String
    | StartEditFeedName String
    | FeedNameInputChanged String
    | SaveFeedName String
    | CancelFeedNameEdit
    | ResetFeedName String


type alias RssFeedResponse =
    { feedTitle : String
    , feedUrl : String
    , items : List RssItem
    }


type alias RssItem =
    { title : String
    , link : String
    , description : String
    , content : String
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
                    case model.activeFilter of
                        ByFeed u ->
                            if u == url then AllFeeds else model.activeFilter

                        _ ->
                            model.activeFilter

                newTopics =
                    Dict.map (\_ urls -> List.filter ((/=) url) urls) model.topics
            in
            ( { model
                | feedUrls = newUrls
                , articles = List.filter (\a -> a.feedUrl /= url) model.articles
                , feedTitles = Dict.remove url model.feedTitles
                , loading = Set.remove url model.loading
                , errors = Dict.remove url model.errors
                , activeFilter = newFilter
                , topics = newTopics
              }
            , Cmd.batch
                [ saveFeedUrls (E.list E.string newUrls)
                , saveTopics (E.dict identity (E.list E.string) newTopics)
                ]
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
                                    , content = item.content
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

        SetFilter f ->
            ( { model | activeFilter = f }, Cmd.none )

        RefreshAll ->
            ( { model | loading = Set.fromList model.feedUrls }
            , Cmd.batch (List.map fetchFeed model.feedUrls)
            )

        RefreshFeed url ->
            ( { model | loading = Set.insert url model.loading }
            , fetchFeed url
            )

        ToggleSidebar ->
            ( { model | sidebarMini = not model.sidebarMini }, Cmd.none )

        SelectArticle art ->
            let
                newRead =
                    Set.insert art.link model.readArticles

                alreadyOpen =
                    List.any (\a -> a.link == art.link) model.openTabs

                newTabs =
                    if alreadyOpen then
                        model.openTabs

                    else
                        model.openTabs ++ [ art ]
            in
            ( { model
                | openTabs = newTabs
                , activeTabLink = Just art.link
                , readArticles = newRead
                , labelInput = ""
              }
            , Cmd.batch
                [ renderContent (E.object [ ( "id", E.string "article-content" ), ( "html", E.string art.content ) ])
                , saveReadArticles (E.list E.string (Set.toList newRead))
                ]
            )

        CloseTab link ->
            let
                newTabs =
                    List.filter (\a -> a.link /= link) model.openTabs

                newActiveLink =
                    if model.activeTabLink /= Just link then
                        model.activeTabLink

                    else
                        List.head newTabs |> Maybe.map .link
            in
            ( { model | openTabs = newTabs, activeTabLink = newActiveLink }
            , case newActiveLink of
                Just l ->
                    case List.head (List.filter (\a -> a.link == l) newTabs) of
                        Just a ->
                            renderContent (E.object [ ( "id", E.string "article-content" ), ( "html", E.string a.content ) ])

                        Nothing ->
                            Cmd.none

                Nothing ->
                    Cmd.none
            )

        ActivateTab maybeLink ->
            ( { model | activeTabLink = maybeLink, labelInput = "" }
            , case maybeLink of
                Just link ->
                    case List.head (List.filter (\a -> a.link == link) model.openTabs) of
                        Just art ->
                            renderContent (E.object [ ( "id", E.string "article-content" ), ( "html", E.string art.content ) ])

                        Nothing ->
                            Cmd.none

                Nothing ->
                    Cmd.none
            )

        PrintArticle ->
            ( model, printArticle E.null )

        NewTopicInputChanged s ->
            ( { model | newTopicInput = s }, Cmd.none )

        CreateTopic ->
            let
                name =
                    String.trim model.newTopicInput
            in
            if String.isEmpty name || Dict.member name model.topics then
                ( { model | newTopicInput = "" }, Cmd.none )

            else
                let
                    newTopics =
                        Dict.insert name [] model.topics
                in
                ( { model | topics = newTopics, newTopicInput = "" }
                , saveTopics (E.dict identity (E.list E.string) newTopics)
                )

        DeleteTopic name ->
            let
                newTopics =
                    Dict.remove name model.topics
            in
            ( { model | topics = newTopics }
            , saveTopics (E.dict identity (E.list E.string) newTopics)
            )

        AssignFeedToTopic feedUrl topicName ->
            let
                cleaned =
                    Dict.map (\_ urls -> List.filter ((/=) feedUrl) urls) model.topics

                newTopics =
                    if topicName == "" then
                        cleaned

                    else
                        Dict.update topicName
                            (Maybe.map
                                (\urls ->
                                    if List.member feedUrl urls then
                                        urls

                                    else
                                        urls ++ [ feedUrl ]
                                )
                            )
                            cleaned
            in
            ( { model | topics = newTopics }
            , saveTopics (E.dict identity (E.list E.string) newTopics)
            )

        ToggleTopic name ->
            ( { model
                | collapsedTopics =
                    if Set.member name model.collapsedTopics then
                        Set.remove name model.collapsedTopics

                    else
                        Set.insert name model.collapsedTopics
              }
            , Cmd.none
            )

        ToggleFavourite link ->
            let
                newFavs =
                    if Set.member link model.favourites then
                        Set.remove link model.favourites

                    else
                        Set.insert link model.favourites
            in
            ( { model | favourites = newFavs }
            , saveFavourites (E.list E.string (Set.toList newFavs))
            )

        LabelInputChanged s ->
            ( { model | labelInput = s }, Cmd.none )

        AddLabel articleLink ->
            let
                label =
                    String.trim model.labelInput
            in
            if String.isEmpty label then
                ( model, Cmd.none )

            else
                let
                    current =
                        Dict.get articleLink model.articleLabels |> Maybe.withDefault []

                    newLabels =
                        if List.member label current then
                            model.articleLabels

                        else
                            Dict.insert articleLink (current ++ [ label ]) model.articleLabels
                in
                ( { model | articleLabels = newLabels, labelInput = "" }
                , saveArticleLabels (E.dict identity (E.list E.string) newLabels)
                )

        RemoveLabel articleLink label ->
            let
                current =
                    Dict.get articleLink model.articleLabels |> Maybe.withDefault []

                updated =
                    List.filter ((/=) label) current

                newLabels =
                    if List.isEmpty updated then
                        Dict.remove articleLink model.articleLabels

                    else
                        Dict.insert articleLink updated model.articleLabels
            in
            ( { model | articleLabels = newLabels }
            , saveArticleLabels (E.dict identity (E.list E.string) newLabels)
            )

        StartEditFeedName url ->
            let
                current =
                    Dict.get url model.customFeedTitles
                        |> Maybe.withDefault
                            (Dict.get url model.feedTitles
                                |> Maybe.withDefault (shortenUrl url)
                            )
            in
            ( { model | editingFeedName = Just url, feedNameInput = current }
            , Cmd.none
            )

        FeedNameInputChanged s ->
            ( { model | feedNameInput = s }, Cmd.none )

        SaveFeedName url ->
            let
                name =
                    String.trim model.feedNameInput

                newCustom =
                    if String.isEmpty name then
                        Dict.remove url model.customFeedTitles

                    else
                        Dict.insert url name model.customFeedTitles
            in
            ( { model
                | customFeedTitles = newCustom
                , editingFeedName = Nothing
                , feedNameInput = ""
              }
            , saveCustomFeedTitles (E.dict identity E.string newCustom)
            )

        CancelFeedNameEdit ->
            ( { model | editingFeedName = Nothing, feedNameInput = "" }, Cmd.none )

        ResetFeedName url ->
            let
                newCustom =
                    Dict.remove url model.customFeedTitles
            in
            ( { model | customFeedTitles = newCustom }
            , saveCustomFeedTitles (E.dict identity E.string newCustom)
            )



-- HELPERS


sortArticles : List Article -> List Article
sortArticles =
    List.sortWith (\a b -> compare b.pubDate a.pubDate)


filteredArticles : Model -> List Article
filteredArticles model =
    case model.activeFilter of
        AllFeeds ->
            model.articles

        ByFeed url ->
            List.filter (\a -> a.feedUrl == url) model.articles

        ByLabel label ->
            if label == "★ Starred" then
                List.filter (\a -> Set.member a.link model.favourites) model.articles

            else
                List.filter
                    (\a ->
                        Dict.get a.link model.articleLabels
                            |> Maybe.withDefault []
                            |> List.member label
                    )
                    model.articles


allLabels : Model -> List String
allLabels model =
    Dict.values model.articleLabels
        |> List.concat
        |> Set.fromList
        |> Set.toList
        |> List.sort


displayFeedTitle : Model -> String -> String
displayFeedTitle model url =
    Dict.get url model.customFeedTitles
        |> Maybe.withDefault
            (Dict.get url model.feedTitles
                |> Maybe.withDefault (shortenUrl url)
            )


feedTopic : Dict String (List String) -> String -> String
feedTopic topics feedUrl =
    Dict.toList topics
        |> List.filterMap
            (\( topicName, urls ) ->
                if List.member feedUrl urls then
                    Just topicName

                else
                    Nothing
            )
        |> List.head
        |> Maybe.withDefault ""


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
        "01" ->
            "Jan"

        "02" ->
            "Feb"

        "03" ->
            "Mar"

        "04" ->
            "Apr"

        "05" ->
            "May"

        "06" ->
            "Jun"

        "07" ->
            "Jul"

        "08" ->
            "Aug"

        "09" ->
            "Sep"

        "10" ->
            "Oct"

        "11" ->
            "Nov"

        "12" ->
            "Dec"

        _ ->
            m



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
    D.map6 RssItem
        (D.field "title" D.string)
        (D.field "link" D.string)
        (D.oneOf [ D.field "description" D.string, D.succeed "" ])
        (D.oneOf [ D.field "content" D.string, D.field "description" D.string, D.succeed "" ])
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
    div [ classList [ ( "app", True ), ( "sidebar-mini", model.sidebarMini ) ] ]
        [ viewSidebar model
        , viewMainContent model
        ]


viewSidebar : Model -> Html Msg
viewSidebar model =
    let
        starCount =
            Set.size model.favourites

        labels =
            allLabels model

        assignedFeeds =
            Dict.values model.topics |> List.concat

        ungroupedFeeds =
            List.filter (\url -> not (List.member url assignedFeeds)) model.feedUrls

        topicSections =
            Dict.toList model.topics
                |> List.sortBy Tuple.first
                |> List.concatMap
                    (\( topicName, topicUrls ) ->
                        let
                            collapsed =
                                Set.member topicName model.collapsedTopics

                            feedsInTopic =
                                List.filter (\url -> List.member url model.feedUrls) topicUrls
                        in
                        [ viewTopicHeader model topicName (List.length feedsInTopic) collapsed ]
                            ++ (if collapsed then
                                    []

                                else
                                    List.map (viewFeedNavItem model) feedsInTopic
                               )
                    )
    in
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
                , title
                    (if model.sidebarMini then
                        "Expand sidebar"

                     else
                        "Collapse sidebar"
                    )
                ]
                [ text
                    (if model.sidebarMini then
                        "›"

                     else
                        "‹"
                    )
                ]
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
            ([ viewAllFeedsItem model
             , viewStarredItem model starCount
             ]
                ++ topicSections
                ++ List.map (viewFeedNavItem model) ungroupedFeeds
                ++ (if not (List.isEmpty labels) then
                        [ div [ class "nav-section-label" ] [ text "Labels" ] ]
                            ++ List.map (viewLabelNavItem model) labels

                    else
                        []
                   )
                ++ [ viewNewTopicInput model ]
            )
        ]


viewTopicHeader : Model -> String -> Int -> Bool -> Html Msg
viewTopicHeader model topicName feedCount collapsed =
    div [ class "topic-header" ]
        [ button
            [ class "topic-toggle"
            , onClick (ToggleTopic topicName)
            , title (if collapsed then "Expand" else "Collapse")
            ]
            [ text (if collapsed then "▶" else "▼")
            , span [ class "topic-name" ] [ text topicName ]
            , span [ class "topic-count" ] [ text (String.fromInt feedCount) ]
            ]
        , button
            [ class "btn-topic-delete"
            , onClick (DeleteTopic topicName)
            , title ("Delete topic \"" ++ topicName ++ "\"")
            ]
            [ text "×" ]
        ]


viewNewTopicInput : Model -> Html Msg
viewNewTopicInput model =
    div [ class "new-topic-row" ]
        [ input
            [ type_ "text"
            , placeholder "New topic…"
            , value model.newTopicInput
            , onInput NewTopicInputChanged
            , onEnter CreateTopic
            , class "new-topic-input"
            ]
            []
        , button
            [ class "btn-topic-add"
            , onClick CreateTopic
            , disabled (String.isEmpty (String.trim model.newTopicInput))
            , title "Create topic"
            ]
            [ text "+" ]
        ]


viewAllFeedsItem : Model -> Html Msg
viewAllFeedsItem model =
    let
        unreadCount =
            List.filter (\a -> not (Set.member a.link model.readArticles)) model.articles
                |> List.length

        isActive =
            model.activeFilter == AllFeeds
    in
    button
        [ classList [ ( "feed-nav-item", True ), ( "active", isActive ) ]
        , onClick (SetFilter AllFeeds)
        , title "All Feeds"
        ]
        [ span [ class "feed-nav-icon" ] [ text "●" ]
        , span [ class "feed-nav-title" ] [ text "All Feeds" ]
        , if unreadCount > 0 then
            span [ class "badge" ] [ text (String.fromInt unreadCount) ]

          else
            text ""
        ]


viewStarredItem : Model -> Int -> Html Msg
viewStarredItem model starCount =
    let
        isActive =
            model.activeFilter == ByLabel "★ Starred"
    in
    button
        [ classList [ ( "feed-nav-item", True ), ( "active", isActive ) ]
        , onClick (SetFilter (ByLabel "★ Starred"))
        , title "Starred articles"
        ]
        [ span [ class "feed-nav-icon" ] [ text "★" ]
        , span [ class "feed-nav-title" ] [ text "Starred" ]
        , if starCount > 0 then
            span [ class "badge" ] [ text (String.fromInt starCount) ]

          else
            text ""
        ]


viewLabelNavItem : Model -> String -> Html Msg
viewLabelNavItem model label =
    let
        isActive =
            model.activeFilter == ByLabel label

        count =
            Dict.values model.articleLabels
                |> List.concat
                |> List.filter ((==) label)
                |> List.length
    in
    button
        [ classList [ ( "feed-nav-item", True ), ( "active", isActive ) ]
        , onClick (SetFilter (ByLabel label))
        , title label
        ]
        [ span [ class "feed-nav-icon" ] [ text "⬡" ]
        , span [ class "feed-nav-title" ] [ text label ]
        , if count > 0 then
            span [ class "badge" ] [ text (String.fromInt count) ]

          else
            text ""
        ]


viewFeedNavItem : Model -> String -> Html Msg
viewFeedNavItem model url =
    let
        feedTitle =
            displayFeedTitle model url

        unreadCount =
            List.filter (\a -> a.feedUrl == url && not (Set.member a.link model.readArticles)) model.articles
                |> List.length

        isLoading =
            Set.member url model.loading

        hasError =
            Dict.member url model.errors

        isActive =
            model.activeFilter == ByFeed url

        currentTopic =
            feedTopic model.topics url

        isEditing =
            model.editingFeedName == Just url

        hasCustomName =
            Dict.member url model.customFeedTitles
    in
    div [ classList [ ( "feed-nav-item-wrapper", True ), ( "is-active", isActive ) ] ]
        [ if isEditing then
            div [ class "feed-name-edit-row" ]
                [ input
                    [ type_ "text"
                    , value model.feedNameInput
                    , onInput FeedNameInputChanged
                    , onEnter (SaveFeedName url)
                    , on "keydown"
                        (D.field "key" D.string
                            |> D.andThen
                                (\k ->
                                    if k == "Escape" then
                                        D.succeed CancelFeedNameEdit

                                    else
                                        D.fail ""
                                )
                        )
                    , class "feed-name-input"
                    , autofocus True
                    ]
                    []
                , button
                    [ class "btn-name-save"
                    , onClick (SaveFeedName url)
                    , title "Save"
                    ]
                    [ text "✓" ]
                , button
                    [ class "btn-name-cancel"
                    , onClick CancelFeedNameEdit
                    , title "Cancel"
                    ]
                    [ text "✗" ]
                ]

          else
            button
                [ classList
                    [ ( "feed-nav-item", True )
                    , ( "active", isActive )
                    , ( "has-error", hasError )
                    ]
                , onClick (SetFilter (ByFeed url))
                , title feedTitle
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
        , if not isEditing then
            div [ class "feed-item-actions" ]
                [ button
                    [ class "btn-edit-name"
                    , onClick (StartEditFeedName url)
                    , title "Rename feed"
                    ]
                    [ text "✎" ]
                , if hasCustomName then
                    button
                        [ class "btn-edit-name"
                        , onClick (ResetFeedName url)
                        , title "Reset to original name"
                        ]
                        [ text "↺" ]

                  else
                    text ""
                , if not (Dict.isEmpty model.topics) then
                    select
                        [ class "topic-select"
                        , onInput (AssignFeedToTopic url)
                        , title "Assign to topic"
                        ]
                        (option [ value "", selected (currentTopic == "") ] [ text "—" ]
                            :: List.map
                                (\topicName ->
                                    option
                                        [ value topicName
                                        , selected (currentTopic == topicName)
                                        ]
                                        [ text topicName ]
                                )
                                (Dict.keys model.topics |> List.sort)
                        )

                  else
                    text ""
                , button
                    [ class "btn-remove"
                    , onClick (RemoveFeed url)
                    , title ("Remove \"" ++ feedTitle ++ "\"")
                    ]
                    [ text "×" ]
                ]

          else
            text ""
        ]


viewTabBar : Model -> Html Msg
viewTabBar model =
    div [ class "tab-bar" ]
        (button
            [ classList [ ( "tab-item", True ), ( "tab-active", model.activeTabLink == Nothing ) ]
            , onClick (ActivateTab Nothing)
            ]
            [ text "Articles" ]
            :: List.map
                (\art ->
                    let
                        isActive =
                            model.activeTabLink == Just art.link

                        label =
                            if String.length art.title > 28 then
                                String.left 25 art.title ++ "…"

                            else
                                art.title
                    in
                    div [ classList [ ( "tab-item", True ), ( "tab-active", isActive ) ] ]
                        [ button
                            [ class "tab-label"
                            , onClick (ActivateTab (Just art.link))
                            , title art.title
                            ]
                            [ text label ]
                        , button
                            [ class "tab-close"
                            , onClick (CloseTab art.link)
                            , title "Close tab"
                            ]
                            [ text "×" ]
                        ]
                )
                model.openTabs
        )


viewArticleReader : Model -> Article -> Html Msg
viewArticleReader model art =
    let
        isFav =
            Set.member art.link model.favourites

        labels =
            Dict.get art.link model.articleLabels |> Maybe.withDefault []
    in
    div [ class "article-reader" ]
        [ div [ class "reader-toolbar" ]
            [ button
                [ class "btn-ghost"
                , onClick (CloseTab art.link)
                ]
                [ text "× Close" ]
            , button
                [ classList [ ( "btn-ghost", True ), ( "btn-fav", True ), ( "is-fav", isFav ) ]
                , onClick (ToggleFavourite art.link)
                , title (if isFav then "Remove from starred" else "Add to starred")
                ]
                [ text (if isFav then "★ Starred" else "☆ Star") ]
            , a
                [ href art.link
                , target "_blank"
                , rel "noopener noreferrer"
                , class "btn-ghost"
                ]
                [ text "Open in browser ↗" ]
            , button
                [ class "btn-ghost"
                , onClick PrintArticle
                , title "Save as PDF / Print"
                ]
                [ text "⎙ Save as PDF" ]
            ]
        , div [ class "reader-body" ]
            [ h1 [ class "reader-title" ] [ text art.title ]
            , div [ class "reader-meta" ]
                [ span [ class "article-feed-name" ] [ text art.feedTitle ]
                , if art.pubDate /= "" then
                    span [] [ text (" · " ++ formatDate art.pubDate) ]

                  else
                    text ""
                , if art.author /= "" then
                    span [] [ text (" · " ++ art.author) ]

                  else
                    text ""
                ]
            , div [ class "reader-labels" ]
                (List.map
                    (\label ->
                        span [ class "label-chip" ]
                            [ text label
                            , button
                                [ class "label-remove"
                                , onClick (RemoveLabel art.link label)
                                , title ("Remove label \"" ++ label ++ "\"")
                                ]
                                [ text "×" ]
                            ]
                    )
                    labels
                    ++ [ div [ class "label-add-row" ]
                            [ input
                                [ type_ "text"
                                , placeholder "Add label…"
                                , value model.labelInput
                                , onInput LabelInputChanged
                                , onEnter (AddLabel art.link)
                                , class "label-input"
                                ]
                                []
                            , button
                                [ class "btn-label-add"
                                , onClick (AddLabel art.link)
                                , disabled (String.isEmpty (String.trim model.labelInput))
                                ]
                                [ text "+" ]
                            ]
                       ]
                )
            , div [ id "article-content", class "reader-content" ] []
            ]
        ]


viewMainContent : Model -> Html Msg
viewMainContent model =
    let
        headerArea =
            if List.isEmpty model.openTabs then
                viewMainHeader model

            else
                viewTabBar model

        contentArea =
            case model.activeTabLink of
                Nothing ->
                    viewArticleList model

                Just link ->
                    case List.head (List.filter (\a -> a.link == link) model.openTabs) of
                        Just art ->
                            viewArticleReader model art

                        Nothing ->
                            viewArticleList model
    in
    main_
        [ class "main" ]
        [ headerArea
        , viewErrors model
        , contentArea
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
                AllFeeds ->
                    "All Articles"

                ByFeed url ->
                    displayFeedTitle model url

                ByLabel label ->
                    label
    in
    div [ class "main-header" ]
        [ h2 [ class "main-title" ] [ text pageTitle ]
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

        isFav =
            Set.member art.link model.favourites

        description =
            art.description
                |> stripHtml
                |> truncateText 280

        labels =
            Dict.get art.link model.articleLabels |> Maybe.withDefault []
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
        , div [ class "article-title-row" ]
            [ button
                [ class "article-title"
                , onClick (SelectArticle art)
                ]
                [ text art.title ]
            , button
                [ classList [ ( "btn-fav-card", True ), ( "is-fav", isFav ) ]
                , onClick (ToggleFavourite art.link)
                , title (if isFav then "Remove star" else "Star article")
                ]
                [ text (if isFav then "★" else "☆") ]
            , a
                [ href art.link
                , target "_blank"
                , rel "noopener noreferrer"
                , class "article-external-link"
                , title "Open in browser"
                ]
                [ text "↗" ]
            ]
        , if not (List.isEmpty labels) then
            div [ class "article-labels" ]
                (List.map (\l -> span [ class "label-chip-sm" ] [ text l ]) labels)

          else
            text ""
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
