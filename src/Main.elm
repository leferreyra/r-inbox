module Main exposing (..)

import Browser
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes as Attr
import Http
import Json.Decode exposing (Decoder, at, list, string)
import Url
import Url.Parser exposing ((</>), Parser, oneOf)



---- MODEL ----


type alias Model =
    { route : Maybe Route
    , state : State
    }


type State
    = Failure
    | Loading
    | Success (List Post)


type alias Post =
    { title : String
    }


type Route
    = Subreddit String


routeParser : Parser (Route -> a) a
routeParser =
    oneOf
        [ Url.Parser.map Subreddit Url.Parser.string
        ]


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url _ =
    let
        route =
            Url.Parser.parse routeParser url

        subreddit =
            case route of
                Nothing ->
                    "all"

                Just (Subreddit r) ->
                    r
    in
    ( { route = route, state = Loading }, getPosts subreddit )



---- UPDATE ----


type Msg
    = GotPosts (Result Http.Error (List Post))
    | UrlChanged
    | LinkClicked


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotPosts result ->
            case result of
                Ok posts ->
                    ( { model | state = Success posts }, Cmd.none )

                Err _ ->
                    ( { model | state = Failure }, Cmd.none )

        UrlChanged ->
            ( model, Cmd.none )

        LinkClicked ->
            ( model, Cmd.none )



---- VIEW ----


view : Model -> Browser.Document Msg
view model =
    { title = "R Inbox"
    , body =
        [ div
            []
            [ div
                []
                [ case model.route of
                    Nothing ->
                        text "Nothing to see here..."

                    Just _ ->
                        case model.state of
                            Success posts ->
                                ul []
                                    (List.map viewPost posts)

                            Failure ->
                                text "An error happened :("

                            Loading ->
                                text "..."
                ]
            ]
        ]
    }


viewPost : Post -> Html Msg
viewPost post =
    li
        []
        [ text post.title ]



-- HTTP


getPosts : String -> Cmd Msg
getPosts subreddit =
    Http.get
        { url = "https://www.reddit.com/r/" ++ subreddit ++ ".json"
        , expect = Http.expectJson GotPosts postsDecoder
        }


postDecoder : Decoder Post
postDecoder =
    Json.Decode.map Post (at [ "data", "title" ] string)


postsDecoder : Decoder (List Post)
postsDecoder =
    at [ "data", "children" ] (list postDecoder)



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.application
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        , onUrlChange = \_ -> UrlChanged
        , onUrlRequest = \_ -> LinkClicked
        }
