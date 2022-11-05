module Main exposing (..)

import Browser
import Browser.Navigation as Nav
import Css.Global
import Html.Styled exposing (..)
import Html.Styled.Attributes as Attr
import Http
import Json.Decode exposing (Decoder, at, list, string)
import Tailwind.Utilities as Tw
import Url



---- MODEL ----


type Model
    = Failure
    | Loading
    | Success (List Post)


type alias Post =
    { title : String
    }


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    Debug.log url.path ( Loading, getPosts )



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
                    ( Success posts, Cmd.none )

                Err _ ->
                    ( Failure, Cmd.none )

        UrlChanged ->
            ( model, Cmd.none )

        LinkClicked ->
            ( model, Cmd.none )



---- VIEW ----


view : Model -> Browser.Document Msg
view model =
    { title = "R Inbox"
    , body =
        [ toUnstyled
            (div
                []
                [ Css.Global.global Tw.globalStyles
                , div
                    [ Attr.css
                        [ Tw.p_6
                        ]
                    ]
                    [ case model of
                        Success posts ->
                            ul []
                                (List.map viewPost posts)

                        Failure ->
                            text "An error happened :("

                        Loading ->
                            text "..."
                    ]
                ]
            )
        ]
    }


viewPost : Post -> Html Msg
viewPost post =
    li [] [ text post.title ]



-- HTTP


getPosts : Cmd Msg
getPosts =
    Http.get
        { url = "https://www.reddit.com/r/argentina.json"
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
