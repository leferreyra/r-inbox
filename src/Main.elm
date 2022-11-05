module Main exposing (..)

import Browser
import Css.Global
import Html.Styled exposing (..)
import Html.Styled.Attributes as Attr
import Http
import Json.Decode exposing (Decoder, at, list, string)
import Tailwind.Utilities as Tw



---- MODEL ----


type Model
    = Failure
    | Loading
    | Success (List Post)


type alias Post =
    { title : String
    }


init : ( Model, Cmd Msg )
init =
    ( Loading, getPosts )



---- UPDATE ----


type Msg
    = GotPosts (Result Http.Error (List Post))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotPosts result ->
            case result of
                Ok posts ->
                    ( Success posts, Cmd.none )

                Err _ ->
                    ( Failure, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
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
    Browser.element
        { view = view >> toUnstyled
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
