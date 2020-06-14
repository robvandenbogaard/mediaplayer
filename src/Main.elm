module Main exposing (main)

import Browser
import Browser.Events
import Browser.Navigation as Navigation exposing (Key)
import Embed.Youtube
import Embed.Youtube.Attributes
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Decode exposing (Decoder)
import Url exposing (Url)
import Url.Parser exposing ((</>), (<?>))
import Url.Parser.Query


type alias Model =
    { entry : Entry
    , key : Key
    , route : List String
    , input : String
    , shift : Bool
    }


type Entry
    = Menu String (List Entry)
    | Radio String String
    | Youtube String (Maybe String)
    | Settings (List Entry)
    | PowerOff String String


type Msg
    = Back
    | Input String
    | Key Char
    | UrlChanged Url
    | LinkClicked Browser.UrlRequest


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = \model -> Browser.Events.onClick clickDecoder
        , onUrlRequest = LinkClicked
        , onUrlChange = UrlChanged
        }


clickDecoder : Decoder Msg
clickDecoder =
    Decode.at [ "target", "nodeName" ] Decode.string
        |> Decode.andThen
            (\targetName ->
                if targetName == "BODY" then
                    Decode.succeed Back

                else
                    Decode.fail ""
            )


init : () -> Url -> Navigation.Key -> ( Model, Cmd Msg )
init _ url key =
    let
        route =
            routeFromUrl url
    in
    ( { entry = entryFromRoute route
      , key = key
      , route = route
      , input = ""
      , shift = False
      }
    , Cmd.none
    )


namedEntry : Entry -> Maybe ( String, Entry )
namedEntry e =
    case e of
        Menu k _ ->
            Just ( k, e )

        Radio k _ ->
            Just ( k, e )

        Youtube k _ ->
            Just ( k, e )

        PowerOff k _ ->
            Just ( k, e )

        _ ->
            Nothing


entryFromRoute : List String -> Entry
entryFromRoute route =
    let
        findEntry k =
            List.foldl
                (\e a ->
                    case e of
                        Settings _ ->
                            if k == "settings" then
                                Just e

                            else
                                a

                        _ ->
                            case namedEntry e of
                                Just ( k_, v ) ->
                                    if k == k_ then
                                        Just v

                                    else
                                        a

                                _ ->
                                    a
                )
                Nothing
    in
    List.foldr
        (\key a ->
            case a of
                Menu _ entries ->
                    case findEntry key (Debug.log "entries" entries) of
                        Nothing ->
                            a

                        Just entry ->
                            entry

                Youtube label Nothing ->
                    Youtube label (Just key)

                _ ->
                    a
        )
        menu
        (Debug.log "route for entry" route)


routeFromUrl : { a | query : Maybe String } -> List String
routeFromUrl { query } =
    case query of
        Nothing ->
            []

        Just "" ->
            []

        Just q ->
            String.split "/" q
                |> List.filterMap Url.percentDecode
                |> List.reverse


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LinkClicked (Browser.Internal url) ->
            ( model
            , Navigation.pushUrl model.key (Url.toString url)
            )

        LinkClicked (Browser.External url) ->
            ( model, Navigation.load url )

        UrlChanged url ->
            let
                route =
                    routeFromUrl url
            in
            ( { model | route = route, entry = entryFromRoute route }, Cmd.none )

        Back ->
            let
                cmd =
                    case model.route of
                        [] ->
                            Navigation.pushUrl model.key "?settings"

                        _ ->
                            let
                                url =
                                    "?"
                                        ++ (List.drop 1 model.route
                                                |> List.reverse
                                                |> String.join "/"
                                           )
                            in
                            case model.entry of
                                Radio _ _ ->
                                    Navigation.load url

                                _ ->
                                    Navigation.pushUrl model.key url
            in
            ( model
            , cmd
            )

        Input value ->
            ( { model | input = value }, Cmd.none )

        Key c ->
            case c of
                '<' ->
                    ( { model | input = String.dropRight 1 model.input }, Cmd.none )

                '^' ->
                    ( { model | shift = not model.shift }, Cmd.none )

                _ ->
                    ( { model | input = model.input ++ String.fromList [ c ] }, Cmd.none )


view model =
    { title =
        case model.entry of
            Menu name _ ->
                "Mediaplayer - " ++ name

            Radio name _ ->
                "Radio - " ++ name

            Youtube name _ ->
                name

            _ ->
                "Media player"
    , body = [ content model ]
    }


link : String -> List String -> Html Msg
link lbl route =
    a [ href ("?" ++ String.join "/" (List.reverse route)) ] [ text lbl ]


keyboard : Bool -> Html Msg
keyboard shifted =
    let
        keys =
            [ "1234567890"
            , "QWERTYUIOP"
            , "ASDFGHJKL-"
            , "^ZXCVBNM_<"
            ]

        shift c =
            if shifted then
                c

            else
                Char.toLower c

        key c =
            td [ onClick <| Key c ] [ text <| String.fromList [ c ] ]
    in
    table [] <|
        List.map
            (\r ->
                tr [] <|
                    List.map (shift >> key) (String.toList r)
            )
            keys


content : Model -> Html Msg
content model =
    case model.entry of
        Menu name entries ->
            let
                entry e =
                    case e of
                        Just ( n, _ ) ->
                            [ link n (n :: model.route), br [] [] ]

                        Nothing ->
                            []
            in
            nav
                []
                (List.concatMap (entry << namedEntry) entries)

        Settings entries ->
            let
                entry e =
                    case e of
                        PowerOff label url ->
                            [ a [ href url ] [ text label ] ]

                        _ ->
                            []
            in
            nav
                []
                (List.concatMap entry entries)

        Radio name url ->
            article
                []
                [ text name
                , br [] []
                , audio [ src url, autoplay True ] []
                ]

        Youtube _ maybeCode ->
            case maybeCode of
                Nothing ->
                    article
                        []
                        [ keyboard model.shift
                        , input [ onInput Input, placeholder "Vul Youtube code in", value model.input ] []
                        , br [] []
                        , link "Kijken" (model.input :: model.route)
                        ]

                Just code ->
                    Embed.Youtube.fromString code
                        |> Embed.Youtube.attributes
                            [ Embed.Youtube.Attributes.width 640
                            , Embed.Youtube.Attributes.height 400
                            , Embed.Youtube.Attributes.autoplay
                            , Embed.Youtube.Attributes.language "nl"
                            , Embed.Youtube.Attributes.modestBranding
                            ]
                        |> Embed.Youtube.toHtml

        PowerOff _ _ ->
            article
                []
                [ text "-"
                ]


menu =
    Menu "start"
        [ Menu "Radio"
            [ Menu "NPO"
                [ Radio "Radio 1" "http://icecast.omroep.nl/radio1-bb-mp3"
                , Radio "Radio 2" "http://icecast.omroep.nl/radio2-bb-mp3"
                , Radio "Radio 3" "http://icecast.omroep.nl/radio3-bb-mp3"
                , Radio "Radio 4" "http://icecast.omroep.nl/radio4-bb-mp3"
                , Radio "Radio 5" "http://icecast.omroep.nl/radio5-bb-mp3"
                , Radio "Radio 2 Soul & Jazz" "http://icecast.omroep.nl/radio6-bb-mp3"
                , Menu "FunX"
                    [ Radio "FunX NL" "https://icecast.omroep.nl/funx-bb-mp3"
                    , Radio "FunX Amsterdam" "https://icecast.omroep.nl/funx-amsterdam-bb-mp3"
                    , Radio "FunX Rotterdam" "https://icecast.omroep.nl/funx-rotterdam-bb-mp3"
                    , Radio "FunX Utrecht" "https://icecast.omroep.nl/funx-utrecht-bb-mp3"
                    , Radio "FunX Den Haag" "https://icecast.omroep.nl/funx-denhaag-bb-mp3"
                    , Radio "FunX Latin" "https://icecast.omroep.nl/funx-latin-bb-mp3"
                    , Radio "FunX Dance" "https://icecast.omroep.nl/funx-dance-bb-mp3"
                    , Radio "FunX Slow Jamz" "https://icecast.omroep.nl/funx-slowjamz-bb-mp3"
                    ]
                ]
            , Radio "Slam FM" "http://stream.slam.nl/slamaac"
            , Radio "BNR Nieuwsradio" "http://icecast-bnr-cdp.triple-it.nl/bnr_mp3_96_04"
            , Radio "Sky Radio" "http://playerservices.streamtheworld.com/api/livestream-redirect/SKYRADIO.mp3"
            , Menu "Sublime"
                [ Radio "Sublime Radio" "http://stream.sublimefm.nl/SublimeFM_mp3"
                , Radio "Arrow Jazz" "http://17873.live.streamtheworld.com/SUBLIMEARROWJAZZ.mp3"
                , Radio "Pure Jazz" "http://20863.live.streamtheworld.com/SUBLIMEPUREJAZZ.mp3"
                , Radio "Soul" "http://20863.live.streamtheworld.com/SUBLIMESOUL.mp3"
                , Radio "Smooth" "http://20863.live.streamtheworld.com/SUBLIMESMOOTH.mp3"
                , Radio "Sublime 500" "http://20863.live.streamtheworld.com/SUBLIME500.mp3"
                ]
            , Menu "Regio / Diversen"
                [ Radio "ORTS" "https://media.maxx-xs.nl/mediaplayer/sasx.php?serps=LY1rCgMhDITvkhNo1KxmT-NqBKEFaRZKKb17ld1$3wzzyBz4q2wNQ-sPOT9DeoVdGRloW-AYxmURgyebPC0RGHSlb-7lOdzixPCWQ$t578w6GrMozoYUldfQJf28zIGy2Y6QCImSKRhRovWuYmjN5eQPZ2uRAPvvDw"
                , Radio "Radio NH" "http://ice.cr1.streamzilla.xlcdn.com:8000/sz%3Dnhnieuws%3DNHRadio_mp3"
                , Radio "Radio NL Kids" "http://stream.radionlkids.nl/rnlkids"
                , Radio "Salto CaribbeanFM" "https://icecast.streamone.net/YoRFHeMSYJ42"
                , Radio "Salto Mokum Radio" "https://icecast.streamone.net/crJNGaeQKRQy"
                , Radio "Radio Salto" "https://icecast.streamone.net/46ANH44CYA8S"
                , Radio "Salto RAZO" "https://icecast.streamone.net/c6YFGcOSYMQS"
                ]
            , Menu "Internationaal"
                [ Menu "Belgie"
                    [ Radio "Radio 1" "http://icecast.vrtcdn.be/radio1-high.mp3"
                    , Radio "Studio Brussel" "http://icecast.vrtcdn.be/stubru-high.mp3"
                    , Radio "VRT Ketnet Hits" "http://icecast.vrtcdn.be/ketnetradio-high.mp3"
                    , Radio "VRT Nieuws" "http://progressive-audio.lwc.vrtcdn.be/content/fixed/11_11niws-snip_hi.mp3"
                    ]
                , Menu "Duitsland"
                    [ Radio "WDR 4" "http://wdr-wdr4-live.icecast.wdr.de/wdr/wdr4/live/mp3/128/stream.mp3"
                    ]
                , Menu "Engeland"
                    [ Radio "BBC World Service" "http://bbcwssc.ic.llnwd.net/stream/bbcwssc_mp1_ws-einws"
                    ]
                , Menu "Frankrijk"
                    [ Radio "France Musique" "http://direct.francemusique.fr/live/francemusique-midfi.mp3"
                    , Radio "FIP" "http://icecast.radiofrance.fr/fip-midfi.mp3"
                    , Radio "FIP Monde" "http://direct.fipradio.fr/live/fip-webradio4.mp3"
                    ]
                , Menu "Noorwegen"
                    [ Radio "NRK Alltid Nyheter" "http://lyd.nrk.no/nrk_radio_alltid_nyheter_mp3_h"
                    , Radio "NRK Folkemusik" "http://lyd.nrk.no/nrk_radio_folkemusikk_mp3_h"
                    ]
                , Menu "Spanje"
                    [ Radio "RTVE Radio Nacional" "https://rne.rtveradio.cires21.com/rne_hc.mp3"
                    ]
                ]
            ]
        , Youtube "YouTube" Nothing
        , Settings
            [ PowerOff "Uitzetten" "/poweroff" ]
        ]
