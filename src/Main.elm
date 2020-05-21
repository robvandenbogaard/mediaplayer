module Main exposing (main)

import Browser
import Browser.Events
import Browser.Navigation
import Embed.Youtube
import Embed.Youtube.Attributes
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Decode exposing (Decoder)


type alias Model =
    { mode : Mode
    , stations : Entries
    }


type Mode
    = Idle
    | Radio Entries
    | RadioPlaying Station
    | Youtube String Keyboard
    | YoutubePlaying String
    | Settings
    | Off


type alias Entries =
    List ( String, Entry )


type Entry
    = Group Entries
    | Url String


type alias Station =
    { name : String
    , url : String
    }


type alias Keyboard =
    { shift : Bool }


type Msg
    = Select Mode
    | Back
    | Input String
    | Key Char
    | PowerOff


main : Program () Model Msg
main =
    Browser.document
        { init = \_ -> ( { stations = radioStations, mode = Idle }, Cmd.none )
        , view = view
        , update = update
        , subscriptions = \model -> Browser.Events.onClick clickDecoder
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


update msg model =
    case msg of
        Select mode ->
            ( { model | mode = mode }, Cmd.none )

        Back ->
            let
                mode =
                    case model.mode of
                        Idle ->
                            Settings

                        Radio _ ->
                            Idle

                        Settings ->
                            Idle

                        Youtube _ _ ->
                            Idle

                        YoutubePlaying _ ->
                            Youtube "" { shift = False }

                        _ ->
                            Idle

                cmd =
                    case model.mode of
                        RadioPlaying _ ->
                            Browser.Navigation.reload

                        _ ->
                            Cmd.none
            in
            ( { model | mode = mode }, cmd )

        Input value ->
            let
                mode =
                    case model.mode of
                        Youtube _ keystate ->
                            Youtube value keystate

                        _ ->
                            model.mode
            in
            ( { model | mode = mode }, Cmd.none )

        Key c ->
            case model.mode of
                Youtube url keystate ->
                    case c of
                        '<' ->
                            ( { model | mode = Youtube (String.dropRight 1 url) keystate }, Cmd.none )

                        '^' ->
                            ( { model | mode = Youtube url { keystate | shift = not keystate.shift } }, Cmd.none )

                        _ ->
                            ( { model | mode = Youtube (url ++ String.fromList [ c ]) keystate }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        PowerOff ->
            -- Should send a request to local service for shutting down
            ( { model | mode = Off }, Cmd.none )


view model =
    { title =
        case model.mode of
            Idle ->
                "Media player"

            Radio _ ->
                "Radio"

            RadioPlaying { name } ->
                "Radio - " ++ name

            Youtube _ _ ->
                "Youtube"

            YoutubePlaying url ->
                "Youtube - " ++ url

            Settings ->
                "Settings"

            Off ->
                "Power off"
    , body = [ content model ]
    }


link lbl msg =
    a [ onClick msg ] [ text lbl ]


keyboard state =
    let
        keys =
            [ "1234567890"
            , "QWERTYUIOP"
            , "ASDFGHJKL-"
            , "^ZXCVBNM_<"
            ]

        shift c =
            if state.shift then
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


content model =
    case model.mode of
        Idle ->
            nav []
                [ link "Radio" <| Select (Radio model.stations)
                , br [] []
                , link "Youtube" <| Select (Youtube "" { shift = False })
                ]

        Radio stations ->
            radio stations

        RadioPlaying station ->
            article
                []
                [ text station.name
                , br [] []
                , audio [ src station.url, autoplay True ] []
                ]

        Youtube url keystate ->
            article
                []
                [ keyboard keystate
                , input [ onInput Input, placeholder "Vul Youtube code in", value url ] []
                , br [] []
                , a [ onClick <| Select (YoutubePlaying url) ] [ text "Kijken" ]
                ]

        YoutubePlaying url ->
            Embed.Youtube.fromString url
                |> Embed.Youtube.attributes
                    [ Embed.Youtube.Attributes.width 640
                    , Embed.Youtube.Attributes.height 400
                    , Embed.Youtube.Attributes.autoplay
                    , Embed.Youtube.Attributes.language "nl"
                    , Embed.Youtube.Attributes.modestBranding
                    ]
                |> Embed.Youtube.toHtml

        Settings ->
            article
                []
                [ a [ onClick PowerOff ] [ text "Power off" ] ]

        Off ->
            nav [] [ text "-" ]


radio stations =
    let
        entry ( name, e ) =
            case e of
                Group entries ->
                    [ link name <| Select (Radio entries), br [] [] ]

                Url url ->
                    [ link name <| Select (RadioPlaying { name = name, url = url }), br [] [] ]
    in
    nav
        []
        (List.concatMap entry <| stations)


radioStations =
    [ ( "NPO"
      , Group
            [ ( "Radio 1", Url "http://icecast.omroep.nl/radio1-bb-mp3" )
            , ( "Radio 2", Url "http://icecast.omroep.nl/radio2-bb-mp3" )
            , ( "Radio 3", Url "http://icecast.omroep.nl/radio3-bb-mp3" )
            , ( "Radio 4", Url "http://icecast.omroep.nl/radio4-bb-mp3" )
            , ( "Radio 5", Url "http://icecast.omroep.nl/radio5-bb-mp3" )
            , ( "Radio 2 Soul & Jazz", Url "http://icecast.omroep.nl/radio6-bb-mp3" )
            , ( "FunX"
              , Group
                    [ ( "FunX NL", Url "https://icecast.omroep.nl/funx-bb-mp3" )
                    , ( "FunX Amsterdam", Url "https://icecast.omroep.nl/funx-amsterdam-bb-mp3" )
                    , ( "FunX Rotterdam", Url "https://icecast.omroep.nl/funx-rotterdam-bb-mp3" )
                    , ( "FunX Utrecht", Url "https://icecast.omroep.nl/funx-utrecht-bb-mp3" )
                    , ( "FunX Den Haag", Url "https://icecast.omroep.nl/funx-denhaag-bb-mp3" )
                    , ( "FunX Latin", Url "https://icecast.omroep.nl/funx-latin-bb-mp3" )
                    , ( "FunX Dance", Url "https://icecast.omroep.nl/funx-dance-bb-mp3" )
                    , ( "FunX Slow Jamz", Url "https://icecast.omroep.nl/funx-slowjamz-bb-mp3" )
                    ]
              )
            ]
      )
    , ( "Slam FM", Url "http://stream.slam.nl/slamaac" )
    , ( "BNR Nieuwsradio", Url "http://icecast-bnr-cdp.triple-it.nl/bnr_mp3_96_04" )
    , ( "Sky Radio", Url "http://playerservices.streamtheworld.com/api/livestream-redirect/SKYRADIO.mp3" )
    , ( "Sublime"
      , Group
            [ ( "Sublime Radio", Url "http://stream.sublimefm.nl/SublimeFM_mp3" )
            , ( "Arrow Jazz", Url "http://17873.live.streamtheworld.com/SUBLIMEARROWJAZZ.mp3" )
            , ( "Pure Jazz", Url "http://20863.live.streamtheworld.com/SUBLIMEPUREJAZZ.mp3" )
            , ( "Soul", Url "http://20863.live.streamtheworld.com/SUBLIMESOUL.mp3" )
            , ( "Smooth", Url "http://20863.live.streamtheworld.com/SUBLIMESMOOTH.mp3" )
            , ( "Sublime 500", Url "http://20863.live.streamtheworld.com/SUBLIME500.mp3" )
            ]
      )
    , ( "Regio / Diversen"
      , Group
            [ ( "ORTS", Url "https://media.maxx-xs.nl/mediaplayer/sasx.php?serps=LY1rCgMhDITvkhNo1KxmT-NqBKEFaRZKKb17ld1$3wzzyBz4q2wNQ-sPOT9DeoVdGRloW-AYxmURgyebPC0RGHSlb-7lOdzixPCWQ$t578w6GrMozoYUldfQJf28zIGy2Y6QCImSKRhRovWuYmjN5eQPZ2uRAPvvDw" )
            , ( "Radio NH", Url "http://ice.cr1.streamzilla.xlcdn.com:8000/sz%3Dnhnieuws%3DNHRadio_mp3" )
            , ( "Radio NL Kids", Url "http://stream.radionlkids.nl/rnlkids" )
            , ( "Salto CaribbeanFM", Url "https://icecast.streamone.net/YoRFHeMSYJ42" )
            , ( "Salto Mokum Radio", Url "https://icecast.streamone.net/crJNGaeQKRQy" )
            , ( "Radio Salto", Url "https://icecast.streamone.net/46ANH44CYA8S" )
            , ( "Salto RAZO", Url "https://icecast.streamone.net/c6YFGcOSYMQS" )
            ]
      )
    , ( "Internationaal"
      , Group
            [ ( "Belgie"
              , Group
                    [ ( "Radio 1", Url "http://icecast.vrtcdn.be/radio1-high.mp3" )
                    , ( "Studio Brussel", Url "http://icecast.vrtcdn.be/stubru-high.mp3" )
                    , ( "VRT Ketnet Hits", Url "http://icecast.vrtcdn.be/ketnetradio-high.mp3" )
                    , ( "VRT Nieuws", Url "http://progressive-audio.lwc.vrtcdn.be/content/fixed/11_11niws-snip_hi.mp3" )
                    ]
              )
            , ( "Duitsland"
              , Group
                    [ ( "WDR 4", Url "http://wdr-wdr4-live.icecast.wdr.de/wdr/wdr4/live/mp3/128/stream.mp3" )
                    ]
              )
            , ( "Engeland"
              , Group
                    [ ( "BBC World Service", Url "http://bbcwssc.ic.llnwd.net/stream/bbcwssc_mp1_ws-einws" )
                    ]
              )
            , ( "Frankrijk"
              , Group
                    [ ( "France Musique", Url "http://direct.francemusique.fr/live/francemusique-midfi.mp3" )
                    , ( "FIP", Url "http://icecast.radiofrance.fr/fip-midfi.mp3" )
                    , ( "FIP Monde", Url "http://direct.fipradio.fr/live/fip-webradio4.mp3" )
                    ]
              )
            , ( "Noorwegen"
              , Group
                    [ ( "NRK Alltid Nyheter", Url "http://lyd.nrk.no/nrk_radio_alltid_nyheter_mp3_h" )
                    , ( "NRK Folkemusik", Url "http://lyd.nrk.no/nrk_radio_folkemusikk_mp3_h" )
                    ]
              )
            , ( "Spanje"
              , Group
                    [ ( "RTVE Radio Nacional", Url "https://rne.rtveradio.cires21.com/rne_hc.mp3" )
                    ]
              )
            ]
      )
    ]
