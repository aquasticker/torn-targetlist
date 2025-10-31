module Model.Country exposing (..)

import List.Extra


type Country
    = Mexico
    | Canada
    | CaymanIslands
    | Hawaii
    | UnitedKingdom
    | Switzerland
    | Argentina
    | UAE
    | SouthAfrica
    | Japan
    | China


countryCode : Country -> String
countryCode country =
    case country of
        Mexico ->
            "MX"

        Canada ->
            "CA"

        CaymanIslands ->
            "CI"

        Hawaii ->
            "HI"

        UnitedKingdom ->
            "UK"

        Switzerland ->
            "CH"

        Argentina ->
            "AR"

        UAE ->
            "AE"

        SouthAfrica ->
            "SA"

        Japan ->
            "JP"

        China ->
            "CN"


countryName : Country -> String
countryName country =
    case country of
        Mexico ->
            "Mexico"

        Canada ->
            "Canada"

        CaymanIslands ->
            "Cayman Islands"

        Hawaii ->
            "Hawaii"

        UnitedKingdom ->
            "United Kingdom"

        Switzerland ->
            "Switzerland"

        Argentina ->
            "Argentina"

        UAE ->
            "UAD"

        SouthAfrica ->
            "South Africa"

        Japan ->
            "Japan"

        China ->
            "China"


countryFromCode : String -> Maybe Country
countryFromCode code =
    case code of
        "MX" ->
            Just Mexico

        "CA" ->
            Just Canada

        "CI" ->
            Just CaymanIslands

        "HI" ->
            Just Hawaii

        "UK" ->
            Just UnitedKingdom

        "CH" ->
            Just Switzerland

        "AR" ->
            Just Argentina

        "AE" ->
            Just UAE

        "SA" ->
            Just SouthAfrica

        "JP" ->
            Just Japan

        "CN" ->
            Just China

        _ ->
            Nothing


detectCountry : String -> Maybe Country
detectCountry string =
    let
        haystack =
            String.toLower string

        contains : ( String, Country ) -> Bool
        contains tuple =
            let
                needle =
                    Tuple.first tuple
            in
            String.contains needle haystack

        mappings =
            [ ( "mexico", Mexico )
            , ( "mexican", Mexico )
            , ( "canada", Canada )
            , ( "canadian", Canada )
            , ( "cayman", CaymanIslands )
            , ( "hawaii", Hawaii )
            , ( "united kingdom", UnitedKingdom )
            , ( "british", UnitedKingdom )
            , ( "japan", Japan )
            , ( "japanese", Japan )
            , ( "china", China )
            , ( "chinese", China )
            , ( "switzerland", Switzerland )
            , ( "swiss", Switzerland )
            , ( "south africa", SouthAfrica )
            , ( "argentina", Argentina )
            , ( "argentinian", Argentina )
            , ( "uae", UAE )
            , ( "emirates", UAE )
            , ( "emirati", UAE )
            ]
    in
    mappings
        |> List.Extra.find contains
        |> Maybe.map Tuple.second
