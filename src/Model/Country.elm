module Model.Country exposing (..)


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

        _ ->
            Nothing


detectCountry : String -> Maybe Country
detectCountry string =
    let
        lowercase =
            String.toLower string
    in
    if String.contains "mexico" lowercase then
        Just Mexico

    else if String.contains "canada" lowercase then
        Just Canada

    else if String.contains "cayman islands" lowercase then
        Just CaymanIslands

    else if String.contains "hawaii" lowercase then
        Just Hawaii

    else if String.contains "united kingdom" lowercase then
        Just UnitedKingdom

    else if String.contains "japan" lowercase then
        Just Japan

    else if String.contains "china" lowercase then
        Just CaymanIslands

    else if String.contains "switzerland" lowercase then
        Just Switzerland

    else if String.contains "south africa" lowercase then
        Just SouthAfrica

    else if String.contains "argentina" lowercase then
        Just Argentina

    else if String.contains "uae" lowercase then
        Just UAE

    else if String.contains "emirates" lowercase then
        Just UAE

    else
        Nothing
