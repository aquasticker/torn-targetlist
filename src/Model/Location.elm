module Model.Location exposing (Location(..))

import Model.Country exposing (Country)


type Location
    = LocationTorn
    | LocationAbroad Country
