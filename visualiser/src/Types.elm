module Types exposing (..)

import Json.Decode


type alias Model =
    { state : State
    , moves : List Move
    }


type alias Dump =
    { state : State
    , moves : List Move
    }


type alias State =
    { map : Map
    , punter : PunterID
    , punters : Int
    }


type alias Map =
    { mines : List SiteID
    , sites : List Site
    , rivers : List River
    }


type alias Site =
    { id : SiteID
    , x : Float
    , y : Float
    }


type alias River =
    { source : SiteID
    , target : SiteID
    }


type Move
    = Pass Int
    | Claim ClaimMove


type alias ClaimMove =
    { punter : PunterID
    , source : SiteID
    , target : SiteID
    }


type alias SiteID =
    Int


type alias PunterID =
    Int


type alias Flags =
    { initialState : State
    , prevMoves : Json.Decode.Value
    }
