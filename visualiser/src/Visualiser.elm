module Visualiser exposing (..)

import Html exposing (..)
import Html.Events exposing (..)
import Json.Decode


-- MODEL


type alias Model =
    {}


type alias GameState =
    { initialState : State
    , prevMoves : List Move
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
    , x : Int
    , y : Int
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


init : Flags -> ( Model, Cmd Msg )
init flags =
    {} ! []



-- UPDATE


type Msg
    = LoadStateDump


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LoadStateDump ->
            model ! []



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ button [ onClick LoadStateDump ] [ text "Load state dump" ]
        ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
