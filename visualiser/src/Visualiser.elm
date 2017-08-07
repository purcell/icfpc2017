module Visualiser exposing (..)

import Html exposing (..)
import Json.Decode as Json
import Svg as S
import Svg.Attributes as A
import Decoders exposing (..)
import Types exposing (..)


-- MODEL


init : Flags -> ( Model, Cmd Msg )
init flags =
    case Json.decodeValue decodeMoves flags.prevMoves of
        Ok moves ->
            { moves = moves
            , state = (normaliseCoordinates flags.initialState)
            }
                ! []

        Err err ->
            Debug.crash err


normaliseCoordinates : State -> State
normaliseCoordinates originalState =
    let
        originalSites =
            originalState.map.sites

        numberOfSites =
            toFloat (List.length originalSites)

        xRange =
            maxX originalSites - minX originalSites

        yRange =
            maxY originalSites - minY originalSites

        scaleMap map =
            { map | sites = List.map scaleSite originalSites }

        scaleSite site =
            { site | x = (site.x / xRange), y = (site.y / yRange) }
    in
        { originalState | map = scaleMap originalState.map }



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
        [ (S.svg
            [ A.version "1.1"
            , A.width "1000"
            , A.height "1000"
            , A.viewBox "-1 -1 2 2"
            ]
            [ viewRivers model
            , viewSites model
            , viewLegend model
            ]
          )
        , viewMapData model
        ]


viewMapData : Model -> Html Msg
viewMapData model =
    let
        sites =
            model.state.map.sites

        rivers =
            model.state.map.rivers

        mines =
            model.state.map.mines
    in
        div []
            [ p [] [ text ("Min x: " ++ toString (minCoord .x sites)) ]
            , p [] [ text ("Min y: " ++ toString (minCoord .y sites)) ]
            , p [] [ text ("Max x: " ++ toString (maxCoord .x sites)) ]
            , p [] [ text ("Max y: " ++ toString (maxCoord .y sites)) ]
            , p [] [ text ("Number of rivers: " ++ toString (List.length rivers)) ]
            , p [] [ text ("Number of sites: " ++ toString (List.length sites)) ]
            , p [] [ text ("Number of mines: " ++ toString (List.length mines)) ]
            ]


minCoord : (Site -> Float) -> List Site -> Float
minCoord coord sites =
    coordExtremity List.minimum coord sites


maxCoord : (Site -> Float) -> List Site -> Float
maxCoord coord sites =
    coordExtremity List.maximum coord sites


coordExtremity :
    (List Float -> Maybe Float)
    -> (Site -> Float)
    -> List Site
    -> Float
coordExtremity extremity coord sites =
    Maybe.withDefault 0.0 (extremity (List.map coord sites))


viewboxWidth : Model -> Float
viewboxWidth model =
    let
        sites =
            model.state.map.sites
    in
        (maxX sites) - (minX sites)


viewboxHeight : Model -> Float
viewboxHeight model =
    let
        sites =
            model.state.map.sites
    in
        (maxY sites) - (minY sites)


minX : List Site -> Float
minX =
    minCoord .x


minY : List Site -> Float
minY =
    minCoord .y


maxX : List Site -> Float
maxX =
    maxCoord .x


maxY : List Site -> Float
maxY =
    maxCoord .y


viewLegend : Model -> S.Svg Msg
viewLegend model =
    S.svg [] []


viewRivers : Model -> S.Svg Msg
viewRivers model =
    let
        map =
            model.state.map

        rivers =
            map.rivers

        sites =
            map.sites
    in
        S.svg
            [ A.overflow "visible", A.x "0", A.y "0" ]
            (List.map (viewRiver sites) rivers)


viewRiver : List Site -> River -> S.Svg Msg
viewRiver sites river =
    let
        riverEnd =
            endOfRiver sites river
    in
        S.line
            [ riverEnd .source .x A.x1
            , riverEnd .source .y A.y1
            , riverEnd .target .x A.x2
            , riverEnd .target .y A.y2
            , A.strokeWidth "0.001"
            , A.stroke "grey"
            ]
            []


endOfRiver :
    List Site
    -> River
    -> (River -> SiteID)
    -> (Site -> Float)
    -> (String -> S.Attribute Msg)
    -> S.Attribute Msg
endOfRiver sites river end xOrY attr =
    let
        sourceOrTarget =
            List.head
                (List.filter (\s -> s.id == (end river)) sites)
    in
        case sourceOrTarget of
            Nothing ->
                attr "0"

            Just site ->
                attr (toString (xOrY site))


viewSites : Model -> S.Svg Msg
viewSites model =
    let
        sites =
            model.state.map.sites
    in
        S.svg
            [ A.overflow "visible", A.x "0", A.y "0" ]
            (List.map (viewSite model sites) sites)


viewSite : Model -> List Site -> Site -> S.Svg Msg
viewSite model sites site =
    S.circle
        [ A.cx (toString site.x)
        , A.cy (toString site.y)
        , A.r "0.002"
        , A.fill "red"
        ]
        []



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
