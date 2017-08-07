module Visualiser exposing (..)

import Html exposing (..)
import Html.Attributes exposing (style)
import Json.Decode as Json
import Set
import Svg as S
import Svg.Attributes as A
import Decoders exposing (..)
import Types exposing (..)
import Exts.Maybe exposing (oneOf)


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


claimForRiver : Model -> River -> Maybe ClaimMove
claimForRiver model river =
    let
        claims =
            selectClaims model.moves

        isForRiver claim =
            case claim of
                Nothing ->
                    False

                Just c ->
                    c.source == river.source && c.target == river.target
    in
        oneOf (List.filter isForRiver claims)


selectClaims : List Move -> List (Maybe ClaimMove)
selectClaims moves =
    let
        extractClaim claim =
            case claim of
                Pass _ ->
                    Nothing

                Claim claim ->
                    Just claim
    in
        List.map extractClaim moves



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
            ]
          )
        , viewLegend model
        , viewDebugInfo model
        ]


viewDebugInfo : Model -> Html Msg
viewDebugInfo model =
    let
        sites =
            model.state.map.sites

        rivers =
            model.state.map.rivers

        mines =
            model.state.map.mines

        claims =
            selectClaims model.moves
    in
        div []
            [ p [] [ text ("Min x: " ++ toString (minCoord .x sites)) ]
            , p [] [ text ("Max x: " ++ toString (maxCoord .x sites)) ]
            , p [] [ text ("Min y: " ++ toString (minCoord .y sites)) ]
            , p [] [ text ("Max y: " ++ toString (maxCoord .y sites)) ]
            , p [] [ text ("Number of punters: " ++ toString (model.state.punters)) ]
            , p [] [ text ("Number of rivers: " ++ toString (List.length rivers)) ]
            , p [] [ text ("Number of sites: " ++ toString (List.length sites)) ]
            , p [] [ text ("Number of mines: " ++ toString (List.length mines)) ]
            , p [] [ text ("Number of claims: " ++ toString (List.length claims)) ]
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
    div []
        (List.map viewPunterLegend (puntersWhoMoved model))


viewPunterLegend : Int -> Html Msg
viewPunterLegend punter =
    h3 [ style [ ( "color", colourForPunter punter ) ] ] [ text (toString punter) ]


puntersWhoMoved : Model -> List Int
puntersWhoMoved model =
    let
        punterId move =
            case move of
                Pass id ->
                    id

                Claim claim ->
                    claim.punter
    in
        Set.toList (Set.fromList (List.map punterId model.moves))


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
            (List.map (viewRiver model) rivers)


viewRiver : Model -> River -> S.Svg Msg
viewRiver model river =
    let
        riverEnd =
            endOfRiver model.state.map.sites river

        claim =
            claimForRiver model river

        colour =
            case claim of
                Nothing ->
                    "grey"

                Just claim ->
                    colourForPunter claim.punter
    in
        S.line
            [ riverEnd .source .x A.x1
            , riverEnd .source .y A.y1
            , riverEnd .target .x A.x2
            , riverEnd .target .y A.y2
            , A.strokeWidth "0.001"
            , A.stroke colour
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
        , A.r (radiusForSite model site)
        , A.fill (colourForSite model site)
          -- "red"
        ]
        []


radiusForSite : Model -> Site -> String
radiusForSite model site =
    if List.member site.id model.state.map.mines then
        "0.004"
    else
        "0.002"


colourForSite : Model -> Site -> String
colourForSite model site =
    if List.member site.id model.state.map.mines then
        "red"
    else
        "grey"


colourForPunter : PunterID -> String
colourForPunter punter =
    case punter of
        0 ->
            "green"

        1 ->
            "purple"

        2 ->
            "lightBlue"

        3 ->
            "red"

        4 ->
            "darkGreen"

        5 ->
            "blue"

        6 ->
            "pink"

        7 ->
            "darkRed"

        _ ->
            "black"



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
