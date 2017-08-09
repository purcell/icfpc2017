module Visualiser exposing (..)

import Html exposing (..)
import Html.Attributes exposing (style, value)
import Html.Events exposing (onClick, onInput)
import Json.Decode as Json
import Set
import Svg as S
import Svg.Attributes as A
import Time exposing (Time, millisecond)
import Decoders exposing (..)
import Types exposing (..)
import Exts.Maybe exposing (oneOf)
import GraphHelpers exposing (..)


-- MODEL


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        state =
            flags.initialState
    in
        case Json.decodeValue decodeMoves flags.prevMoves of
            Ok moves ->
                { moves = moves
                , state = { state | map = normaliseCoords state.map }
                , time = 0
                , animate = False
                , movesToShow = List.length moves
                , animationSpeed = 100
                }
                    ! []

            Err err ->
                Debug.crash err


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
    = ToggleAnimation
    | Tick Time
    | ChangeAnimationSpeed String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ToggleAnimation ->
            let
                newNumberOfMovesToShow =
                    if model.animate then
                        List.length model.moves
                    else
                        0
            in
                { model
                    | animate = not model.animate
                    , movesToShow = newNumberOfMovesToShow
                }
                    ! []

        Tick newTime ->
            let
                newNumberOfMovesToShow =
                    if model.animate then
                        model.movesToShow + 1
                    else
                        model.movesToShow
            in
                if newNumberOfMovesToShow > List.length model.moves then
                    toggleAnimation model ! []
                else
                    { model
                        | time = newTime
                        , movesToShow = newNumberOfMovesToShow
                    }
                        ! []

        ChangeAnimationSpeed newSpeed ->
            case String.toFloat newSpeed of
                Ok speed ->
                    { model | animationSpeed = speed } ! []

                Err _ ->
                    model ! []


toggleAnimation : Model -> Model
toggleAnimation model =
    { model | animate = not model.animate }



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ animateButton model
        , animationSpeedInput model
        , p [] [ text ("Moves played: " ++ toString model.movesToShow) ]
        , (S.svg
            [ A.version "1.1"
            , A.viewBox "-1 -1 2 2"
            ]
            [ viewRivers model
            , viewSites model
            ]
          )
        , viewLegend model
        , viewDebugInfo model
        ]


animateButton : Model -> Html Msg
animateButton model =
    let
        description =
            if model.animate then
                "Stop Animation"
            else
                "Start Animation"
    in
        button [ onClick ToggleAnimation ] [ text description ]


animationSpeedInput : Model -> Html Msg
animationSpeedInput model =
    input [ onInput ChangeAnimationSpeed, value (toString model.animationSpeed) ] []


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
            [ p [] [ text ("Number of punters: " ++ toString (model.state.punters)) ]
            , p [] [ text ("Number of rivers: " ++ toString (List.length rivers)) ]
            , p [] [ text ("Number of sites: " ++ toString (List.length sites)) ]
            , p [] [ text ("Number of mines: " ++ toString (List.length mines)) ]
            , p [] [ text ("Number of claims: " ++ toString (List.length claims)) ]
            , debugMoves model
            ]


debugMoves : Model -> Html Msg
debugMoves model =
    div []
        (List.map debugMove model.moves)


debugMove : Move -> Html Msg
debugMove move =
    p [] [ text (toString move) ]


viewLegend : Model -> Html Msg
viewLegend model =
    div []
        (List.map viewPunterLegend (puntersWhoMoved model))


viewPunterLegend : Int -> Html Msg
viewPunterLegend punter =
    h3 [ style [ ( "color", colourForPunter punter ) ] ]
        [ text ("Punter " ++ toString punter) ]


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

        riversAlreadyAnimated =
            List.take model.movesToShow rivers

        riversYetToBeAnimated =
            List.drop model.movesToShow rivers
    in
        S.svg
            [ A.x "-1", A.y "-1", A.overflow "visible" ]
            (List.map (viewRiver model riversAlreadyAnimated) rivers)


viewRiver : Model -> List River -> River -> S.Svg Msg
viewRiver model animatedRivers river =
    let
        riverEnd =
            endOfRiver model.state.map.sites river

        ( colour, width ) =
            case claimForRiver model river of
                Nothing ->
                    ( "grey", "0.001" )

                Just claim ->
                    if List.member river animatedRivers then
                        ( colourForPunter claim.punter, "0.003" )
                    else
                        ( "grey", "0.001" )
    in
        S.line
            [ riverEnd .source .x A.x1
            , riverEnd .source .y A.y1
            , riverEnd .target .x A.x2
            , riverEnd .target .y A.y2
            , A.strokeWidth width
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
            [ A.x "-1", A.y "-1", A.overflow "visible" ]
            (List.map (viewSite model sites) sites)


viewSite : Model -> List Site -> Site -> S.Svg Msg
viewSite model sites site =
    S.circle
        [ A.cx (toString site.x)
        , A.cy (toString site.y)
        , A.r (ifMineElse model site "0.004" "0.002")
        , A.fill (ifMineElse model site "red" "grey")
        ]
        []


ifMineElse : Model -> Site -> a -> a -> a
ifMineElse model site a b =
    if List.member site.id model.state.map.mines then
        a
    else
        b


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
    Time.every (model.animationSpeed * millisecond) Tick
