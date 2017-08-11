module Visualiser exposing (..)

import Html exposing (..)
import Html.Attributes exposing (style, value, type_, min, max)
import Html.Events exposing (onClick, onInput)
import Json.Decode as Json
import Set
import Svg as S
import Svg.Attributes as A
import Time exposing (Time, second)
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
                , movesToShow = 0
                , animationSpeed = 50
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


selectClaim : Move -> Maybe ClaimMove
selectClaim move =
    case move of
        Pass _ ->
            Nothing

        Claim claim ->
            Just claim



-- UPDATE


type Msg
    = ToggleAnimation
    | Tick Time
    | UpdateAnimationSpeed String
    | StepForward
    | StepBack


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ToggleAnimation ->
            { model | animate = not model.animate } ! []

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

        UpdateAnimationSpeed newSpeed ->
            case String.toFloat newSpeed of
                Ok speed ->
                    { model | animationSpeed = speed } ! []

                Err _ ->
                    model ! []

        StepForward ->
            let
                newNumberOfMovesToShow =
                    if model.movesToShow == List.length model.moves then
                        0
                    else
                        model.movesToShow + 1
            in
                { model | movesToShow = newNumberOfMovesToShow } ! []

        StepBack ->
            let
                newNumberOfMovesToShow =
                    if model.movesToShow == 0 then
                        List.length model.moves
                    else
                        model.movesToShow - 1
            in
                if newNumberOfMovesToShow > List.length model.moves then
                    model ! []
                else
                    { model | movesToShow = newNumberOfMovesToShow } ! []


toggleAnimation : Model -> Model
toggleAnimation model =
    { model | animate = not model.animate }



-- VIEW


view : Model -> Html Msg
view model =
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
        div []
            [ viewLegend model
            , viewDebugInfo model
            , stepFowardButton model
            , stepBackButton model
            , animateButton model
            , animationSpeedSlider model
            , p [] [ text ("Moves played: " ++ toString model.movesToShow) ]
            , (S.svg
                [ A.version "1.1"
                , A.viewBox "-1 -1 2 2"
                ]
                [ viewRivers model
                , viewSites model
                ]
              )
            ]


stepFowardButton : Model -> Html Msg
stepFowardButton model =
    button [ onClick StepForward ] [ text "Step forward" ]


stepBackButton : Model -> Html Msg
stepBackButton model =
    button [ onClick StepBack ] [ text "Step back" ]


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


animationSpeedSlider : Model -> Html Msg
animationSpeedSlider model =
    div []
        [ input
            [ type_ "range"
            , Html.Attributes.min "0"
            , Html.Attributes.max "100"
            , value (toString model.animationSpeed)
            , onInput UpdateAnimationSpeed
            ]
            []
        , text ("Move every " ++ (toString (model.animationSpeed * 0.01)) ++ " seconds")
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
            [ p [] [ text ("Number of punters: " ++ toString (model.state.punters)) ]
            , p [] [ text ("Number of rivers: " ++ toString (List.length rivers)) ]
            , p [] [ text ("Number of sites: " ++ toString (List.length sites)) ]
            , p [] [ text ("Number of mines: " ++ toString (List.length mines)) ]
            , p [] [ text ("Number of claims: " ++ toString (List.length claims)) ]
            ]


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

        animatedMoves =
            List.take model.movesToShow model.moves

        animatedClaims =
            List.filterMap selectClaim animatedMoves

        animatedRivers =
            List.filter (riverIsAnimated animatedClaims) rivers
    in
        S.svg
            [ A.x "-1", A.y "-1", A.overflow "visible" ]
            (List.map (viewRiver model animatedRivers) rivers)


riverIsAnimated : List ClaimMove -> River -> Bool
riverIsAnimated animatedClaims river =
    let
        riverMatchesAnimatedClaim claim =
            river.source == claim.source && river.target == claim.target
    in
        case
            List.head
                (List.filter riverMatchesAnimatedClaim animatedClaims)
        of
            Nothing ->
                False

            _ ->
                True


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
    Time.every ((model.animationSpeed * 0.01) * second) Tick
