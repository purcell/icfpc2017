module GraphHelpers exposing (..)

import Types exposing (..)


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


normaliseCoords : Map -> Map
normaliseCoords originalMap =
    let
        originalSites =
            originalMap.sites

        numberOfSites =
            toFloat (List.length originalSites)

        xRange =
            maxX originalSites - minX originalSites

        yRange =
            maxY originalSites - minY originalSites

        scaleSite site =
            { site | x = (site.x / xRange), y = (site.y / yRange) }

        scaleMap map =
            { map | sites = List.map scaleSite originalSites }
    in
        scaleMap originalMap
