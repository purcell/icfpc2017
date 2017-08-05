{-# LANGUAGE OverloadedStrings #-}

module Visualiser where

import Data.Maybe (listToMaybe)
import Data.Monoid ((<>))
import qualified Data.Set as Set
import Text.Blaze.Svg.Renderer.String (renderSvg)
import Text.Blaze.Svg11 ((!))
import qualified Text.Blaze.Svg11 as S
import qualified Text.Blaze.Svg11.Attributes as A
import Types

testRender :: IO ()
testRender = do
  let svgStr = renderStateToString sampleMap sampleClaims
  writeFile "visualiser-output.svg" svgStr

renderStateToString :: Map -> [Claim] -> String
renderStateToString m claims = renderSvg $ mapToSvg m claims

mapToSvg :: Map -> [Claim] -> S.Svg
mapToSvg m claims =
  S.docTypeSvg ! A.version "1.1" ! A.width "500" ! A.height "500" !
  A.viewbox (viewboxAttrs m) $ do
    mapM_ (riverToSvg claims $ sites m) $ rivers m
    mapM_ (siteToSvg $ mines m) $ sites m

siteToSvg :: Set.Set SiteID -> Site -> S.Svg
siteToSvg mineIDs site =
  S.circle ! A.cx (S.toValue $ x site) ! A.cy (S.toValue $ y site) ! A.r "0.1" !
  A.fill (colourForSite mineIDs site)

riverToSvg :: [Claim] -> Set.Set Site -> River -> S.Svg
riverToSvg claims sites' r =
  S.line ! A.x1 (S.toValue (x $ endOfRiver source sites' r)) !
  A.y1 (S.toValue (y $ endOfRiver source sites' r)) !
  A.x2 (S.toValue (x $ endOfRiver target sites' r)) !
  A.y2 (S.toValue (y $ endOfRiver target sites' r)) !
  A.strokeWidth (widthForRiver claims r) !
  A.stroke (colourForRiver claims r)

endOfRiver :: (River -> SiteID) -> Set.Set Site -> River -> Site
endOfRiver f sites' r =
  if Set.null matchingSites
    then error "somehow a river had an end that isn't in the sites!"
    else Set.findMin matchingSites
  where
    matchingSites = Set.filter (\e -> Types.id e == f r) sites'

widthForRiver :: [Claim] -> River -> S.AttributeValue
widthForRiver claims r =
  if r `elem` map river claims
    then claimedRiverWidth
    else unclaimedRiverWidth

colourForRiver :: [Claim] -> River -> S.AttributeValue
colourForRiver claims r =
  case maybeClaim of
    Nothing -> unclaimedRiverColour
    (Just claim) -> colourForPunter $ punter claim
  where
    maybeClaim = listToMaybe $ filter (\claim -> river claim == r) claims

colourForPunter :: PunterID -> S.AttributeValue
colourForPunter punterId =
  case punterId of
    0 -> firstPunterColour
    1 -> secondPunterColour
    2 -> thirdPunterColour
    _ -> fourthPunterColour

colourForSite :: Set.Set SiteID -> Site -> S.AttributeValue
colourForSite mineIDs site =
  if Set.member (Types.id site) mineIDs
    then mineColour
    else siteColour

mineColour :: S.AttributeValue
mineColour = "#DBAFC1"

siteColour :: S.AttributeValue
siteColour = "#B8BDB5"

viewboxAttrs :: Map -> S.AttributeValue
viewboxAttrs m =
  S.toValue (minX m - padding) <> " " <> S.toValue (minY m - padding) <> " " <>
  S.toValue (width m + (padding * 2)) <>
  " " <>
  S.toValue (height m + (padding * 2))

width :: Map -> Double
width m = maxX m - minX m

height :: Map -> Double
height m = maxY m - minY m

minX :: Map -> Double
minX m = minimum $ xs m

maxX :: Map -> Double
maxX m = maximum $ xs m

minY :: Map -> Double
minY m = minimum $ ys m

maxY :: Map -> Double
maxY m = maximum $ ys m

xs :: Map -> Set.Set Double
xs m = Set.map x $ sites m

ys :: Map -> Set.Set Double
ys m = Set.map y $ sites m

padding :: Double
padding = 1.0

unclaimedRiverWidth :: S.AttributeValue
unclaimedRiverWidth = "0.03"

claimedRiverWidth :: S.AttributeValue
claimedRiverWidth = "0.04"

unclaimedRiverColour :: S.AttributeValue
unclaimedRiverColour = "#E2E4F6"

firstPunterColour :: S.AttributeValue
firstPunterColour = "#7DDF64"

secondPunterColour :: S.AttributeValue
secondPunterColour = "#8D86C9"

thirdPunterColour :: S.AttributeValue
thirdPunterColour = "#EAD637"

fourthPunterColour :: S.AttributeValue
fourthPunterColour = "#DB5461"

-- SAMPLE DATA:
sampleMap :: Map
sampleMap =
  Map
  { sites =
      Set.fromList
        [ Site {Types.id = 0, x = 0.0, y = 0.0}
        , Site {Types.id = 1, x = 1.0, y = 0.0}
        , Site {Types.id = 2, x = 2.0, y = 0.0}
        , Site {Types.id = 3, x = 2.0, y = -1.0}
        , Site {Types.id = 4, x = 2.0, y = -2.0}
        , Site {Types.id = 5, x = 1.0, y = -2.0}
        , Site {Types.id = 6, x = 0.0, y = -2.0}
        , Site {Types.id = 7, x = 0.0, y = -1.0}
        ]
  , rivers =
      Set.fromList
        [ River {source = 0, target = 1}
        , River {source = 1, target = 2}
        , River {source = 0, target = 7}
        , River {source = 7, target = 6}
        , River {source = 6, target = 5}
        , River {source = 5, target = 4}
        , River {source = 4, target = 3}
        , River {source = 3, target = 2}
        , River {source = 1, target = 7}
        , River {source = 1, target = 3}
        , River {source = 7, target = 5}
        , River {source = 5, target = 3}
        ]
  , mines = Set.fromList [1, 5]
  }

sampleClaims :: [Claim]
sampleClaims =
  [ Claim {punter = 0, river = River {source = 7, target = 5}}
  , Claim {punter = 0, river = River {source = 3, target = 2}}
  , Claim {punter = 1, river = River {source = 1, target = 2}}
  , Claim {punter = 0, river = River {source = 5, target = 3}}
  , Claim {punter = 1, river = River {source = 1, target = 3}}
  , Claim {punter = 0, river = River {source = 0, target = 7}}
  , Claim {punter = 1, river = River {source = 1, target = 7}}
  , Claim {punter = 0, river = River {source = 4, target = 3}}
  , Claim {punter = 1, river = River {source = 0, target = 1}}
  , Claim {punter = 0, river = River {source = 5, target = 4}}
  , Claim {punter = 1, river = River {source = 7, target = 6}}
  ]
