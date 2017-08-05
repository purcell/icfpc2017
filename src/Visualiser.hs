{-# LANGUAGE OverloadedStrings #-}

module Visualiser where

import Data.Monoid ((<>))
import qualified Data.Set as Set
import Text.Blaze.Svg.Renderer.String (renderSvg)
import Text.Blaze.Svg11 ((!))
import qualified Text.Blaze.Svg11 as S
import qualified Text.Blaze.Svg11.Attributes as A
import Types

render :: IO ()
render = do
  let a = renderSvg $ mapToSvg sampleMap
  writeFile "visualiser-output.svg" a

mapToStr :: Map -> String
mapToStr m = renderSvg $ mapToSvg m

mapToSvg :: Map -> S.Svg
mapToSvg m =
  S.docTypeSvg ! A.version "1.1" ! A.width "500" ! A.height "500" !
  A.viewbox (viewboxAttrs m) $ do
    mapM_ (riverToSvg $ sites m) $ rivers m
    mapM_ (siteToSvg $ mines m) $ sites m

siteToSvg :: Set.Set SiteID -> Site -> S.Svg
siteToSvg mineIDs site =
  S.circle ! A.cx (S.toValue $ x site) ! A.cy (S.toValue $ y site) ! A.r "0.1" !
  A.fill (colourForSite mineIDs site)

riverToSvg :: Set.Set Site -> River -> S.Svg
riverToSvg allSites r =
  S.line ! A.x1 (S.toValue (x $ endOfRiver source allSites r)) !
  A.y1 (S.toValue (y $ endOfRiver source allSites r)) !
  A.x2 (S.toValue (x $ endOfRiver target allSites r)) !
  A.y2 (S.toValue (y $ endOfRiver target allSites r)) !
  A.strokeWidth "0.01" !
  A.stroke "#E2E4F6"

endOfRiver :: (River -> SiteID) -> Set.Set Site -> River -> Site
endOfRiver f allSites r =
  if Set.null matchingSites
    then error "somehow a river had an end that isn't in the sites!"
    else Set.findMin matchingSites
  where
    matchingSites = Set.filter (\e -> Types.id e == f r) allSites

targetOfRiver :: River -> Site
targetOfRiver = undefined

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
