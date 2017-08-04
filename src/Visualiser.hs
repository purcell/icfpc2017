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

mapToSvg :: Map -> S.Svg
mapToSvg mapToRender =
  S.docTypeSvg ! A.version "1.1" ! A.width "500" ! A.height "500" !
  A.viewbox (viewboxAttrs mapToRender) $
  mapM_ (siteToSvg $ mines mapToRender) $ sites mapToRender

siteToSvg :: Set.Set SiteID -> Site -> S.Svg
siteToSvg mineIDs site =
  S.circle ! A.cx (S.toValue $ x site) ! A.cy (S.toValue $ y site) ! A.r "0.1" !
  A.fill (colourForSite mineIDs site)

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
padding = 10.0

colourForSite :: Set.Set SiteID -> Site -> S.AttributeValue
colourForSite mineIDs site =
  if Set.member (Types.id site) mineIDs
    then mineColour
    else siteColour

mineColour :: S.AttributeValue
mineColour = "#DBAFC1"

siteColour :: S.AttributeValue
siteColour = "#B8BDB5"

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
  , rivers = Set.empty
  , mines = Set.fromList [1, 5]
  }
