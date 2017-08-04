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
  mapM_ siteToSvg (sites mapToRender)

siteToSvg :: Site -> S.Svg
siteToSvg site =
  S.circle ! A.cx (S.toValue $ x site) ! A.cy (S.toValue $ y site) ! A.r "0.1" !
  A.fill "#008d46"

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
minX m = minimum (Set.map x $ sites m)

maxX :: Map -> Double
maxX m = maximum (Set.map x $ sites m)

minY :: Map -> Double
minY m = minimum (Set.map y $ sites m)

maxY :: Map -> Double
maxY m = maximum (Set.map y $ sites m)

padding :: Double
padding = 10.0

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
  , mines = Set.empty
  }
