{-# LANGUAGE OverloadedStrings #-}

module Visualiser where

import Data.Foldable (find)
import Data.List as List
import Data.Monoid ((<>))
import qualified Data.Set as Set
import GamePlay
import Text.Blaze.Svg.Renderer.String (renderSvg)
import Text.Blaze.Svg11 ((!))
import qualified Text.Blaze.Svg11 as S
import qualified Text.Blaze.Svg11.Attributes as A
import Types

writeStateToSvgFiles :: GameState -> IO Int
writeStateToSvgFiles gameState = do
  let svgs = renderMovesFromState gameState
  mapM_ writeMoveSvgToFile svgs
  return $ length svgs

writeMoveSvgToFile :: (Int, String) -> IO ()
writeMoveSvgToFile (moveCount, svg) =
  writeFile ("visualisations/move-" <> show moveCount <> ".svg") svg

renderMovesFromState :: GameState -> [(Int, String)]
renderMovesFromState state = List.map (renderMovesOnMap state) (moveInits state)

renderMovesOnMap :: GameState -> [Move] -> (Int, String)
renderMovesOnMap state moves =
  let claims' = GamePlay.claimsInMoves moves
      svgStr = renderSvg $ mapToSvg state claims'
  in (length moves, svgStr)

mapToSvg :: GameState -> Set.Set Claim -> S.Svg
mapToSvg state claims' =
  S.docTypeSvg ! A.version "1.1" ! A.width "500" ! A.height "500" !
  A.viewbox viewboxAttrs $ do
    renderLegendFromState w y' state
    mapM_ (riverToSvg w claims' sites') rivers'
    mapM_ (siteToSvg w mines') sites'
  where
    map' = GamePlay.map $ initialState state
    sites' = sites map'
    mines' = mines map'
    rivers' = rivers map'
    x' = viewboxX map'
    y' = viewboxY map'
    w = viewboxW map'
    h = viewboxH map'
    viewboxAttrs = foldr (\d m -> S.toValue d <> " " <> m) "" [x', y', w, h]

renderLegendFromState :: Double -> Double -> GameState -> S.Svg
renderLegendFromState viewboxWidth minY' state =
  mapM_ (punterToSvg viewboxWidth minY' $ myPunterID state) $
  puntersInGame state

punterToSvg :: Double -> Double -> PunterID -> PunterID -> S.Svg
punterToSvg viewboxWidth minY' ourPunter p =
  S.text_ (S.toSvg punterDescription) ! fontSize ! fontColour ! textAnchor !
  translation
  where
    fontSize = A.fontSize $ S.toValue (viewboxWidth * 0.05)
    fontColour = A.fill (colourForPunter p)
    textAnchor = A.textAnchor "start"
    translation = A.transform $ S.toValue $ "translate(0 " <> yTrans <> ")"
    yTrans = show (minY' + (fromIntegral p / viewboxWidth))
    punterDescription =
      if p == ourPunter
        then show p <> " (us)"
        else show p

punterYOffset :: PunterID -> Double
punterYOffset p = (0.22 * fromIntegral p) :: Double

siteCoords :: Site -> (Double, Double)
siteCoords (Site _ (Just l)) = l
siteCoords (Site _ Nothing) = (0, 0)

x :: Site -> Double
x = fst . siteCoords

y :: Site -> Double
y = snd . siteCoords

siteToSvg :: Double -> Set.Set SiteID -> Site -> S.Svg
siteToSvg viewboxWidth mineIDs site = S.circle ! cx ! cy ! r ! colour
  where
    cx = A.cx (S.toValue $ x site)
    cy = A.cy (S.toValue $ y site)
    r = A.r $ S.toValue (viewboxWidth * 0.01)
    colour = A.fill (colourForSite mineIDs site)

riverToSvg :: Double -> Set.Set Claim -> Set.Set Site -> River -> S.Svg
riverToSvg viewboxWidth claims' sites' r =
  S.line ! A.x1 (S.toValue (x $ endOfRiver source sites' r)) !
  A.y1 (S.toValue (y $ endOfRiver source sites' r)) !
  A.x2 (S.toValue (x $ endOfRiver target sites' r)) !
  A.y2 (S.toValue (y $ endOfRiver target sites' r)) !
  A.strokeWidth (widthForRiver viewboxWidth riverClaimed) !
  A.stroke (colourForRiver riverClaimed claims' r)
  where
    riverClaimed = Set.member r $ Set.map claimRiver claims'

endOfRiver :: (River -> SiteID) -> Set.Set Site -> River -> Site
endOfRiver f sites' r =
  if Set.null matchingSites
    then error "somehow a river had an end that isn't in the sites!"
    else Set.findMin matchingSites
  where
    matchingSites = Set.filter (\e -> Types.id e == f r) sites'

widthForRiver :: Double -> Bool -> S.AttributeValue
widthForRiver viewboxWidth riverClaimed =
  S.toValue $ baseWidth * (viewboxWidth * 0.1)
  where
    baseWidth =
      if riverClaimed
        then 0.05
        else 0.03

colourForRiver :: Bool -> Set.Set Claim -> River -> S.AttributeValue
colourForRiver riverClaimed claims' r =
  if riverClaimed
    then case maybePunter of
           Nothing -> unclaimedRiverColour
           (Just p) -> colourForPunter p
    else unclaimedRiverColour
  where
    maybePunter = claimPunter <$> find ((r ==) . claimRiver) claims'

colourForSite :: Set.Set SiteID -> Site -> S.AttributeValue
colourForSite mineIDs site =
  if Set.member (Types.id site) mineIDs
    then mineColour
    else siteColour

mineColour :: S.AttributeValue
mineColour = "#DBAFC1"

siteColour :: S.AttributeValue
siteColour = "#B8BDB5"

viewboxX :: Map -> Double
viewboxX m = minX m - padding

viewboxY :: Map -> Double
viewboxY m = minY m - padding

viewboxW :: Map -> Double
viewboxW m = mapWidth m + (padding * 2)

viewboxH :: Map -> Double
viewboxH m = mapHeight m + (padding * 2)

mapWidth :: Map -> Double
mapWidth m = maxX m - minX m

mapHeight :: Map -> Double
mapHeight m = maxY m - minY m

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
padding = 0.2

unclaimedRiverColour :: S.AttributeValue
unclaimedRiverColour = "#E2E4F6"

colourForPunter :: PunterID -> S.AttributeValue
colourForPunter 0 = green
colourForPunter 1 = purple
colourForPunter 2 = yellow
colourForPunter 3 = red
colourForPunter 4 = darkGreen
colourForPunter 5 = blue
colourForPunter _ = white

green :: S.AttributeValue
green = "#7DDF64"

purple :: S.AttributeValue
purple = "#8D86C9"

yellow :: S.AttributeValue
yellow = "#EAD637"

red :: S.AttributeValue
red = "#DB5461"

white :: S.AttributeValue
white = "#FFFFFF"

darkGreen :: S.AttributeValue
darkGreen = "#585123"

blue :: S.AttributeValue
blue = "#52B2CF"

-- SAMPLE DATA:
sampleMap :: Map
sampleMap =
  Map
  { sites =
      Set.fromList
        [ Site {Types.id = 0, loc = Just (0.0, 0.0)}
        , Site {Types.id = 1, loc = Just (1.0, 0.0)}
        , Site {Types.id = 2, loc = Just (2.0, 0.0)}
        , Site {Types.id = 3, loc = Just (2.0, -1.0)}
        , Site {Types.id = 4, loc = Just (2.0, -2.0)}
        , Site {Types.id = 5, loc = Just (1.0, -2.0)}
        , Site {Types.id = 6, loc = Just (0.0, -2.0)}
        , Site {Types.id = 7, loc = Just (0.0, -1.0)}
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

sampleMoves :: [Move]
sampleMoves =
  [ ClaimMove 0 (River 7 5)
  , Pass 1
  , ClaimMove 0 (River 3 2)
  , ClaimMove 1 (River 1 2)
  , ClaimMove 0 (River 5 3)
  , ClaimMove 1 (River 1 3)
  , ClaimMove 0 (River 0 7)
  , ClaimMove 1 (River 1 7)
  , ClaimMove 0 (River 4 3)
  , ClaimMove 1 (River 0 1)
  , ClaimMove 0 (River 5 4)
  , ClaimMove 1 (River 7 6)
  ]

sampleGameState :: GameState
sampleGameState =
  GameState
  { initialState =
      SetupState {GamePlay.punter = 0, punters = 2, GamePlay.map = sampleMap}
  , prevMoves = sampleMoves
  , shortestMinePaths = []
  }

testRender :: IO Int
testRender = writeStateToSvgFiles sampleGameState
