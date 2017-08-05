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

writeStateToFile :: GameState -> IO ()
writeStateToFile gameState = do
  let svgs =
        List.map
          (renderMovesOnMap (GamePlay.map $ initialState gameState))
          (moveInits gameState)
  mapM_ writeSvgToFile svgs

renderMovesOnMap :: Map -> [Move] -> (Int, String)
renderMovesOnMap map' moves =
  let claims' = GamePlay.claimsInMoves moves
      svgStr = renderSvg $ mapToSvg map' claims'
  in (length moves, svgStr)

writeSvgToFile :: (Int, String) -> IO ()
writeSvgToFile (moveCount, svg) =
  writeFile ("visualisations/move-" <> show moveCount <> ".svg") svg

mapToSvg :: Map -> Set.Set Claim -> S.Svg
mapToSvg m claims' =
  S.docTypeSvg ! A.version "1.1" ! A.width "500" ! A.height "500" !
  A.viewbox (viewboxAttrs m) $ do
    mapM_ (riverToSvg claims' $ sites m) $ rivers m
    mapM_ (siteToSvg $ mines m) $ sites m

siteToSvg :: Set.Set SiteID -> Site -> S.Svg
siteToSvg mineIDs site =
  S.circle ! A.cx (S.toValue $ x site) ! A.cy (S.toValue $ y site) ! A.r "0.1" !
  A.fill (colourForSite mineIDs site)

riverToSvg :: Set.Set Claim -> Set.Set Site -> River -> S.Svg
riverToSvg claims' sites' r =
  S.line ! A.x1 (S.toValue (x $ endOfRiver source sites' r)) !
  A.y1 (S.toValue (y $ endOfRiver source sites' r)) !
  A.x2 (S.toValue (x $ endOfRiver target sites' r)) !
  A.y2 (S.toValue (y $ endOfRiver target sites' r)) !
  A.strokeWidth (widthForRiver riverClaimed) !
  A.stroke (colourForRiver riverClaimed claims' r)
  where
    riverClaimed = Set.member r $ Set.map river claims'

endOfRiver :: (River -> SiteID) -> Set.Set Site -> River -> Site
endOfRiver f sites' r =
  if Set.null matchingSites
    then error "somehow a river had an end that isn't in the sites!"
    else Set.findMin matchingSites
  where
    matchingSites = Set.filter (\e -> Types.id e == f r) sites'

widthForRiver :: Bool -> S.AttributeValue
widthForRiver riverClaimed =
  if riverClaimed
    then claimedRiverWidth
    else unclaimedRiverWidth

colourForRiver :: Bool -> Set.Set Claim -> River -> S.AttributeValue
colourForRiver riverClaimed claims' r =
  if riverClaimed
    then case maybePunter of
           Nothing -> unclaimedRiverColour
           (Just p) -> colourForPunter p
    else unclaimedRiverColour
  where
    maybePunter = Types.punter <$> find (\claim -> river claim == r) claims'

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
  viewboxX m <> " " <> viewboxY m <> " " <> viewboxW m <> " " <> viewboxH m

viewboxX :: Map -> S.AttributeValue
viewboxX m = S.toValue $ minX m - padding

viewboxY :: Map -> S.AttributeValue
viewboxY m = S.toValue $ minY m - padding

viewboxW :: Map -> S.AttributeValue
viewboxW m = S.toValue $ width m + (padding * 2)

viewboxH :: Map -> S.AttributeValue
viewboxH m = S.toValue $ height m + (padding * 2)

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

sampleMoves :: [Move]
sampleMoves =
  [ ClaimMove Claim {Types.punter = 0, river = River {source = 7, target = 5}}
  , Pass 1
  , ClaimMove Claim {Types.punter = 0, river = River {source = 3, target = 2}}
  , ClaimMove Claim {Types.punter = 1, river = River {source = 1, target = 2}}
  , ClaimMove Claim {Types.punter = 0, river = River {source = 5, target = 3}}
  , ClaimMove Claim {Types.punter = 1, river = River {source = 1, target = 3}}
  , ClaimMove Claim {Types.punter = 0, river = River {source = 0, target = 7}}
  , ClaimMove Claim {Types.punter = 1, river = River {source = 1, target = 7}}
  , ClaimMove Claim {Types.punter = 0, river = River {source = 4, target = 3}}
  , ClaimMove Claim {Types.punter = 1, river = River {source = 0, target = 1}}
  , ClaimMove Claim {Types.punter = 0, river = River {source = 5, target = 4}}
  , ClaimMove Claim {Types.punter = 1, river = River {source = 7, target = 6}}
  ]

sampleGameState :: GameState
sampleGameState =
  GameState
  { initialState =
      SetupState {GamePlay.punter = 0, punters = 2, GamePlay.map = sampleMap}
  , prevMoves = sampleMoves
  }

testRender :: IO ()
testRender = writeStateToFile sampleGameState
