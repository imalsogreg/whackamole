{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE RecursiveDo       #-}
{-# LANGUAGE RankNTypes        #-}

module Main where

import Control.Concurrent (threadDelay)
import Control.Monad.IO.Class (liftM)
import Data.Time
import Data.Time.Clock
import Safe
import System.Random

import GHCJS.DOM
import GHCJS.DOM.Document
import GHCJS.DOM.Element
import GHCJS.DOM.HTMLElement
import GHCJS.DOM.HTMLDocument
import Reflex.Dom


data MoleState = MoleDown | MoleUp | MoleWhacked
               deriving (Eq)

data MoleAction = Whack | GoUp | GoDown
                deriving (Eq)

-- What happens to the mole in response to an action command
-- And, does this result in a successful whack for the player?
updateMole :: MoleAction -> (MoleState) -> (MoleState,Bool)
updateMole GoUp   MoleDown    = (MoleUp, False)
updateMole GoDown MoleUp      = (MoleDown, False)
updateMole Whack  MoleUp      = (MoleWhacked, True)
updateMole Whack  MoleWhacked = (MoleWhacked, False)
updateMole GoDown MoleWhacked = (MoleDown, False)

------------------------------------------------------------------------------
whackAMoleWidget :: (RandomGen r, MonadWidget t m) => r -> UTCTime -> m ()
whackAMoleWidget rnd t0 = mdo

  let rands        = nRnd 9 rnd
      moleRndGens  = zip [(0::Int)..] rands
      moleDivAttrs = "style" =: "background-color: #d7e0d1;max-width:500px"

  dynText =<< forDyn score (("Score: " <>) . show)  -- HTML show the score

  el "br" $ return ()                               -- HTML Line break

  moleEvents <- elAttr "div" moleDivAttrs $ do
    leftmost <$> forM moleRndGens
      (\(k,v) -> moleWidget v t0 (constDyn 0.2))

  score <- foldDyn (\_ acc -> acc + (100 :: Int)) 0 moleEvents

  return ()


------------------------------------------------------------------------------
moleWidget :: (RandomGen r, MoleWidget t m)
           => r
           -> UTCTime
           -> Dynamic t Double
           -> m ()
moleWidget rnd t0 popupRate = do

  picAttrs <- forDyn moleState $ \s ->
    ("draggable" =: "false") <>
    ("src" =: "static/" <> show s <> ".png")

  molePic <- fmap fst $ elDynAttr' "img" picAttrs $ return ()

  let (r,r') = split rnd

  popupTimes  <- inhomogeneousPoisson r (current popupRate) 5 t0
  goDownTimes <- mapDyn (*2) popupRate >>= \goDownRate ->
    inhomogeneousPoisson r' (current goDownRate) 10 t0

  moleState  <- foldDyn updateMole MoleDown
                (leftMost [fmap (const GoUp)   popupTimes
                          ,fmap (const GoDown) popupTimes
                          ,fmap (const Whack)  whacks
                          ])

  return . ffilter (== (MoleWhacked,True)) . updated $ moleState

------------------------------------------------------------------------------
nRnd :: RandomGen g => Int -> g -> [g]
nRand 0 g = []
nRand 1 g = [g]
nRand n g = let (g',g'') = split g in g' : nRnd (n-1) g''

waitUntilJust :: IO (Maybe a) -> IO a
waitUntilJust a = do
  mx =<< a
  case mx of
    Just x -> return x
    Nothing -> do
      threadDelay 10000
      waitUntilJust a

main :: IO ()
main = do
  rnd    <- getStdGen
  runWebGui $ \webView -> do
    doc <- waitUntilJust $ liftM (fmap castToHTMLDocument) $
           webViewGetDomDocument webView
    let btag = "reflex-area" :: String
    root <- waitUntilJust $ liftM (fmap castToHTMLElement) $
            documentGetElementById doc btag
    attachWidget root webView (runApp tStart)

runApp :: forall t m g.(MonadWidget t m, RandomGen g) => g -> m ()
runApp = do
  liftIO getCurrentTime >>= \t0 -> whackAMoleWidget g t0
