{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM.TBChan
import Control.Lens hiding (argument)
import Control.Monad (forever)
import Control.Monad.STM (atomically)
import Data.Char (chr)
import Data.Foldable (for_)
import qualified Data.Map as Map
import Data.Semigroup ((<>))
import Game.Tetris
import Graphics.Vty
import Linear.V2 (V2(..))
import Prelude hiding (Left, Right)
import Options.Applicative hiding ((<|>))

versionOption :: Parser (a -> a)
versionOption = infoOption "0.1.0.1" (long "version" <> help "Show version")

data Options = Options { initialDelay :: Int } deriving Eq

programOptions :: Parser Options
programOptions =
  Options <$> option auto (long "initial-delay" <>
                           value 1000000 <>
                           help "Initial delay in microseconds")

data Event e = Tick | Ev e deriving (Eq, Read, Show, Functor)

main = do
  Options {..} <- execParser $
    info (helper <*> versionOption <*> programOptions)
         (fullDesc <> progDesc "Braille Tetris")
  vty <- mkVty defaultConfig
  chan <- atomically $ newTBChan 10
  game <- initGame 0
  speed <- newTVarIO initialDelay

  forkIO $ forever $ do
    e <- nextEvent vty
    atomically $ writeTBChan chan $ Ev e

  forkIO $ forever $ do
    atomically $ writeTBChan chan Tick
    delay <- readTVarIO speed
    atomically $ modifyTVar speed ((-) 100)
    threadDelay delay

  consume vty chan game

  shutdown vty

consume vty chan game = if isGameOver game then pure () else do
  update vty $ picForImage $
   string defAttr (emboss game) <|> string defAttr (show $ game ^. score)
  e <- atomically $ readTBChan chan
  case e of
    Ev (EvKey KEsc []) -> pure ()
    Tick -> timeStep game >>= consume vty chan
    Ev (EvKey KLeft []) -> consume vty chan (hardDrop game)
    Ev (EvKey KUp []) -> consume vty chan (shift Left game)
    Ev (EvKey KDown []) -> consume vty chan (shift Right game)
    Ev (EvKey KEnter []) -> consume vty chan (rotate game)
    _ -> consume vty chan game

emboss :: Game -> String
emboss g = map go [1, 3 .. boardHeight] where
  go y = chr $ foldr (f y) 0x2800 [((0,0),1), ((1,0), 2), ((2,0), 4)
                                  ,((0,1),8), ((1,1),16), ((2,1),32)
                                  ,((3,0),64),((3,1),128)]
  f y ((x',y'), v) a = case Map.lookup (V2 (x+x') (y+y')) fullBoard of
    Just _ -> a + v
    _ -> a
  x = minimum $ (boardWidth - 3) : map (\(V2 x _) -> x) (coords $ g ^. block)
  fullBoard = g ^. board <> blk (g ^. block)
  blk b = Map.fromList $ map (, b ^. shape) $ coords b
