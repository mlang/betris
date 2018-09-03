{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecordWildCards #-}
module Command.Betris (command, Options(..), betris) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM.TBChan
import Control.Lens hiding (argument)
import Control.Monad (forever)
import Control.Monad.STM (atomically)
import Data.Char (chr)
import Data.Foldable (for_)
import qualified Data.Map as Map
import Data.Time.Units
import Data.Version (showVersion)
import Game.Tetris
import Graphics.Vty
import Linear.V2 (V2(..))
import Paths_betris (version)
import Prelude hiding (Left, Right)
import Options.Applicative hiding (command, (<|>))

command :: Parser (IO ())
command = betris <$> (versionOption <*> programOptions)

data Options = Options { initialDelay :: Millisecond } deriving (Eq, Show)

programOptions :: Parser Options
programOptions =
  Options <$> option auto (long "initial-delay" <> metavar "DURATION" <>
                           value (fromMicroseconds 1000000) <> showDefault <>
                           help "Initial delay")

data Event e = Tick | Ev e deriving (Eq, Read, Show, Functor)

betris Options{..} = do
  vty <- mkVty =<< userConfig
  chan <- newTBChanIO 10
  game <- initGame 0
  speed <- newTVarIO $ fromIntegral $ toMicroseconds initialDelay

  forkIO $ forever $ do
    e <- nextEvent vty
    atomically $ writeTBChan chan $ Ev e

  forkIO $ forever $ do
    atomically $ writeTBChan chan Tick
    delay <- readTVarIO speed
    atomically $ modifyTVar speed ((-) 500)
    threadDelay delay

  _ <- play vty chan game
  shutdown vty
  putStrLn ""

play vty chan game
  | isGameOver game = pure $ game ^. score
  | otherwise = do
    update vty $ picForImage $
      string defAttr (emboss game) <|> string defAttr (show $ game ^. score)
    e <- atomically $ readTBChan chan
    case e of
      Ev (EvKey KEsc []) -> pure $ game ^. score
      Tick -> timeStep game >>= play vty chan
      Ev (EvKey KLeft []) -> play vty chan (hardDrop game)
      Ev (EvKey KUp []) -> play vty chan (shift Left game)
      Ev (EvKey KDown []) -> play vty chan (shift Right game)
      Ev (EvKey KEnter []) -> play vty chan (rotate game)
      _ -> play vty chan game

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

versionOption :: Parser (a -> a)
versionOption = infoOption (showVersion version) $
    long "version"
 <> help "Show version"
