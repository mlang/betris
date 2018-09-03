{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecordWildCards #-}
module Command.Betris (command, Options(..), betris) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM.TChan
import Control.Lens hiding (argument)
import Control.Monad (forever)
import Control.Monad.STM (atomically)
import Data.Char (chr)
import Data.Foldable (for_)
import Data.IORef
import qualified Data.Map as Map
import Data.Time.Units
import Data.Version (showVersion)
import Game.Tetris
import Graphics.Vty hiding (Event)
import qualified Graphics.Vty as Vty (Event)
import Linear.V2 (V2(..))
import Paths_betris (version)
import Prelude hiding (Left, Right)
import Options.Applicative hiding (command, (<|>))

command :: Parser (IO ())
command = betris <$> (versionOption <*> programOptions)

data Options = Options { initialDelay :: Millisecond } deriving (Eq, Show)

programOptions :: Parser Options
programOptions = Options <$> initialDelayOption

data Event e = Tick | Ev e deriving (Eq, Read, Show, Functor)

betris :: Options -> IO ()
betris Options{..} = do
  vty <- mkVty =<< userConfig
  chan <- newTChanIO
  game <- initGame 0
  speed <- newIORef $ fromIntegral $ toMicroseconds initialDelay

  forkIO $ forever $ nextEvent vty >>= atomically . writeTChan chan . Ev
  forkIO $ forever $ do
    readIORef speed >>= threadDelay
    modifyIORef speed ((-) 500)
    atomically $ writeTChan chan Tick

  _ <- play vty chan game
  shutdown vty
  putStrLn ""

play :: Vty -> TChan (Event Vty.Event) -> Game -> IO Game
play vty chan tetris
  | isGameOver tetris = pure tetris
  | otherwise = do
    update vty $ picForImage $
      string defAttr (emboss tetris) <|> string defAttr (show $ tetris ^. score)
    e <- atomically $ readTChan chan
    case e of
      Tick                 -> play vty chan =<< timeStep tetris
      Ev (EvKey KLeft [])  -> play vty chan $ hardDrop tetris
      Ev (EvKey KUp [])    -> play vty chan $ Left `shift` tetris
      Ev (EvKey KDown [])  -> play vty chan $ Right `shift` tetris
      Ev (EvKey KEnter []) -> play vty chan $ rotate tetris
      Ev (EvKey KEsc [])   -> pure tetris
      _                    -> play vty chan tetris

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

initialDelayOption :: Parser Millisecond
initialDelayOption = option auto $
    long "initial-delay"
 <> short 'i'
 <> metavar "DURATION"
 <> value (fromMicroseconds 1000000)
 <> showDefault
 <> help "Initial delay"

versionOption :: Parser (a -> a)
versionOption = infoOption (showVersion version) $
    long "version"
 <> help "Show version"
