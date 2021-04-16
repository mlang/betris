{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecordWildCards #-}
module Command.Betris (command, Options(..), betris) where

import Prelude hiding (Left, Right)
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM.TChan
import Control.Lens hiding (argument)
import Control.Monad (forever)
import Control.Monad.STM (atomically)
import Data.Char (chr)
import Data.IORef
import qualified Data.Map as Map
import Data.Semigroup ((<>))
import Data.Time.Units
import Data.Version (showVersion)
import Game.Tetris
import Graphics.Vty.Config (userConfig)
import Graphics.Vty.Input hiding (Event)
import qualified Graphics.Vty.Input as Vty (Event)
import Linear.V2 (V2(..), _x)
import Paths_betris (version)
import Prelude hiding (Left, Right)
import Options.Applicative hiding (command, (<|>))
import System.Console.ANSI
import System.IO (hFlush, stdout)

command :: Parser (IO ())
command = betris <$> (versionOption <*> programOptions)

newtype Options = Options { initialDelay :: Millisecond } deriving (Eq, Show)

programOptions :: Parser Options
programOptions = Options <$> initialDelayOption

data Event e = Tick | Ev e deriving (Eq, Read, Show, Functor)

betris :: Options -> IO ()
betris Options{..} = do
  input <- inputForConfig =<< userConfig
  chan <- newTChanIO
  game <- initGame 0
  speed <- newIORef $ fromIntegral $ toMicroseconds initialDelay

  _ <- forkIO . forever . atomically $
    readTChan (_eventChannel input) >>= writeTChan chan . Ev
  _ <- forkIO $ forever $ do
    readIORef speed >>= threadDelay
    modifyIORef speed (subtract 500)
    atomically $ writeTChan chan Tick

  _ <- play chan game
  shutdownInput input
  putStrLn ""

play :: TChan (Event Vty.Event) -> Game -> IO Game
play chan tetris
  | isGameOver tetris = pure tetris
  | otherwise = do
    putStr $ "\r"
          <> emboss tetris
          <> " [" <> show (tetris ^. score) <> "]"
          <> clearFromCursorToLineEndCode
    hFlush stdout
    atomically (readTChan chan) >>= \case
      Tick                 -> play chan =<< timeStep tetris
      Ev (EvKey KLeft [])  -> play chan $ hardDrop tetris
      Ev (EvKey KUp [])    -> play chan $ Left `shift` tetris
      Ev (EvKey KDown [])  -> play chan $ Right `shift` tetris
      Ev (EvKey KEnter []) -> play chan $ rotate tetris
      Ev (EvKey KEsc [])   -> pure tetris
      _                    -> play chan tetris

emboss :: Game -> String
emboss game = map go [1, 3 .. boardHeight + 6] where
  go y = chr $ foldr (f y) 0x2800 [((0,0),1), ((1,0), 2), ((2,0), 4)
                                  ,((0,1),8), ((1,1),16), ((2,1),32)
                                  ,((3,0),64),((3,1),128)]
  f y ((x',y'), v) a = case Map.lookup (V2 (x+x') (y+y')) fullBoard of
    Just _ -> a + v
    _ -> a
  minx b = minimum $ (boardWidth - 3) : map (^. _x) (coords b)
  x = minx (game ^. block)
  fullBoard = game ^. board <> blk (game ^. block) <> blk next
  next = let b = initBlock (game ^. nextShape) in
         translateBy (-4) Down $ translateBy (x - minx b) Right b
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
