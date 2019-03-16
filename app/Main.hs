module Main where

import qualified Command.Betris as Betris
import Control.Monad (join)
import Data.Semigroup ((<>))
import Options.Applicative

main :: IO ()
main = join $ execParser $ info (helper <*> Betris.command) $
    fullDesc
 <> progDesc "Braille Tetris"
