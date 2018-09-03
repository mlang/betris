module Main where

import qualified Command.Betris as Betris
import Control.Monad (join)
import Options.Applicative

main = join $ execParser $ info (helper <*> Betris.command) $
    fullDesc
 <> progDesc "Braille Tetris"
