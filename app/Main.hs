module Main
    ( main
    ) where

import           Cocobolo                       ( runCocobolo )
import           Options                        ( parseOptions )

-- | The entrypoint of the program parses argument options
-- and passes them onto the Cocobolo module!
main :: IO ()
main = parseOptions >>= runCocobolo
