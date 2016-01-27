module Core where

import Control.Monad.Trans.State

irods = "4.2"

h :: [String] -> StateT [String] IO (Int, [String])
h ps = return (0, ["Hello World"])
