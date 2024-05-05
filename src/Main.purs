module Main where

import Prelude


import Component.Counter as Counter
import Effect (Effect)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)


main :: Effect Unit
main = do
  HA.runHalogenAff do 
     body <- HA.awaitBody
     runUI Counter.component 0 body
