module Main where

import Prelude


import Component.Pentomino as Pentomino
import Effect (Effect)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)


main :: Effect Unit
main = do
  HA.runHalogenAff do 
     body <- HA.awaitBody
     runUI Pentomino.component 0 body
