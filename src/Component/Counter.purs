module Component.Counter where

import Data.Array
import Prelude

import CSS.Common (hidden)
import CSS.Display (display, flex)
import CSS.Flexbox (AlignContentValue, JustifyContentValue, flexDirection, row, justifyContent)
import CSS.Geometry (width, height)
import CSS.Property (Value)
import CSS.Size (rem)
import CSS.String (fromString)
import Data.Const (Const)
import Data.Maybe (Maybe(..), fromMaybe)
import Effect.Aff.Class (class MonadAff)
import Effect.Class.Console (log)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.CSS as HC
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Data.Function.Uncurried
import Effect


class SpaceEvenly a where 
  spaceEvenly :: a


instance spaceEvenlyValue :: SpaceEvenly Value where 
  spaceEvenly = fromString "space-evenly"

instance spaceEvenlyAlignContentValue
    :: SpaceEvenly AlignContentValue where
  spaceEvenly = fromString "space-evenly"

instance spaceEvenlyJustifyContentValue
    :: SpaceEvenly JustifyContentValue where
  spaceEvenly = fromString "space-evenly"

type Board = Array (Array Int)

type Figure = { base :: Array (Array Int), board :: Board, coords :: Array Int }

fig :: Figure
fig = { base : [[0,0], [2,0], [2,2], [0,2]], board : [[0,0], [1, 0], [0, 1], [1, 1], [0, 2]], coords : [2,1] }

type Input = Int
type Output = Figure
-- type State = { count :: Int }
type State = Figure
type Slots :: forall k. Row k 
type Slots = ()

type Query :: ∀ k. k -> Type 
type Query = Const Void
data Action = Initialize | Finalize | Increment | Decrement

cells fig j = mapWithIndex (\i _ -> HH.div [HP.classes [HH.ClassName if (present (real_coords fig.board i j) [b,a]) then "figcell" else "cell"]] []) (range 0 8)
  where 
    a = fromMaybe 0 (fig.coords !! 0)
    b = fromMaybe 0 (fig.coords !! 1)
     

rows fig = mapWithIndex (\j _ -> HH.div [HP.classes [HH.ClassName "row"]] (cells fig j)) (range 0 5)

down :: Figure -> Figure 
down f = f { coords = new_coords }
  where
    x = fromMaybe 0 (f.coords !! 0)
    y = fromMaybe 0 (f.coords !! 1)
    new_coords = if (x + 1) <= 8 then [x + 1, y] else [x, y]

real_coords :: Board -> Int -> Int -> Board
real_coords board x y = map (\t -> [fromMaybe 0 (t!!0) + x, fromMaybe 0 (t!!1) + y]) board

present :: Board -> Array Int -> Boolean
present board x = not $ null $ filter (\t -> t == x) board


component
  :: ∀ m
  . MonadAff m
  => H.Component Query Input Output m
component = H.mkComponent
    { initialState: \i -> fig
    , render
    , eval: H.mkEval H.defaultEval {
      initialize = Just Initialize 
      , finalize = Just Finalize
      , handleAction = handleAction
      }
    }
    where
    render :: State -> H.ComponentHTML Action Slots m 
    render fig = 
      HH.div [ HP.classes [HH.ClassName "container"], HP.tabIndex 0,  HE.onKeyDown (\_ -> Increment) ]
      (rows fig)
    handleAction = case _ of
                       Initialize -> log "Initialize"
                       Finalize -> log "Finalize"
                       Increment -> H.modify_ \s -> down s
                       Decrement -> H.modify_ \s -> s

