module Component.Counter where

import Data.Array
import Data.Function.Uncurried
import Effect
import Prelude
import Web.UIEvent.KeyboardEvent

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
import Halogen.Query as HQ
import Halogen.HTML as HH
import Halogen.HTML.CSS as HC
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Web.HTML.HTMLElement (focus)
import Data.Foldable (for_)


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
data Action = Initialize | Finalize | MoveRight | MoveLeft | MoveUp | MoveDown | NoMove

cells fig j = mapWithIndex (\i _ -> HH.div [HP.classes [HH.ClassName if (present (real_coords fig.board a b) [i,j]) then "figcell" else "cell"]] []) (range 0 9)
  where 
    a = fromMaybe 0 (fig.coords !! 0)
    b = fromMaybe 0 (fig.coords !! 1)
     

rows fig = mapWithIndex (\j _ -> HH.div [HP.classes [HH.ClassName "row"]] (cells fig j)) (range 0 5)

right :: Figure -> Figure 
right f = f { coords = new_coords }
  where
    x = fromMaybe 0 (f.coords !! 0)
    y = fromMaybe 0 (f.coords !! 1)
    new_coords = if (x + 1) <= 9 then [x + 1, y] else [x, y]

left :: Figure -> Figure 
left f = f { coords = new_coords }
  where
    x = fromMaybe 0 (f.coords !! 0)
    y = fromMaybe 0 (f.coords !! 1)
    new_coords = if (x - 1) >= 0 then [x - 1, y] else [x, y]

up :: Figure -> Figure 
up f = f { coords = new_coords }
  where
    x = fromMaybe 0 (f.coords !! 0)
    y = fromMaybe 0 (f.coords !! 1)
    new_coords = if (y - 1) >= 0 then [x, y - 1] else [x, y]

down :: Figure -> Figure 
down f = f { coords = new_coords }
  where
    x = fromMaybe 0 (f.coords !! 0)
    y = fromMaybe 0 (f.coords !! 1)
    new_coords = if (y + 1) <= 5 then [x, y + 1] else [x, y]

real_coords :: Board -> Int -> Int -> Board
real_coords board x y = map (\t -> [fromMaybe 0 (t!!0) + x, fromMaybe 0 (t!!1) + y]) board

present :: Board -> Array Int -> Boolean
present board x = not $ null $ filter (\t -> t == x) board


keyboardHandler e = case (code e) of 
  "ArrowLeft" -> MoveLeft
  "ArrowRight" -> MoveRight
  "ArrowUp" -> MoveUp
  "ArrowDown" -> MoveDown
  _ -> NoMove

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
      HH.div [ HP.classes [HH.ClassName "container"], HP.tabIndex 0, HP.ref (H.RefLabel "container"),  HE.onKeyDown keyboardHandler ]
      (rows fig)
    handleAction = case _ of
                       Initialize -> do
                            mElement <- HQ.getHTMLElementRef $ H.RefLabel "container"
                            for_ mElement \element -> do
                                H.liftEffect $ focus element
                       Finalize -> log "Finalize"
                       MoveRight -> H.modify_ \s -> right s
                       MoveLeft -> H.modify_ \s -> left s
                       MoveUp -> H.modify_ \s -> up s
                       MoveDown -> H.modify_ \s -> down s
                       NoMove -> H.modify_ \s -> s

