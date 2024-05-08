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
import Data.Int (toStringAs, radix)


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

type Figure = { base :: Array (Array Int), board :: Board, coords :: Array Int, number :: Int }

fig :: Figure
fig = { base : [[0,0], [2,0], [2,2], [0,2]], board : [[0,0], [1, 0], [0, 1], [1, 1], [0, 2]], coords : [2,1], number : 1 }

fig1 :: Figure 
fig1 = { base :  [[0,2], [2,0], [4,2], [2,4]], board : [[0,0], [1,0], [2,0], [3,0], [4,0]], coords : [3,2], number : 2 }


fig2 :: Figure 
fig2 = { base :  [[0,0], [2,0], [2,2], [0,2]], board : [[0,0], [1,0], [2,0], [1,1], [1,2]], coords : [5,2], number : 3 }

fig3 :: Figure 
fig3 = { base :  [[0,0], [2,0], [2,2], [0,2]], board : [[0,0], [1,0], [1,1], [1,2], [2,2]], coords : [7,3], number : 4 }

type Input = Int
type Output = Figure
-- type State = { count :: Int }
type State = { active_figure :: Figure, figures :: Array Figure }
type Slots :: forall k. Row k 
type Slots = ()

type Query :: ∀ k. k -> Type 
type Query = Const Void
data Action = Initialize | Finalize | MoveRight | MoveLeft | MoveUp | MoveDown | NoMove | Activate Int

cellInFig :: Figure -> Int -> Int -> Boolean
cellInFig fig' i j = present (realCoords fig'.board a b) [i, j]
  where 
    a = fromMaybe 0 (fig'.coords !! 0)
    b = fromMaybe 0 (fig'.coords !! 1)

cellClass figs i j = if (length figs_present > 0) then "cellfig" <> (fromMaybe "" (strings !! 0)) else "cell" 
  where 
    figs_present = filter (\x -> cellInFig x i j) figs
    strings = map (\f' -> (fromMaybe "" $ map (\x -> toStringAs x f'.number) (radix 10))) figs_present
    

cells fig figs j = mapWithIndex (\i _ -> HH.div [HP.classes [HH.ClassName (cellClass figs i j)]] []) (range 0 9)
  where 
    cls = "figcell" <> (fromMaybe "" $ map (\x -> toStringAs x fig.number) (radix 10))
     

rows fig figs = mapWithIndex (\j _ -> HH.div [HP.classes [HH.ClassName "row"]] (cells fig figs j)) (range 0 5)


move :: Figure -> Array Figure -> Array Int -> State
move f figs new_coords = { active_figure : nf, figures : new_figs }
  where
    nf = f { coords = new_coords }
    t = fromMaybe [] (tail figs)
    new_figs = cons nf t

  


right :: State -> State 
right {active_figure : f, figures : figs} = move f figs new_coords 
  where
    x = fromMaybe 0 (f.coords !! 0)
    y = fromMaybe 0 (f.coords !! 1)
    new_coords = if (x + 1) <= 9 then [x + 1, y] else [x, y]

left :: State -> State 
left {active_figure : f, figures : figs} =  move f figs new_coords
  where
    x = fromMaybe 0 (f.coords !! 0)
    y = fromMaybe 0 (f.coords !! 1)
    new_coords = if (x - 1) >= 0 then [x - 1, y] else [x, y]

up :: State -> State 
up {active_figure : f, figures : figs} = move f figs new_coords
  where
    x = fromMaybe 0 (f.coords !! 0)
    y = fromMaybe 0 (f.coords !! 1)
    new_coords = if (y - 1) >= 0 then [x, y - 1] else [x, y]

down :: State -> State 
down {active_figure : f, figures : figs} = move f figs new_coords
  where
    x = fromMaybe 0 (f.coords !! 0)
    y = fromMaybe 0 (f.coords !! 1)
    new_coords = if (y + 1) <= 5 then [x, y + 1] else [x, y]

activate :: State -> Int -> State
activate {active_figure : f, figures : figs } n = { active_figure : nf, figures : new_figs }
  where 
    nf = fromMaybe f ((filter (\t -> t.number == n) figs) !! 0)
    ind = fromMaybe 0 (elemIndex nf figs)
    a1 = fromMaybe f (figs !! 0)
    a2 = fromMaybe f (figs !! ind)
    new_figs = if nf == f then figs else fromMaybe [] (modifyAt ind  (\_ -> a1) (fromMaybe [] (modifyAt 0 (\_ -> a2) figs)))

realCoords :: Board -> Int -> Int -> Board
realCoords board x y = map (\t -> [fromMaybe 0 (t!!0) + x, fromMaybe 0 (t!!1) + y]) board

present :: Board -> Array Int -> Boolean
present board x = not $ null $ filter (\t -> t == x) board


keyboardHandler e = case (code e) of 
  "ArrowLeft" -> MoveLeft
  "ArrowRight" -> MoveRight
  "ArrowUp" -> MoveUp
  "ArrowDown" -> MoveDown
  "Digit1" -> Activate 1
  "Digit2" -> Activate 2
  "Digit3" -> Activate 3
  "Digit4" -> Activate 4
  _ -> NoMove

component
  :: ∀ m
  . MonadAff m
  => H.Component Query Input Output m
component = H.mkComponent
    { initialState: \i -> { active_figure : fig, figures : [fig, fig1, fig2, fig3] }
    , render
    , eval: H.mkEval H.defaultEval {
      initialize = Just Initialize 
      , finalize = Just Finalize
      , handleAction = handleAction
      }
    }
    where
    render :: State -> H.ComponentHTML Action Slots m 
    render { active_figure : fig, figures : figures } = 
      HH.div [ HP.classes [HH.ClassName "container"], HP.tabIndex 0, HP.ref (H.RefLabel "container"),  HE.onKeyDown keyboardHandler ]
      (rows fig figures)
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
                       Activate n -> H.modify_ \s -> activate s n

