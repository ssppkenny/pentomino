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

fig4 :: Figure 
fig4 = { base :  [[1,0], [2,1], [1,2], [0,1]], board : [[1,0], [1,1], [1,2], [0,1], [2,1]], coords : [2,3], number : 5 }

fig5 :: Figure 
fig5 = { base :  [[1,0], [2,1], [1,2], [0,1]], board : [[1,0], [2,0], [0,1], [1,1], [1,2]], coords : [6,4], number : 6 }

fig6 :: Figure 
fig6 = { base :  [[0,0], [2,0], [2,2], [0,2]], board : [[0,1], [1,1], [2,1], [3,1], [3,0]], coords : [0,5], number : 7 }

fig7 :: Figure 
fig7 = { base :  [[0,0], [2,0], [2,2], [0,2]], board : [[0,0], [0,1], [1,1], [1,2], [2,2]], coords : [5,2], number : 8 }

fig8 :: Figure 
fig8 = { base :  [[0,0], [2,0], [2,2], [0,2]], board : [[0,0], [0,1], [1,1], [2,1], [2,0]], coords : [1,3], number : 9 }

fig9 :: Figure 
fig9 = { base :  [[1,0], [2,1], [1,2], [0,1]], board : [[0,1], [1,0], [1,1], [1,2], [1,3]], coords : [7,1], number : 10 }

fig10 :: Figure 
fig10 = { base :  [[0,0], [2,0], [2,2], [0,2]], board : [[0,0], [1,0], [1,1], [2,1], [3,1]], coords : [4,1], number : 11 }

fig11 :: Figure 
fig11 = { base :  [[0,0], [2,0], [2,2], [0,2]], board : [[0,0], [0,1], [0,2], [1,2], [2,2]], coords : [4,3], number : 12 }

type Input = Int
type Output = Figure
-- type State = { count :: Int }
type State = { active_figure :: Figure, figures :: Array Figure }
type Slots :: forall k. Row k 
type Slots = ()

type Query :: ∀ k. k -> Type 
type Query = Const Void
data Action = Initialize | Finalize | MoveRight | MoveLeft | MoveUp | MoveDown | NoMove | Activate Int | Rotate | Flip


max :: Int -> Int -> Int 
max a b = if a > b then a else b

size :: Board -> Int
size b = max mx my
  where
    t = map (\n -> n !! 0) b 
    r = map (\n -> n !! 1) b 
    xs = map (\x -> fromMaybe 0 x) t 
    ys = map (\x -> fromMaybe 0 x) r 
    mx = maxdiff xs
    my = maxdiff ys

maximum :: Array Int -> Int 
maximum xs = foldl (\x y -> if x > y then x else y) 0 xs

minimum :: Array Int -> Int 
minimum xs = foldl (\x y -> if x < y then x else y) 100 xs


maxdiff :: Array Int -> Int 
maxdiff xs = (maximum xs) - (minimum xs)


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
     

rows fig figs = mapWithIndex (\j _ -> 
  if j <= 5 then HH.div [HP.classes [HH.ClassName "row"]] (cells fig figs j) else HH.div [ HP.classes [HH.ClassName "imagerow"] ] [HH.img [HP.src "pents1.gif"]]
  ) (range 0 6)


move :: Figure -> Array Figure -> Array Int -> Board -> State
move f figs new_coords new_board = { active_figure : nf, figures : new_figs }
  where
    nf = f { coords = new_coords, board = new_board }
    t = fromMaybe [] (tail figs)
    new_figs = cons nf t

addImage arrayOfDivs = snoc arrayOfDivs (HH.div [] [(HH.img [HP.src "pents1.gif"])])   


right :: State -> State 
right {active_figure : f, figures : figs} = move f figs new_coords f.board
  where
    x = fromMaybe 0 (f.coords !! 0)
    y = fromMaybe 0 (f.coords !! 1)
    new_coords = [x + 1, y]

left :: State -> State 
left {active_figure : f, figures : figs} =  move f figs new_coords f.board
  where
    x = fromMaybe 0 (f.coords !! 0)
    y = fromMaybe 0 (f.coords !! 1)
    new_coords = [x - 1, y]

up :: State -> State 
up {active_figure : f, figures : figs} = move f figs new_coords f.board
  where
    x = fromMaybe 0 (f.coords !! 0)
    y = fromMaybe 0 (f.coords !! 1)
    new_coords = [x, y - 1]

down :: State -> State 
down {active_figure : f, figures : figs} = move f figs new_coords f.board
  where
    x = fromMaybe 0 (f.coords !! 0)
    y = fromMaybe 0 (f.coords !! 1)
    new_coords = [x, y + 1]

rotate :: State -> State 
rotate {active_figure : f, figures : figs} = move f figs new_coords new_board 
  where
    sz = size $ f.board
    new_board = map (\n ->  [ sz - fromMaybe 0 (n!!1), fromMaybe 0 (n!!0) ]) f.board
    a1 = fromMaybe [] (f.board !! 0)
    b1 = fromMaybe [] (new_board !! 0)
    a11 = fromMaybe 0 (a1 !! 0)
    b11 = fromMaybe 0 (b1 !! 0)
    a12 = fromMaybe 0 (a1 !! 1)
    b12 = fromMaybe 0 (b1 !! 1)
    dx = a11 - b11
    dy = a12 - b12
    x = fromMaybe 0 (f.coords !! 0)
    y = fromMaybe 0 (f.coords !! 1)
    new_coords = [x + dx, y + dy]


fliph :: State -> State 
fliph {active_figure : f, figures : figs} = move f figs new_coords new_board 
  where
    sz = size $ f.board
    new_board = map (\n ->  [ fromMaybe 0 (n!!0), sz - fromMaybe 0 (n!!1) ]) f.board
    a1 = fromMaybe [] (f.board !! 0)
    b1 = fromMaybe [] (new_board !! 0)
    a11 = fromMaybe 0 (a1 !! 0)
    b11 = fromMaybe 0 (b1 !! 0)
    a12 = fromMaybe 0 (a1 !! 1)
    b12 = fromMaybe 0 (b1 !! 1)
    dx = a11 - b11
    dy = a12 - b12
    x = fromMaybe 0 (f.coords !! 0)
    y = fromMaybe 0 (f.coords !! 1)
    new_coords = [x - dx, y - dy]


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
  "KeyP" -> Activate 1
  "KeyI" -> Activate 2
  "KeyT" -> Activate 3
  "KeyY" -> Activate 4
  "KeyX" -> Activate 5
  "KeyF" -> Activate 6
  "KeyL" -> Activate 7
  "KeyW" -> Activate 8
  "KeyU" -> Activate 9
  "KeyZ" -> Activate 10
  "KeyN" -> Activate 11
  "KeyV" -> Activate 12
  "KeyR" -> Rotate
  "KeyH" -> Flip
  _ -> NoMove

component
  :: forall m
  . MonadAff m
  => H.Component Query Input Output m
component = H.mkComponent
    { initialState: \i -> { active_figure : fig, figures : [fig, fig1, fig2, fig3, fig4, fig5, fig6, fig7, fig8, fig9, fig10, fig11 ] }
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
                       Rotate -> H.modify_ \s -> rotate s
                       Flip -> H.modify_ \s -> fliph s

