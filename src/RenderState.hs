{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedRecordDot #-} --todo
{-# LANGUAGE RankNTypes #-}

-- todo refactor: get rid of BoardInfo, access directly from board Array and pass Snake & Board directly into GameState

{-|
This module defines the board. A board is an array of CellType elements indexed by a tuple of ints: the height and width.

for example, The following array represents a 3 by 4 board (left top corner is (1,1); right bottom corner is (3,4)) with a snake at 
(2, 2) and (2, 3) and an apple at (3,4)

< ((1,1) : Empty), ((1,2) : Empty), ((1,3) : Empty),     ((1,2) : Empty)
, ((2,1) : Empty), ((2,2) : Snake)  ((2,3) : SnakeHead)  ((2,4) : Empty)
, ((3,1) : Empty), ((3,2) : Empty), ((3,3) : Empty),     ((3,4) : Apple) >

Which would look like this:

- - - -
- 0 $ -
- - - X
-}
module RenderState where

-- todo: haskell function, update, that operates on array's index and value, for updating
-- that is, a fmap over both indexer and element...
-- todotodo: make function and contribute to ghc...!
-- like with hashmaps...
-- type signature:
-- update Idx i => (Array i e) --> (i --> e --> t) --> (Array i t)
-- TODO: #type maniulations visualisations blog (concat map, fmap...)

-- This are all imports you need. Feel free to import more things.
import Data.Array ( (//), listArray, Array, elems, Ix, assocs, bounds, array, indices )
import Data.Foldable ( foldl' )
import Data.List ( intercalate, unfoldr )

-- A point is just a tuple of integers.
type Point = (Int, Int)

-- | Cell types. We distinguish between Snake and SnakeHead
data CellType = Empty | Snake | SnakeHead | Apple
    deriving (Show, Eq)

-- | The board info is just a description of height and width.
data BoardInfo = BoardInfo {height :: Int, width :: Int}
    deriving (Show, Eq)

-- ^The board is an Array indexed by points with elements of type CellType
type Board = Array Point CellType

-- | A delta is a small change in the board at some points. For example [((2,2), SnakeHead), ((2,1), Empty)]
--   would represent the change "cell (2,2) should change to become the SnakeHead and cell (2,1) should change by an empty cell"
type DeltaBoard = [(Point, CellType)]

-- | The render message represent all message the GameState can send to the RenderState
--   Right now Possible messages are a RenderBoard with a payload indicating which cells change
--   or a GameOver message.
data RenderMessage = RenderBoard DeltaBoard | GameOver
    deriving (Show, Eq)

-- | The RenderState contains the board and if the game is over or not.
data RenderState   = RenderState {board :: Board, gameOver :: Bool}
    deriving (Show, Eq)

-- | Given The board info, this function should return a board with all Empty cells
emptyGrid :: BoardInfo -> Board
emptyGrid (BoardInfo height width) = listArray (lowerBound, upperBound) (replicate (height*width) Empty)
    where (lowerBound, upperBound) = ((1,1), (height, width))


{-
This is a test for emptyGrid. It should return 
array ((1,1),(2,2)) [((1,1),Empty),((1,2),Empty),((2,1),Empty),((2,2),Empty)]
-}
-- >>> emptyGrid (BoardInfo 2 2)
-- array ((1,1),(2,2)) [((1,1),Empty),((1,2),Empty),((2,1),Empty),((2,2),Empty)]


-- | Given BoardInfo, initial point of snake and initial point of apple, builds a board
buildInitialBoard
  :: BoardInfo -- ^ Board size
  -> Point     -- ^ initial point of the snake
  -> Point     -- ^ initial Point of the apple
  -> RenderState
buildInitialBoard boardInfo initialSnakePoint initialApplePoint =
    RenderState {
        board = emptyGrid boardInfo // [(initialSnakePoint, SnakeHead), (initialApplePoint, Apple)],
        gameOver = False
    }

{- Test for buildInitialBoard.
>>> buildInitialBoard (BoardInfo 2 2) (1,1) (2,2)
RenderState {board = array ((1,1),(2,2)) [((1,1),SnakeHead),((1,2),Empty),((2,1),Empty),((2,2),Apple)], gameOver = False}

Expected:
RenderState {board = array ((1,1),(2,2)) [((1,1),SnakeHead),((1,2),Empty),((2,1),Empty),((2,2),Apple)], gameOver = False}
-}

-- | Given the current render state, and a message -> update the render state
updateRenderState :: RenderState -> RenderMessage -> RenderState
updateRenderState renderState GameOver
    = renderState {gameOver = True}
updateRenderState oldState@(RenderState oldBoard _) (RenderBoard deltaBoard)
    = oldState {board = oldBoard // deltaBoard}

{-
This is a test for updateRenderState

message1 should return:
RenderState {board = array ((1,1),(2,2)) [((1,1),Empty),((1,2),SnakeHead),((2,1),Apple),((2,2),Apple)], gameOver = False}

message2 should return:
RenderState {board = array ((1,1),(2,2)) [((1,1),SnakeHead),((1,2),Empty),((2,1),Empty),((2,2),Apple)], gameOver = True}
-}
-- >>> initial_board =  buildInitialBoard (BoardInfo 2 2) (1,1) (2,2)
-- >>> message1 = RenderBoard [((1,2), SnakeHead), ((2,1), Apple), ((1,1), Empty)]
-- >>> message2 = GameOver
-- >>> updateRenderState initial_board message1
-- >>> updateRenderState initial_board message2


-- | Provisional Pretty printer
--   For each cell type choose a string to representing. 
--   a good option is
--     Empty -> "- "
--     Snake -> "0 "
--     SnakeHead -> "$ "
--     Apple -> "X "
--   In other to avoid shrinking, I'd recommend to use some character followed by an space.
--todo: unicode boxes
ppCell :: CellType -> String
ppCell cell = case cell of
    Empty     -> "-"
    Snake     -> "\x1b[32m"   ++ "T" ++ "\x1b[0m "
    SnakeHead -> "\x1b[1;32m" ++ "$" ++ "\x1b[0m " --colour SnakeHead green "\x1b[32m$\x1b[0m"
    Apple     -> "\x1b[31m"   ++ "X" ++ "\x1b[0m "

-- | convert the RenderState in a String ready to be flushed into the console.
--   It should return the Board with a pretty look. If game over, return the empty board.

-- render' :: BoardInfo -> RenderState -> String
-- render' boardInfo@(BoardInfo _height width) (RenderState _ True) =
--     insertAtN (2*width) '\n' (concatMap ppCell (emptyGrid boardInfo)) ++ ['\n'] --todo refactor
-- render' (BoardInfo _height width) (RenderState board _) =
--     insertAtN (2*width) '\n' (concatMap ppCell board) ++ ['\n']
--         -- fmap ppCell board
--         -- rowEnds = [(row, width) | row <- [1..height]] --TODO: Simplify

render :: BoardInfo -> RenderState -> String
-- render2 boardInfo@(BoardInfo _height width) (RenderState _ True) =
--     insertAtN (2*width) '\n' (concatMap ppCell (emptyGrid boardInfo)) ++ ['\n'] --todo refactor
render (BoardInfo height width) (RenderState board _) =
    let condition :: (Point, CellType) -> String
        condition (index, cell)
            | index `elem` [(row, width) | row <- [1..height]] = ppCell cell ++ ['\n']
            | otherwise = ppCell cell
        board' = update board condition 
    in concat board' --concatMap condition (assocs board)

-- todo: haskell function, update, that operates on array's index and value, for updating
-- that is, a fmap over both indexer and element...
-- todotodo: make function and contribute to ghc...!
-- like with hashmaps...
-- type signature:
-- update Idx i => (Array i e) --> (i --> e --> t) --> (Array i t)
-- TODO: #type maniulations visualisations blog (concat map, fmap...)

update :: Ix i => Array i e -> ((i, e) -> t) -> Array i t
update a f = listArray (bounds a) (map f (assocs a)) --warning: doesn't enforce size... --or with //
--uncurry/curry if using i -> e -> t

update2 :: Ix i => Array i e -> ((i, e) -> t) -> Array i t --forall i e t.
-- update2 a f = a // fmap f (assocs a) --can't do incremental updates, if you're changing the type, need a variable foldl' function!!
update2 a f = array (bounds a) (zip (indices a) (fmap f (assocs a)))

-- update3 a f = foldl condition (assocs a)

-- Helper function to insert a newline at every n-th character in the board-string (end of row)
-- don't like this... (should be able to flatMap/concatMap etc...)
insertAtN :: Int -> t -> [t] -> [t]
insertAtN n y = intercalate [y] . groups n
  where groups n = takeWhile (not . null) . unfoldr (Just . splitAt n)

{-
This is a test for render. It should return:
"- - - - \n- 0 $ - \n- - - X \n"

Notice, that this depends on what you've chosen for ppCell
-}
-- >>> board = listArray ((1,1), (3,4)) [Empty, Empty, Empty, Empty, Empty, Snake, SnakeHead, Empty, Empty, Empty, Empty, Apple]
-- >>> board_info = BoardInfo 3 4
-- >>> render_state = RenderState board False
-- >>> render board_info render_state
-- "- - - - \n- 0 $ - \n- - - X \n"
