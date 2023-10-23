{-|
This module defines the logic of the game and the communication with the `Board.RenderState`
-}
module GameState where

-- These are all the import. Feel free to use more if needed.
import RenderState (BoardInfo (..), Point)
import qualified RenderState as Board
import Data.Sequence ( Seq(..))
import qualified Data.Sequence as S
import System.Random ( uniformR, StdGen, RandomGen (split))

-- The movement is one of this.
data Movement = North | South | East | West
    deriving (Show, Eq)

-- | The snakeSeq is a non-empty sequence. It is important to use precise types in Haskell
--   In first sight we'd define the snake as a sequence, but If you think carefully, an empty 
--   sequence can't represent a valid Snake, therefore we must use a non empty one.
--   You should investigate the Seq type in haskell and why it is a good option for our purpose.
-- TODO: Is there a way to enforce a non-empty sequence? that would msake pattern deconstruction significantly easier....
data SnakeSeq = SnakeSeq {snakeHead :: Point, snakeBody :: Seq Point}
    deriving (Show, Eq)

-- | The GameState represents all important bits in the game. The Snake, the apple, the current direction of movement and 
--   a random seed to calculate the next random apple.
data GameState = GameState
  { snakeSeq :: SnakeSeq
  , applePosition :: Point -- > refactor: Asset Location
  , movement :: Movement
  , randomGen :: StdGen
  }
  deriving (Show, Eq)

-- | This function should calculate the opposite movement.
oppositeMovement :: Movement -> Movement
oppositeMovement m = case m of
    North -> South
    South -> North
    East -> West
    West -> East

-- >>> oppositeMovement North == South
-- >>> oppositeMovement South == North
-- >>> oppositeMovement East == West
-- >>> oppositeMovement West == East
-- True
-- True
-- True
-- True


-- | Purely creates a random point within the board limits
--   You should take a look to System.Random documentation. 
--   Also, in the import list you have all relevant functions.
makeRandomPoint :: BoardInfo -> StdGen -> (Point, StdGen)
-- makeRandomPoint (BoardInfo height width) gen = ((rd1, rd2), gen'')
--     where (rd1, gen') = uniformR (height, width) gen
--           (rd2, gen'') = uniformR (height, width) gen'
--doWhile;  iter; unfoldr?
-- ^ my above definition doesn't work!!!
-- this would make for a **very** interesting technical discussion/blog post on PRNG (is it to do with threads?)
--todo: blog post on threading and event queues in Haskell
--todo: simplication: allow Snake to JUST be a Seq... and enforece non-emptiness (Maybe Seq a?)
---the ttypes start to model the domain, the code models/implements the logic/functions on the data domain
makeRandomPoint (BoardInfo h w) sg = (newPoint , g1')
  where (g1, g2)  = split sg
        (x, g1') = uniformR (1, h) g1
        (y, _g2') = uniformR (1, w) g2
        newPoint  = (x, y)
         --todo: neater way? need to pass gen of gen
         
{-
We can't test makeRandomPoint, because different implementation may lead to different valid result.
-}


-- | Check if a point is in the snake
inSnake :: Point -> SnakeSeq  -> Bool
inSnake pt (SnakeSeq head body)
    = pt == head || pt `elem` body --note: check

{-
This is a test for inSnake. It should return 
True
True
False
-}
-- >>> snake_seq = SnakeSeq (1,1) (Data.Sequence.fromList [(1,2), (1,3)])
-- >>> inSnake (1,1) snake_seq
-- >>> inSnake (1,2) snake_seq
-- >>> inSnake (1,4) snake_seq
-- True
-- True
-- False

-- | Calculates the new head of the snake. Considering it is moving in the current direction
--   Take into acount the edges of the board
nextHead :: BoardInfo -> GameState -> Point
nextHead (BoardInfo height width) (GameState (SnakeSeq snakeHead@(x, y) _) _ m _) = case m of
    --todo: ordering on tuples + smarter way... wrap with zip... --using ordering on tuples as shorthand?
    North -> if x - 1 >= 1      then (x - 1, y) else (height, y)
    South -> if x + 1 <= height then (x + 1, y) else (1, y)
    West  -> if y - 1 >= 1      then (x, y - 1) else (x, width)
    East  -> if y + 1 <= width  then (x, y + 1) else (x, 1)

{-
This is a test for nextHead. It should return
True
True
True
-}
-- >>> snake_seq = SnakeSeq (1,1) (Data.Sequence.fromList [(1,2), (1,3)])
-- >>> apple_pos = (2,2) 
-- >>> board_info = BoardInfo 4 4
-- >>> game_state1 = GameState snake_seq apple_pos West (Random.StdGen 1)
-- >>> game_state2 = GameState snake_seq apple_pos South (Random.mkStdGen 1)
-- >>> game_state3 = GameState snake_seq apple_pos North (Random.mkStdGen 1)
-- >>> nextHead board_info game_state1 == (1,4)
-- >>> nextHead board_info game_state2 == (2,1)
-- >>> nextHead board_info game_state3 == (4,1)
-- True
-- True
-- True


-- | Calculates a new random apple, avoiding creating the apple in the same place, or in the snake body
newApple :: BoardInfo -> GameState -> (Point, StdGen)
newApple boardInfo (GameState snake oldApplePos _ gen) =
    --note: make sure this returns the new generator too
    -- let (randomPt, gen') = makeRandomPoint boardInfo gen in 
    --     if not (randomPt `inSnake` snake) && randomPt /= oldApplePos then (randomPt, gen')
    --     else newApple boardInfo gs{randomGen = gen'} -- while
    let validPoint (randomPt, _) = not (randomPt `inSnake` snake) && randomPt /= oldApplePos
    in until validPoint (makeRandomPoint boardInfo . snd) (makeRandomPoint boardInfo gen)
    -- function *inside* random  state... --unpack tuple result instead of snd (aka 'naming' snd) (\(num,gen' )-> gen')

{- We can't test this function because it depends on makeRandomPoint -}


-- | Moves the snake based on the current direction. It sends the adequate RenderMessage
-- Notice that a delta board must include all modified cells in the movement.
-- For example, if we move between these two steps
--        - - - -          - - - -
--        - 0 $ -    =>    - - 0 $
--        - - - -    =>    - - - -
--        - - - X          - - - X
-- We need to send the following delta: [((2,2), Empty), ((2,3), Snake), ((2,4), SnakeHead)]
--
-- Another example, if we move between these two steps
--        - - - -          - - - -
--        - - - -    =>    - X - -
--        - - - -    =>    - - - -
--        - 0 $ X          - 0 0 $
-- We need to send the following delta: [((2,2), Apple), ((4,3), Snake), ((4,4), SnakeHead)]
-- 

{-TODO: Code design: if I update gen's state n times, I have to keep track of each time and update it to gen^'(n), which is terrible design
-- should be able to seamlessly pass it through
--(snakeHead S.<| snakeBody) is *suggestive notation* the old snakehead becomes part of the body
-- Suggestive API notation design
-}
-- TODO: check direction to know wherhher to take head or tail?
--todo: update snake, then send the snake's render as a de;ta to the board
move :: BoardInfo -> GameState -> (Board.RenderMessage , GameState)
move boardInfo gs@(GameState (SnakeSeq snakeHead snakeBody) apple m gen)
    | snakeHead' `elem` snakeBody = (Board.GameOver, gs)
    | snakeHead' == apple = --or snakeHead' == apple ? (subtle difference, update before/afyer, race condition...)
        let (apple', gen') = newApple boardInfo gs
            snake' = case snakeBody of
                S.Empty   -> SnakeSeq snakeHead' (S.singleton snakeHead)
                _         -> SnakeSeq snakeHead' (snakeHead S.:<| snakeBody) --or snakeBody@(h S.:<| t), make explicit non-empty by destructuring.., or _
            snakeDelta = [(snakeHead, Board.Snake), (apple', Board.Apple)]
        in (Board.RenderBoard (snakeHead'Delta : snakeDelta), GameState snake' apple' m gen') --want to name this gs', but how?
    | otherwise =
        let (snake', snakeDelta) = case snakeBody of
                S.Empty            -- isn't that just no update?? i.e. `snake' = snake {snakeHead=snakeHead'}` is the same
                    -> (SnakeSeq snakeHead' S.empty, [(snakeHead, Board.Empty)])
                (h S.:<| S.Empty) -- 1 element in tail, new is just new head + oldHead
                    -> (SnakeSeq snakeHead' (S.singleton snakeHead), [(snakeHead, Board.Snake), (h, Board.Empty)])
                (h S.:<| (mid S.:|> end))  --warning:: not actually sure what this means!!! drop last, move newHead in (xs=(h2)? is /2nd from first right?/, right?)
                    -> (SnakeSeq snakeHead' (snakeHead S.:<| h S.:<| mid), [(snakeHead, Board.Snake), (end, Board.Empty)])
        in (Board.RenderBoard (snakeHead'Delta : snakeDelta), GameState snake' apple m gen) --always update the snakehead regardless...
    where snakeHead' = nextHead boardInfo gs -- this happens every move increment, regardless of anything else
          snakeHead'Delta = (snakeHead', Board.SnakeHead)

{- todo: refactor -- use this abstraction:
data Render = Render Type DeltaBoard
data Render t = Render t Board.DeltaBoard -- whwre t :: Renderable (or, data Renderable = Snake | Apple, or Tyeclasses and instanceof)
e.g. Render Snake []
e.g. Render Apple
-}


-- -- tail' :: (Num a, Num b) => Seq (a, b) -> Seq (a, b)
-- tail' :: Seq a -> Seq a
-- tail' (_h S.:<| t) = t
-- tail' Empty = Empty--S.singleton (1,1)
--snake' = SnakeSeq snakeHead' (snakeHead S.<| (tail' snakeBody)) --old snakehead becomes part of body & drop last?

-- head' :: Seq a -> Maybe a
-- head' (h S.:<| _t) = Just h
-- head' Empty = Nothing --S.singleton (1,1)


{- This is a test for move. It should return

RenderBoard [((1,4),SnakeHead),((1,1),Snake),((1,3),Empty)]
RenderBoard [((2,1),SnakeHead),((1,1),Snake),((3,1),Apple)] ** your Apple might be different from mine
RenderBoard [((4,1),SnakeHead),((1,1),Snake),((1,3),Empty)]

-}

-- >>> snake_seq = SnakeSeq (1,1) (Data.Sequence.fromList [(1,2), (1,3)])
-- >>> apple_pos = (2,1) 
-- >>> board_info = BoardInfo 4 4
-- >>> game_state1 = GameState snake_seq apple_pos West (System.Random.mkStdGen 1)
-- >>> game_state2 = GameState snake_seq apple_pos South (System.Random.mkStdGen 1)
-- >>> game_state3 = GameState snake_seq apple_pos North (System.Random.mkStdGen 1)
-- >>> fst $ move board_info game_state1
-- >>> fst $ move board_info game_state2
-- >>> fst $ move board_info game_state3
-- RenderBoard [((1,4),SnakeHead),((1,1),Snake),((1,3),Empty)]
-- RenderBoard [((2,1),SnakeHead),((1,1),Snake),((4,4),Apple)]
-- RenderBoard [((4,1),SnakeHead),((1,1),Snake),((1,3),Empty)]

