module Main where

import Data.Bifunctor (first, second)
import Data.Ix (range)
import System.IO (BufferMode(..), hSetBuffering, hSetEcho, stdin)

data Direction = L | R | U | D
     deriving Show

data Action = Move Direction | Pass | Quit
     deriving Show

data ObjectKind = Player | Wall | Box
     deriving (Show, Eq)

type Object = (ObjectKind, Position)
type World = [Object]

type Position = (Int, Int)

startingObjects :: World
startingObjects = [
		(Wall, (0,0)),
		(Box, (2, 0)),
		(Box, (3, 0)),
		(Box, (5, 0)),
		(Box, (7, 0)),
		(Player, (8, 0)),
		(Box, (8, -1)),
		(Wall, (9, 0))
		]

main :: IO ()
main =
     -- Immediately consume input (doesn't require entering lines)
     hSetBuffering stdin NoBuffering >>
     -- Don't echo input back to the interface
     hSetEcho stdin False >>
     -- start the game
     loop startingObjects

invDir :: Direction -> Direction
invDir L = R
invDir R = L
invDir U = D
invDir D = U

renderWorldTile :: [ObjectKind] -> Char
renderWorldTile [] = ' '
renderWorldTile [Player] = '@'
renderWorldTile [Wall] = '#'
renderWorldTile [Box] = '.'
renderWorldTile _ = '?'

showWorld :: World -> String
showWorld world = unlines $ renderRow . yRow <$> yVals
  where
    renderRow objs = renderWorldTile <$> objs
    yRow y = (\x -> objectsAtPosition world (x, y)) <$> xVals
    xVals = [minX..maxX]
    yVals = [minY..maxY]
    minX = minimum $ fmap (fst . snd) world
    maxX = maximum $ fmap (fst . snd) world
    minY = minimum $ fmap (snd . snd) world
    maxY = maximum $ fmap (snd . snd) world

loop :: World -> IO ()
loop world = do
     putStrLn $ showWorld world

     inp <- getChar

     case parseInput inp of
     	  Nothing -> putStrLn "Invalid action" >> loop world
     	  Just Quit -> putStrLn "Goodbye!"
     	  Just action -> loop (processAction action world)


parseInput :: Char -> Maybe Action
parseInput 'a' = Just (Move L)
parseInput 'd' = Just (Move R)
parseInput 'w' = Just (Move U)
parseInput 's' = Just (Move D)
parseInput ' ' = Just Pass
parseInput 'q' = Just Quit
parseInput _ = Nothing

shiftPos :: Direction -> Int -> Position -> Position
shiftPos L n = shiftPos R (-n)
shiftPos R n = first (+ n)
shiftPos U n = shiftPos D (-n)
shiftPos D n = second (+ n)

objectsAtPosition :: World -> Position -> [ObjectKind]
objectsAtPosition objs pos = let objects = filter ((== pos) . snd) objs
		       	     	 in fst <$> objects

boxIsPushable :: Direction -> World ->  Position -> Bool
boxIsPushable dir world pos = 
	 (not $ Wall `elem` objectsAtPosition world (shiftPos dir 1 pos))
	 &&
	 ((not $ Box `elem` objectsAtPosition world (shiftPos dir 1 pos)) || boxIsPushable dir world (shiftPos dir 1 pos))

canMove :: Direction -> World -> Position -> Bool
canMove dir world  pos =
	 (not $ Wall `elem` objectsAtPosition world (shiftPos dir 1 pos))
	 &&
	 ((not $ Box `elem` objectsAtPosition world (shiftPos dir 1 pos))
	       || (boxIsPushable dir world (shiftPos dir 1 pos)))

isPushed :: Direction -> World -> Position -> Bool
isPushed dir world pos =
	  (Player `elem` objectsAtPosition world (shiftPos (invDir dir) 1 pos) ||
	    Box `elem` objectsAtPosition world (shiftPos (invDir dir) 1 pos) && isPushed dir world (shiftPos (invDir dir) 1 pos))
	  &&
	  boxIsPushable dir world pos

processAction :: Action -> World -> World
processAction action world = f <$> world
  where
    f :: Object -> Object
    f (Player, x) = case action of
      (Move dir) -> if canMove dir world x then (Player, shiftPos dir 1 x) else (Player, x)
      _ -> (Player, x)
    f (Box, x) = case action of
      (Move dir) -> if isPushed dir world x then (Box, shiftPos dir 1 x) else (Box, x)
      _ -> (Box, x)
    f o = o