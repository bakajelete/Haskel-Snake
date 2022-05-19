{-# LANGUAGE NamedFieldPuns #-}

module Snake where

import Data.List
import System.Random

data Dir = UP | DOWN | LEFT | RIGHT deriving (Eq, Ord) --Letrehozza az irany tipust
type Pos = (Int, Int) --Pozicio tipus
type Snake = [Pos] --Minden egyes idopillanatban a snake elhelyezkedeseinek a lista beli ertekei

data Cell = FREE | WALL deriving (Eq) --Az egy cellak tipusai(fal vagy szabad)
type Grid = [[Cell]]

data State = RUNNING | WIN | LOST deriving (Eq) --A jatek allapotanak a tipusa (fut, nyert, vesztett)

data GameState = GameState  --Ez az osztaly szedi ossze a kulonfele altipusokat
  { snake :: Snake
  , dir :: Dir
  , food :: Pos
  , level :: Grid
  , rand :: StdGen
  , state :: State
  }

getCell :: Grid -> Pos -> Cell --Mehatarozza adott kordinataju cellak tipusat, hogy utkozeskor tudja, hogy game over vagy nem 
getCell grid (x, y) = grid !! x !! y

isRunning :: GameState -> Bool --Megadja, hogy a jatek meg folyamatban van-e
isRunning GameState { state }
  | state == RUNNING = True
  | otherwise = False

initialGameState :: Grid -> Snake -> Dir -> GameState --A jatek inditasakor beallitja a dolgokat
initialGameState level snake dir  
  = generateFood $ GameState --letrehozok egy gameState instanciat es azt elkuldi a generateFood fugvenynek
    { snake = snake
    , dir = dir
    , food = (0, 0) -- overwritten by generateFood
    , level = level
    , rand = mkStdGen 100
    , state = RUNNING
    }

dirToVec :: Dir -> Pos
dirToVec UP = (0, (-1)) -- Adott gomblenyomas ertelmezesehez hozzarendel egy mozdulasi vektort
dirToVec DOWN = (0, 1)
dirToVec LEFT = ((-1), 0)
dirToVec RIGHT = (1, 0)

move :: GameState -> GameState --Frissiti a belso allapotot periodikusan
move gameState@GameState { snake, dir, food, level } --Meghatarozza, hogy mozgaspillanat utan az uj cella milyen hatassal van (ures-> semmi)
  | not $ isRunning gameState = gameState            -- (fal vagy sajat farka-> halal) (gyumolcs -> no eggyet a kigyo)
  | getCell level newHead == WALL || newHead `elem` snake
    = gameState { state = LOST }
  | food == newHead = generateFood $ gameState { snake = longSnake }
  | otherwise = gameState { snake = init longSnake }
  where
    posAdd :: Pos -> Pos -> Pos
    posAdd (x, y) (a, b) = (x + a, y + b) --ket kordinata osszeadasa

    width = length $ head level --lekeri a kivalasztott palya hosszat es szelesseget
    height = length level
    posWrap (x, y) = ((x + width) `mod` width, ((y + height) `mod` height)) -- amikot nincs fal akkor atteleportal a masok felere

    currHead = head snake --lekeri a kigyo fejet hogy megtudja a, hogy mozgasko hova megy
    newHead = posWrap $ posAdd currHead $ dirToVec dir
    longSnake = newHead : snake --A snake noveles amikor kap gyumolcsot

generateFood :: GameState -> GameState -- A gyumolcs generalasa
generateFood gameState@GameState { rand, level, snake }
  | len == 0 = gameState { rand = rand1, food = ((-1), (-1)), state = WIN }
  | otherwise = gameState { rand = rand1, food = coord }
  where
    freeGridCoords :: [Pos] --az osszes szabad cella a jatekteren belul
    freeGridCoords = concat $ map toCoords $ zip [0 ..] $ map (elemIndices FREE) level
    toCoords (x, ys) = map ((,)x) ys

    possibleCoords :: [Pos] -- a szabad cellekbol kiveszi a kigyo altal foglatat
    possibleCoords = filter (not.(flip elem snake)) freeGridCoords
    len = length possibleCoords
    
    (idx, rand1) = randomR (0, len-1) rand --a random generalasa a gyumolcsnek, index generealodik 
    coord = possibleCoords !! idx --de csak a possibleCoords (lehetseges cellak) halmazabol

changeDir :: GameState -> Dir -> GameState --megvaltoztatom a kigyo haladasi iranyat a jatekallapoton belul
changeDir gameState dir = gameState { dir = dir }
