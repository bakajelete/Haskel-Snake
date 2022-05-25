-- stack --resolver lts --install-ghc runghc --package gloss --package random

{-# LANGUAGE NamedFieldPuns #-}

module Main where

import Snake
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Data.List
import Debug.Trace

data GUIState  --Ez az osztaly tartalmazza az jatek kulonfele funkciok allapotat: jatekallapot, palyakivalasztas
  = Game
    { gameState :: GameState
    }
  | LevelSelect
    { levels :: [(Grid, Snake, Dir)]
    , idx :: Int
    }

changeLevel :: GUIState -> Int -> GUIState
changeLevel levelSelect@LevelSelect { idx, levels } offs --A levelselet allapotban megvaltoztaja, hogy mi van kivalasztva
  = levelSelect { idx = (idx + offs + count) `mod` count }
  where
    count = length levels

initialGuiState :: GUIState --Megadja az alkalmazas kezdeti allapotat, a jetek alindulasakok a level select allapotba visz, ott ki lehet valasztani a palyay es utanna kezdodik a jatek
initialGuiState = LevelSelect levels 0
  where
    levels = [
        (readLevel $ [
          "##############",
          "#            #",
          "#            #",
          "#            #",
          "#            #",
          "#            #",
          "#            #",
          "#            #",
          "#            #",
          "#            #",
          "#            #",
          "#            #",
          "#            #",
          "#            #",
          "#            #",
          "#            #",
          "#            #",
          "#            #",
          "#            #",
          "#            #",
          "#            #",
          "#            #",
          "#            #",
          "#            #",
          "#            #",
          "#            #",
          "#            #",
          "#            #",
          "#            #",
          "#            #",
          "#            #",
          "#            #",
          "##############"
        ], [(2, 2)], DOWN),
        (readLevel $ [
          "#            #",
          "#            #",
          "#            #",
          "#            #",
          "#            #",
          "#            #",
          "#            #",
          "#            #",
          "#            #",
          "#            #",
          "#            #",
          "#            #",
          "#            #",
          "#            #",
          "#            #",
          "#            #",
          "#            #",
          "#            #",
          "#            #",
          "#            #",
          "#            #",
          "#            #",
          "#            #",
          "#            #",
          "#            #",
          "#            #",
          "#            #",
          "#            #",
          "#            #",
          "#            #",
          "#            #",
          "#            #",
          "#            #"
        ], [(2, 2)], DOWN),
        (readLevel $ [
          "#################", --a bal felso fal kocka kordinatja a (0,0)
          "#               #",
          "#       #       #",
          "#       #       #",
          "#     #####     #",
          "#       #       #",
          "#       #       #",
          "#               #",
          "#################"
        ], [(2, 2)], DOWN), --kezdeti pozicio es utanna milyen iranyba indul
        (readLevel $ [
          "########################################",
          "#                                      #",
          "#                                      #",
          "#                                      #",
          "#                                      #",
          "#                                      #",
          "#                                      #",
          "#                                      #",
          "#                                      #",
          "#                                      #",
          "#                                      #",
          "#                                      #",
          "#                                      #",
          "#                                      #",
          "#                                      #",
          "#                                      #",
          "#                                      #",
          "#                                      #",
          "#                                      #",
          "#                                      #",
          "#                                      #",
          "#                                      #",
          "#                                      #",
          "#                                      #",
          "#                                      #",
          "#                                      #",
          "#                                      #",
          "#                                      #",
          "########################################"
        ],[(2,2)], DOWN)
      ]
    readLevel :: [String] -> Grid --Az adott tipusu cellak ertelmezese a kirajzolashoz
    readLevel = map (map readCell)
    readCell ' ' = FREE
    readCell '#' = WALL

toFloat :: (Integral a, Num b) => (a, a) -> (b, b)
toFloat (x, y) = (fromIntegral x, fromIntegral y)

windowSize = (640, 480) --Alapertelmezett ablak meret

window :: Display
window = InWindow "Haskell Snake Game" windowSize (100, 100)

background :: Color
background = white

render :: GUIState -> Picture  --Ez a fo rajzolo fuggveny, meghivja a tobbi alegyseg kirajzolasanak fuggvenyet
render LevelSelect { levels, idx } = renderGrid grid snake []
  where
    (grid, snake, food) = levels !! idx
render Game { gameState } = renderGame gameState
  where
    renderGame GameState { snake, food, level }
      = pictures $ (renderGrid level snake [food]):gameOverScreen
    gameOverScreen
      = if not $ isRunning gameState
      then (color blue $ 
            translate (-200) (0) $ 
            scale 0.5 0.5 $ 
            text "GAME OVER"):[]
      else []

renderGrid :: Grid -> Snake -> [Pos] -> Picture --Ez kirajzolja a palyat, magat a kigyo
renderGrid grid snake food
  = pictures $
    map (convertToPicture black.swapCoords) wallCoords ++
    map (convertToPicture blue) snake ++
    map (convertToPicture red) food
  where
    wallCoords :: [Pos]  --Lekeri, hogy melyik palya van kivalasztva
    wallCoords = concat $ map toCoords $ zip [0 ..] $ map (elemIndices WALL) grid
    toCoords (x, ys) = map ((,)x) ys --Atvaltja a kivalasztott koordinatak ak a rajzolashoz
    swapCoords (x, y) = (y, x)
  
    convertToPicture :: Color -> (Int, Int) -> Picture --Adott tipusu kep cellakhoz a megfelelo szin tipust rendeli a kirajzolashoz
    convertToPicture color' (x, y) = fillRectangle color' (toFloat (x, y)) cellSizeF offsetF
    fillRectangle color' (tx, ty) (w, h) (ow, oh)
      = color color' $
        scale 1 (-1) $
        translate (tx * w + ow) (ty * h + oh) $
        rectangleSolid w h

    (winWidth, winHeight) = windowSize  --Az eppen kivalsztott palyat igazitja a lejatszsai ablak mereteihez a lejatszaskor
    winW2 = winWidth `div` 2
    winH2 = winHeight `div` 2
  
    width = length $ head grid
    height = length grid
    
    wView = winWidth `div` width  --A kepernyoablak meretehez itt alakatja at a palyak meretet
    hView = winHeight `div` height
  
    cellSize = min wView hView
    offsetX = ((-cellSize) * (width - 1)) `div` 2 --Itt tolodik el a kirajzolas amikor kepernyonezet valtozik
    offsetY = ((-cellSize) * (height - 1)) `div` 2

    cellSizeF = toFloat (cellSize, cellSize)
    offsetF = toFloat (offsetX, offsetY)

update :: Float -> GUIState -> GUIState --Periodikusan frissiti a jatek allapott
update _ guiState@Game { gameState } --Ez a parameter game tipusu kell legyen es benne a van a teljes game objektum
  = if isRunning gameState
    then Game { gameState = move gameState }
    else guiState

update _ guiState = guiState 

handleKeys :: Event -> GUIState -> GUIState --A gomblenyomasok esemenyeinek a lekezelese

-- gameplay
handleKeys (EventKey (SpecialKey KeyLeft ) Down _ _) guiState@Game { gameState } --lekeri a jatek allapotat es annak megfelelo reakciot ad a gomblenyomasra
  = guiState { gameState = changeDir gameState LEFT }                            -- palya valasztas kozben a palyakat fogja valtogatni, jatek kozben a snaket fogja iranyitani
handleKeys (EventKey (SpecialKey KeyRight) Down _ _) guiState@Game { gameState }
  = guiState { gameState = changeDir gameState RIGHT } 
handleKeys (EventKey (SpecialKey KeyUp   ) Down _ _) guiState@Game { gameState }
  = guiState { gameState = changeDir gameState UP } 
handleKeys (EventKey (SpecialKey KeyDown ) Down _ _) guiState@Game { gameState }
  = guiState { gameState = changeDir gameState DOWN }
handleKeys (EventKey (SpecialKey KeySpace) Down _ _) guiState@Game { gameState } -- A space lenyomasara lehet kivalasztani a palyat a jatek elottes 
  = if not $ isRunning gameState                                                 -- halolkor visszadoba a level selectbe es mefint spacre lehet kezdeni
    then initialGuiState
    else guiState

-- level select
handleKeys (EventKey (SpecialKey KeyRight) Down _ _) guiState@LevelSelect{} --A palyak kozott lehet valtogatni, csak a righ es left-el
  = changeLevel guiState 1
handleKeys (EventKey (SpecialKey KeyLeft) Down _ _) guiState@LevelSelect{}
  = changeLevel guiState (-1)
handleKeys (EventKey (SpecialKey KeySpace) Down _ _) guiState@LevelSelect { idx, levels }
  = Game { gameState = initialGameState grid snake dir } --az implementacio az indulas mechanizmusra
  where
    (grid, snake, dir) = levels !! idx

handleKeys _ gameState = gameState -- A handlekeys is figyeli, hogy a jatek allapot miyen, es ciklikusan keri az informaciot

main :: IO () --stack Main.hs 
main = play window background 3 initialGuiState render handleKeys update --glos konyvtarbol jon a metodus, ez elintditja 
                            -- egy masodpercenkent hanyszor legyen update, jatek sebessege 
