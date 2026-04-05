module Main where



import App.HandleEvent (handleEvent)
import App.Render (render)
import App.Update (update)
import App.World (World(..))

import Parsing.BtnOpen_click (btnOpen_click)

import Utils.Transliteration (transliteration)

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game


initWidth :: Int
initWidth = 990


initHeigth :: Int
initHeigth = 500

initLeft :: Int
initLeft = 30

initRight :: Int
initRight = 100

initTop :: Int
initTop = 20

initBottom :: Int
initBottom = 50

initialWorldFunc :: [String] -> World
initialWorldFunc data_ =
  btnOpen_click (initWidth_float, initHeigth_float)
                (initLeft, initRight, initTop, initBottom)
                data_
  where
    initWidth_float = fromIntegral initWidth :: Float
    initHeigth_float = fromIntegral initHeigth :: Float


myAquamarine :: Color
myAquamarine = makeColorI 127 255 212 0


main :: IO ()
main = do
  file <- readFile $ "Files/MyPicture.txt"
  -- let initW = initialWorldFunc (lines file)
  -- print initW
  playIO (InWindow (transliteration "MyForm")
                 (initWidth, initHeigth) (0, 0))
         myAquamarine 60 (initialWorldFunc (lines file)) render handleEvent update