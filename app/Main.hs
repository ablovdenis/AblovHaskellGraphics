module Main where



import App.HandleEvent (handleEvent)
import App.Render (render)
import App.Update (update)
import App.World (World(..))

import Math.Matrix (create_diag_matrix)

import Parsing.BtnOpen_click (btnOpen_click)

import Utils.Transliteration (transliteration)

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game


initWidth :: Int
initWidth = 400


initHeigth :: Int
initHeigth = 300


initialWorldFunc :: [String] -> World
initialWorldFunc data_ = World (fromIntegral initWidth) (fromIntegral initHeigth)
                               mp_list (create_diag_matrix 1) fr
  where (fr, mp_list) = btnOpen_click data_


myAquamarine :: Color
myAquamarine = makeColorI 127 255 212 0


main :: IO ()
main = do
  file <- readFile $ "Files/Hare.txt"
  playIO (InWindow (transliteration "MyForm")
                 (initWidth, initHeigth) (0, 0))
         myAquamarine 60 (initialWorldFunc (lines file)) render handleEvent update