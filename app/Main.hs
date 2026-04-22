module Main where



import App.HandleEvent (handleEvent)
import App.Render (render)
import App.Update (update)
import App.World (World(..), ProjType(..))

import Math.Vector (Vec3(..))
import Math.Matrix (create_diag_matrix)

import Parsing.BtnOpen_click (btnOpen_click)

import Utils.Transliteration (transliteration)

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game


-- БАЗОВЫЕ НАСТРОЙКИ ПРИЛОЖЕНИЯ:
initWidth :: Int -- Ширина окна.
initWidth = 990

initHeigth :: Int -- Высота окна.
initHeigth = 500

-- Отступы рамки, ограничивающей изображение:
initLeft :: Int
initLeft = 30

initRight :: Int
initRight = 100

initTop :: Int
initTop = 20

initBottom :: Int
initBottom = 50

-- Фоновый цвет:
backgroundColor :: Color
backgroundColor = makeColorI 127 255 212 0

-- (относительный) Путь к файлу:
pathReadFile :: String
pathReadFile = "Files/Geometric3D-2.txt"

-- Название окна приложения при запуске:
window_title :: String
window_title = "MyForm"
-- КОНЕЦ БАЗОВЫх НАСТРОЕК.


initialWorldFunc :: [String] -> World
initialWorldFunc data_ =
  btnOpen_click
    (World
      initWidth_Float initHeigth_Float
      []
      (create_diag_matrix 1)
      (Vec3 0 0 0) (Vec3 0 0 0) (Vec3 0 0 0)
      0 0 0 0 0 0 0 0 0 0 0 0 0
      Ortho
      (create_diag_matrix 1)
      initLeft initRight initTop initBottom)
      data_
  where
    initWidth_Float = fromIntegral initWidth :: Float
    initHeigth_Float = fromIntegral initHeigth :: Float


main :: IO ()
main = do
  file <- readFile pathReadFile
  playIO (InWindow (transliteration window_title)
                 (initWidth, initHeigth) (0, 0))
         backgroundColor 60 (initialWorldFunc (lines file))
         render handleEvent update
