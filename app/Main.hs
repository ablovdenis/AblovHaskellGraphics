module Main where


import ToRightCoordSyst
import Transliteration

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

data World = World
  { winWidth  :: Float
  , winHeight :: Float
  , picturesDefault :: Bool -- Состояние рисунка, определяющее, что надо
                            -- отобразить: рисунок из методички или собственный.
  , proportionality :: Bool -- Делает изображение пропорциональным размеру окна,
                            -- когда равен True.
  }

initWidth :: Int
initWidth = 400
initHeigth :: Int
initHeigth = 300

initialWorld :: World
initialWorld = World (fromIntegral initWidth) (fromIntegral initHeigth)
                     True False

render :: World -> Picture
render world = 
  if picturesDefault world then -- Если picturesDefault == True, то отобразиться
                                -- кролик, иначе отобразится гриб.
    Color black $ Pictures [
      Line $ toCoords [
        (0.5,3.0), (1.0,4.5), (0.5,6.0), (0.5,7.5), (1.0,8.0), (1.5,8.0),
        (2.0,7.5), (1.5,6.0), (1.5,4.5), (3.0,4.5), (3.0,6.0), (2.5,7.5),
        (3.0,8.0), (3.5,8.0), (4.0,7.5), (4.0,6.0), (3.5,4.5), (4.0,3.0),
        (3.5,1.5), (2.5,1.0), (2.0,1.0), (1.0,1.5), (0.5,3.0)
      ],
      Line $ toCoords [
        (4.0,3.0), (5.5,3.5), (7.0,3.5), (7.5,2.5), (8.0,2.5), (8.0,2.0),
        (7.5,2.0), (7.5,0.5), (6.5,0.5), (6.5,1.0), (6.0,1.0), (6.0,0.5),
        (5.0,0.5), (5.0,1.0), (4.0,1.0), (4.0,0.5), (3.0,0.5), (3.0,1.0),
        (2.5,1.0), (2.5,0.5), (1.5,0.5), (1.5,1.25)
      ],
      Line $ toCoords [
        (1.5,3.5), (1.5,3.0), (2.0,3.0), (2.0,3.5), (1.5,3.5)
      ],
      Line $ toCoords [
        (2.5,3.5), (2.5,3.0), (3.0,3.0), (3.0,3.5), (2.5,3.5)
      ],
      Line $ toCoords [(1.0,5.5), (1,7)],
      Line $ toCoords [(3.5,5.5), (3.5,7.0)],
      Line $ toCoords [
        (2.0,2.5), (2.5,2.5), (2.25,2.0), (2.0,2.5)
      ]
    ]
  else
    Color red $ Pictures [
      Line $ toCoords [
        (1,0), (0.5,0.5), (1.25,0.5), (0.75,1), (1.25,0.75), (1.25,1.25),
        (1.5,0.75), (1.5,1.75), (1.75,0.75), (2,0.25), (2.5,0), (3,0.25),
        (3.25,0.75), (3.5,1.75), (3.5,0.75), (3.75,1.25), (3.75,0.75), (4.25,1),
        (3.75, 0.5), (4.5,0.5), (4,0), (1,0)
      ], -- Трава.

      Line $ toCoords [
        (1.75,0.75), (2,2), (1.5,2), (0.75,2.25), (0.25,2.75), (0.25,3.5), (0.75,4),
        (2,6.75), (2.25,7), (2.75,7), (3, 6.75), (4.25,4), (4.75,3.5), (4.75,2.75),
        (4.25,2.25), (3.5,2), (3,2), (3.25,0.75)
      ], -- Внешний контур гриба.

      Line $ toCoords [
        (1.917,2.25), (1.5,2.25), (1,2.5), (0.5,3), (1,3.5), (1.75,3.75), (3.25,3.75),
        (4,3.5), (4.5,3), (4,2.5), (3.25,2.25), (3.083,2.25)
      ], -- Внутреннее кольцо гриба.

      Line $ toCoords [
        (2,2), (1.75,2.75), (2.25,3), (2.75,3), (3.25,2.75), (3,2)
      ], -- Верхняя часть ножки гриба.

      Line $ toCoords [(1.75,2.75), (0.75,3.25)],
      Line $ toCoords [(2,2.875), (1.5,3.667)],
      Line $ toCoords [(2.25,3), (2,3.75)],
      Line $ toCoords [(2.5,3), (2.5,3.75)],
      Line $ toCoords [(2.75,3), (3,3.75)],
      Line $ toCoords [(3,2.875), (3.5,3.667)],
      Line $ toCoords [(3.25,2.75), (4.25,3.25)], -- Лучи, выходящие от верхней части ножки гриба.
      
      Line $ toCoords [(0.25,3), (0.5,3.25), (0.25,3.5)],
      Line $ toCoords [(0.5,3.75), (1,3.75), (1.25,4), (1.25,4.25), (1,4.5)],
      Line $ toCoords [(1.75,6.25), (2,6), (2.25,6), (2.5,6.25),
                       (2.5,6.5), (2.25,6.75), (2,6.75)],
      Line $ toCoords [(4.25,4), (4,3.75), (4.25,3.5), (4.75,3.5)],
      Line $ toCoords [(1.75,3.75), (1.5,4), (1.75,4.25),
                       (2,4.25), (2.25,4), (2,3.75)], -- Пятна около контура.

      Line $ toCoords [(1.5,4.75), (1.75,4.5), (2,4.5), (2.25,4.75), (2.25,5),
                       (2,5.25), (1.75,5.25), (1.5,5), (1.5,4.75)],
      Line $ toCoords [(2.5,5.5), (2.75,5.25), (3,5.25), (3.25,5.5), (3.25,5.75),
                       (3,6), (2.75,6), (2.5,5.75), (2.5,5.5)],
      Line $ toCoords [(2.75,4.75), (3,5), (3.5,5), (3.75,4.75), (3.75,4.25),
                       (3.5,4), (3,4), (2.75,4.25), (2.75,4.75)] -- Пятна в середине.
    ]
    where
      w = winWidth world
      h = winHeight world
      minHW = min w h
      sc = minHW / 9
      toCoords = if proportionality world then
                   map $ (toRightCoord w h .
                          (\ (x,y) -> (x * w / 9, y * h / 9)))
                 else
                   map $ ((\ (x,y) -> (x, y + h - minHW)) .
                          toRightCoord w h .
                          (\ (x,y) -> (x * sc, y * sc)))

handleEvent :: Event -> World -> World
handleEvent (EventResize (w, h))
            (World _ _ picD prop) =
  World (fromIntegral w) (fromIntegral h) picD prop
handleEvent (EventKey (Char 'n') Down _ _) -- Если произошло нажатие клавиши "n"
                                           -- (раскладка в момент нажатия должна быть
                                           -- английской), то сменится рисунок.
            (World w h picD prop) =
  World w h (not picD) prop
handleEvent (EventKey (Char 'm') Down _ _) -- Если произошло нажатие клавиши "m"
                                           -- (раскладка в момент нажатия должна быть
                                           -- английской), то сменится режим
                                           -- отображения.
            (World w h picD prop) =
  World w h picD $ not prop
handleEvent _ world = world

update :: p -> world -> world
update _ world = world

myAquamarine :: Color
myAquamarine = makeColorI 127 255 212 0

main :: IO ()
main = do
  play (InWindow (transliteration "MyForm")
                 (initWidth, initHeigth) (0, 0))
       myAquamarine 60 initialWorld render handleEvent update
