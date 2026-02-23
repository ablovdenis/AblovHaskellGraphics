module Main where

import Transliteration (transliteration)
import ThickLine
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

data World = World
  { winWidth  :: Float
  , winHeight :: Float
  }

initWidth :: Int
initWidth = 400
initHeigth :: Int
initHeigth = 300

initialWorld :: World
initialWorld = World (fromIntegral initWidth) (fromIntegral initHeigth)

render :: World -> Picture
render world = 
  Pictures [
    Color red $ thickLine 6 (- wDiv2, hDiv2) (wDiv2, - hDiv2),
    Color blue $ thickLine 10 (-wDiv2 + 90, hDiv2 - 50) (wDiv2, hDiv2 - 80),
    Color black $ Translate (- wDiv2 + 40) (hDiv2 - 100) $
                Scale (0.1) (0.1) $ Text $ transliteration "Надпись на форме",
    Color myGreen $ thickBrokenLine 5 [(-wDiv2, hDiv2 - hDiv3),
                                       (-wDiv2 + wDiv3, -hDiv2),
                                       (wDiv2, -hDiv2 + hDiv3),
                                       (wDiv2 - wDiv3, hDiv2),
                                       (-wDiv2, hDiv2 - hDiv3)]
  ]
    where
      w = winWidth world
      h = winHeight world
      wDiv2 = w / 2
      hDiv2 = h / 2
      wDiv3 = w / 3
      hDiv3 = h / 3
      myGreen = makeColorI 0 128 0 255

handleEvent :: Event -> World -> World
handleEvent (EventResize (w, h)) _ = World (fromIntegral w) (fromIntegral h)
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

-- Документация по библиотеке Gloss:
--     https://hackage.haskell.org/package/gloss-1.13.2.2/docs/


-- Краткий гайд по функциям:

-- Picture - это тип данных, отвечающий за отображение графических элементов на экране.
-- Чтоб создать один графический элемент, используйте соответствующий конструктор или
-- его псевдоним.
-- Чтоб создать несколько графич. элементов, используйте конструктор Pictures [Picture].
-- Конструкторы графических элементов смотреть в документации.

-- play :: Display -> Color -> Int -> world -> (world -> Picture) ->
--         (Event -> world -> world) -> (Float -> world -> world) -> IO ()
-- Это игровой режим, позволяющий самостоятельно управлять вводимыми данными.
-- Я выбрал play вместо display, потому что в play есть обработка клавиш и мыши.
-- Единственный минус play - нельзя автоматически масштабировать изображение.

-- Что значат его аргументы:

-- Display - режим отображения.
-- Описывает, как "Gloss" должен отображать свои выходные данные.
-- Либо исполбзуется полноэкранный режим (FullScreen), либо
-- с указанным названием, начальными размером и координатами:
-- (InWindow String (Int, Int) (Int, Int))

-- Color - цвет фона.
-- Можно указать либо через встроенные цвета (например, red, green), а можно через
-- функции, создающие цвета (например, makeColorI, принимающий 4 целых числа:
-- числа RGBA от 0 до 255 каждый).

-- Int - количество шагов моделирования, которые необходимо выполнить за
-- каждую секунду реального времени (условно говоря, это FPS).

-- world - создаваемый мир. Это некоторый тип данных (может быть даже пользовательским),
-- с которой могут работать следующие функции.

-- (world -> Picture) - функция для преобразования окружающего мира в картинку.
-- Это функция, которую надо написать самостоятельно. Она на вход принимает значение
-- типа world, обрабатывает его и возвращает какое-то изображение (Picture).

-- (Event -> world -> world) - функция для обработки входных событий.
-- Считывает клавиши/мышь/размер окна в данный момент и на основе прежнего значения
-- типа world создаёт и передаёт новое значение типа world.

-- (Float -> world -> world) - эта функция вызывается автоматически каждый кадр
-- (с частотой, указанной в параметре fps). Она получает время в секундах, прошедшее
-- после рендеринга предыдущего кадра, а также значение типа world. Возвращает новое
-- значение типа world.
-- Функция нужна для создания изменений в "игре". Т.к. мы делаем просто изображение, то
-- эта функция просто возвращает прежнее значение типа world.
-- Первый аргумент равен 3-ему аргументу функции "play".