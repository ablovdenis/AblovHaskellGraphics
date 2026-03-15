module Main where



import ToLeftCoordSyst
import Transliteration
import Figure
import ThickLine
import BtnOpen_click
import Vector
import Matrix
import TransForm

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game


data World = World
  { winWidth  :: Float -- Ширина окна.
  , winHeight :: Float -- Длина окна.
  , selected_image :: [MyPath] -- Хранит картинку в виде списка
                               -- конфигурационных объектов.
  , t_ :: Mat3 -- Матрица, в которой накапливаются все преобразования.
  , frame :: (Float, Float) -- Размер изображения (Vx и Vy).
  }


initWidth :: Int
initWidth = 400


initHeigth :: Int
initHeigth = 300


initialWorldFunc :: [String] -> World
initialWorldFunc data_ = World (fromIntegral initWidth) (fromIntegral initHeigth)
                               mp_list (create_diag_matrix 1) fr
  where (fr, mp_list) = btnOpen_click data_


render :: World -> IO Picture
render world = return $ Pictures [
  Color black $ Pictures (mypath_lst_to_picture_lst $ selected_image world),
  button
  ]
  where
    (vx_, vy_) = frame world

    wx_ = winWidth world
    wy_ = winHeight world

    button = Pictures [
      Color white $
            (Translate (wx_ / 2 - 30)
                       (wy_ / 2 - 20) $
                       rectangleSolid 40 20),
      
      Color blue $
            (Translate (wx_ / 2 - 30)
                       (wy_ / 2 - 20) $
                       rectangleWire 40 20),
      Color black $
            (Translate (wx_ / 2 - 45)
                       (wy_ / 2 - 25) $
                       (Scale 0.1 0.1 $ Text "Open"))
      ]

    aspectFig = vx_ / vy_ -- Пропорции рисунка.
    aspectForm = wx_ / wy_ -- Пропорции окна.

    -- Выбор значения масштабирования в зависимости от пропорций размеров
    -- рисунка и окна.
    s_ = if aspectFig < aspectForm then wy_ / vy_ else wx_ / vx_
    
    -- Смещение в положительную сторону по оси Oy после смены знака.
    ty_ = s_ * vy_

    toLeftCoordMat = toLeftCoord wx_ wy_

    -- Матрица с преобразованиями.
    -- Преобразования применяются справа налево.
    initT = toLeftCoordMat * t_ world * my_translate 0 ty_ * my_scale s_ (-s_)

    vector_transformation v = a
      where
        a_ = vec3_from_vec2 v 1
        a = normalize (product_of_matrix_by_vector initT a_)

    mypath_lst_to_picture_lst :: [MyPath] -> [Picture]
    mypath_lst_to_picture_lst [] = []
    mypath_lst_to_picture_lst (mp : lst) =
      (Color col $ thickBrokenLine thick lst_points) :
      mypath_lst_to_picture_lst lst
      where
        MyPath lst_vec2 col_vec3 thick = mp
        Vec3 re gr bl = col_vec3
        col = makeColor (re / 255) (gr / 255) (bl / 255) 1
        lst_points = map vector_transformation lst_vec2


handleEvent :: Event -> World -> IO World
handleEvent (EventResize (w, h))
            (World _ _ sel_im t_world fr) =
  return $
  World (fromIntegral w) (fromIntegral h) sel_im t_world fr

handleEvent (EventKey (Char 'q') Down _ _) -- Если произошло нажатие клавиши "q"
                                           -- (раскладка в момент нажатия должна быть
                                           -- английской), то произойдёт поворот
                                           -- относительно центра окна против часовой
                                           -- стрелки.
            (World w h sel_im t_world fr) =
  return $
  World w h sel_im t_2_translated fr
  where
    w_div_2 = w / 2
    h_div_2 = h / 2
    t_translated = my_translate (-w_div_2) (-h_div_2) * t_world
    t_rotated = my_rotate 0.01 * t_translated
    t_2_translated = my_translate w_div_2 h_div_2 * t_rotated

handleEvent (EventKey (Char 'e') Down _ _) -- Если произошло нажатие клавиши "q"
                                           -- (раскладка в момент нажатия должна быть
                                           -- английской), то произойдёт поворот
                                           -- относительно центра окна по часовой
                                           -- стрелке.
            (World w h sel_im t_world fr) =
  return $
  World w h sel_im t_2_translated fr
  where
    w_div_2 = w / 2
    h_div_2 = h / 2
    t_translated = my_translate (-w_div_2) (-h_div_2) * t_world
    t_rotated = my_rotate (-0.01) * t_translated
    t_2_translated = my_translate w_div_2 h_div_2 * t_rotated

handleEvent (EventKey (Char 'y') Down _ _)
            (World w h sel_im t_world fr) =
  return $
  World w h sel_im t_2_translated fr
  where
    w_div_2 = w / 2
    h_div_2 = h / 2
    t_translated = my_translate (-w_div_2) (-h_div_2) * t_world
    t_rotated = my_rotate 0.05 * t_translated
    t_2_translated = my_translate w_div_2 h_div_2 * t_rotated

handleEvent (EventKey (Char 'r') Down _ _)
            (World w h sel_im t_world fr) =
  return $
  World w h sel_im t_2_translated fr
  where
    w_div_2 = w / 2
    h_div_2 = h / 2
    t_translated = my_translate (-w_div_2) (-h_div_2) * t_world
    t_rotated = my_rotate (-0.05) * t_translated
    t_2_translated = my_translate w_div_2 h_div_2 * t_rotated

handleEvent (EventKey (Char 'w') Down _ _)
            (World w h sel_im t_world fr) =
  return $
  World w h sel_im t_translated fr
  where
    t_translated = my_translate 0 (-1) * t_world

handleEvent (EventKey (Char 's') Down _ _)
            (World w h sel_im t_world fr) =
  return $
  World w h sel_im t_translated fr
  where
    t_translated = my_translate 0 1 * t_world

handleEvent (EventKey (Char 'a') Down _ _)
            (World w h sel_im t_world fr) =
  return $
  World w h sel_im t_translated fr
  where
    t_translated = my_translate (-1) 0 * t_world

handleEvent (EventKey (Char 'd') Down _ _)
            (World w h sel_im t_world fr) =
  return $
  World w h sel_im t_translated fr
  where
    t_translated = my_translate 1 0 * t_world

handleEvent (EventKey (Char 't') Down _ _)
            (World w h sel_im t_world fr) =
  return $
  World w h sel_im t_translated fr
  where
    t_translated = my_translate 0 (-10) * t_world

handleEvent (EventKey (Char 'g') Down _ _)
            (World w h sel_im t_world fr) =
  return $
  World w h sel_im t_translated fr
  where
    t_translated = my_translate 0 10 * t_world

handleEvent (EventKey (Char 'f') Down _ _)
            (World w h sel_im t_world fr) =
  return $
  World w h sel_im t_translated fr
  where
    t_translated = my_translate (-10) 0 * t_world

handleEvent (EventKey (Char 'h') Down _ _)
            (World w h sel_im t_world fr) =
  return $
  World w h sel_im t_translated fr
  where
    t_translated = my_translate 10 0 * t_world

handleEvent (EventKey (Char 'z') Down _ _)
            (World w h sel_im t_world fr) =
  return $
  World w h sel_im t_2_translated fr
  where
    w_div_2 = w / 2
    h_div_2 = h / 2
    t_translated = my_translate (-w_div_2) (-h_div_2) * t_world
    t_scaled = my_scale 1.1 1.1 * t_translated
    t_2_translated = my_translate w_div_2 h_div_2 * t_scaled

handleEvent (EventKey (Char 'x') Down _ _)
            (World w h sel_im t_world fr) =
  return $
  World w h sel_im t_2_translated fr
  where
    sc = 1 / 1.1
    w_div_2 = w / 2
    h_div_2 = h / 2
    t_translated = my_translate (-w_div_2) (-h_div_2) * t_world
    t_scaled = my_scale sc sc * t_translated
    t_2_translated = my_translate w_div_2 h_div_2 * t_scaled

handleEvent (EventKey (Char 'i') Down _ _)
            (World w h sel_im t_world fr) =
  return $
  World w h sel_im t_2_translated fr
  where
    w_div_2 = w / 2
    h_div_2 = h / 2
    t_translated = my_translate (-w_div_2) (-h_div_2) * t_world
    t_scaled = my_scale 1.1 1 * t_translated
    t_2_translated = my_translate w_div_2 h_div_2 * t_scaled

handleEvent (EventKey (Char 'k') Down _ _)
            (World w h sel_im t_world fr) =
  return $
  World w h sel_im t_2_translated fr
  where
    sc = 1 / 1.1
    w_div_2 = w / 2
    h_div_2 = h / 2
    t_translated = my_translate (-w_div_2) (-h_div_2) * t_world
    t_scaled = my_scale sc 1 * t_translated
    t_2_translated = my_translate w_div_2 h_div_2 * t_scaled

handleEvent (EventKey (Char 'o') Down _ _)
            (World w h sel_im t_world fr) =
  return $
  World w h sel_im t_2_translated fr
  where
    w_div_2 = w / 2
    h_div_2 = h / 2
    t_translated = my_translate (-w_div_2) (-h_div_2) * t_world
    t_scaled = my_scale 1 1.1 * t_translated
    t_2_translated = my_translate w_div_2 h_div_2 * t_scaled

handleEvent (EventKey (Char 'l') Down _ _)
            (World w h sel_im t_world fr) =
  return $
  World w h sel_im t_2_translated fr
  where
    sc = 1 / 1.1
    w_div_2 = w / 2
    h_div_2 = h / 2
    t_translated = my_translate (-w_div_2) (-h_div_2) * t_world
    t_scaled = my_scale 1 sc * t_translated
    t_2_translated = my_translate w_div_2 h_div_2 * t_scaled

handleEvent (EventKey (Char 'u') Down _ _)
            (World w h sel_im t_world fr) =
  return $
  World w h sel_im t_2_translated fr
  where
    h_div_2 = h / 2
    t_translated = my_translate 0 (-h_div_2) * t_world
    t_mirrored = my_mirrorX * t_translated
    t_2_translated = my_translate 0 h_div_2 * t_mirrored

handleEvent (EventKey (Char 'j') Down _ _)
            (World w h sel_im t_world fr) =
  return $
  World w h sel_im t_2_translated fr
  where
    w_div_2 = w / 2
    t_translated = my_translate (-w_div_2) 0 * t_world
    t_mirrored = my_mirrorY * t_translated
    t_2_translated = my_translate w_div_2 0 * t_mirrored

handleEvent (EventKey (SpecialKey KeyEsc) Down _ _)
            (World w h sel_im _ fr) =
  return $
  World w h sel_im (create_diag_matrix 1) fr

handleEvent (EventKey (MouseButton LeftButton)
             Down _ (mouse_x, mouse_y))
             (World w h sel_im t_world fr) = do
  if (mouse_x >= w_div2 - 50) && (mouse_x <= w_div2 - 10) &&
     (mouse_y >= h_div2 - 30) && (mouse_y <= h_div2 - 10)
  then do
    putStrLn "Enter the file name with the extension (<name>.txt):"
    name <- inputFunc
    file <- readFile $ "Files/" ++ name
    return $
      let (fr_new, mp_list) = btnOpen_click (lines file)
      in World w h mp_list t_world fr_new
  else return $ World w h sel_im t_world fr
  where
    w_div2 = w / 2
    h_div2 = h / 2
    inputFunc = do
      str <- getLine
      if null str
      then do
        inputFunc
      else return str

handleEvent _ world = return world


update :: p -> world -> IO world
update _ world = return world


myAquamarine :: Color
myAquamarine = makeColorI 127 255 212 0


main :: IO ()
main = do
  file <- readFile $ "Files/Hare.txt"
  playIO (InWindow (transliteration "MyForm")
                 (initWidth, initHeigth) (0, 0))
         myAquamarine 60 (initialWorldFunc (lines file)) render handleEvent update