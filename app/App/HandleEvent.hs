module App.HandleEvent where



import App.World

import Math.Matrix (Mat3(..), transpose)
import Math.TransForm (my_translate, my_rotate, my_scale,
                       my_mirrorX, my_mirrorY)
import Math.Vector (Vec3(..))

import Parsing.BtnOpen_click (btnOpen_click)

import Graphics.Gloss.Interface.IO.Game


handleEvent :: Event -> World -> IO World
handleEvent (EventResize (w, h)) world =
  return $ change_T_world _t_new
           $ change_initT_world initT_new
           $ change_width_world width_new
           $ change_height_world height_new world
  where
    _vx = _vx_ world
    _vy = _vy_ world
    leftFloat = fromIntegral (left_ world) :: Float
    rightFloat = fromIntegral (right_ world) :: Float
    topFloat = fromIntegral (top_ world) :: Float
    bottomFloat = fromIntegral (bottom_ world) :: Float
    width_new = fromIntegral w :: Float
    height_new = fromIntegral h :: Float
    _wx = width_new - leftFloat - rightFloat
    _wy = height_new - topFloat - bottomFloat

    aspectFig = _vx / _vy

    aspectRect = _wx / _wy
    _t1 = my_translate (-_vx / 2) (-_vy / 2)
    _s = if aspectFig < aspectRect
          then _wy / _vy else _wx / _vx
    _s1 = my_scale _s _s
    _t2 = my_translate (leftFloat + _wx / 2) (bottomFloat + _wy / 2)
    initT_new = _t2 * _s1 * _t1
    _t = _t_ world

    inverse_matrix (Mat3 (Vec3 x1 y1 c1)
                         (Vec3 x2 y2 c2)
                         (Vec3 x3 y3 c3)) =
      transpose matrix_of_algebraic_complements_div_det
      where
        minor_x1 = y2 * c3 - y3 * c2
        minor_y1 = x2 * c3 - x3 * c2
        minor_c1 = x2 * y3 - x3 * y2
        
        minor_x2 = y1 * c3 - y3 * c1
        minor_y2 = x1 * c3 - x3 * c1
        minor_c2 = x1 * y3 - x3 * y1

        minor_x3 = y1 * c2 - y2 * c1
        minor_y3 = x1 * c2 - x2 * c1
        minor_c3 = x1 * y2 - x2 * y1

        det = x1 * minor_x1 - y1 * minor_y1 + c1 * minor_c1
        matrix_of_algebraic_complements_div_det =
          Mat3 (Vec3 (minor_x1 / det) (-minor_y1 / det) (minor_c1 / det))
               (Vec3 (-minor_x2 / det) (minor_y2 / det) (-minor_c2 / det))
               (Vec3 (minor_x3 / det) (-minor_y3 / det) (minor_c3 / det))

    _t_new = _t * inverse_matrix (initT_ world) * initT_new


handleEvent (EventKey (Char 'q') Down _ _) world =
                                           -- Если произошло нажатие клавиши "q"
                                           -- (раскладка в момент нажатия должна быть
                                           -- английской), то произойдёт поворот
                                           -- относительно центра окна против часовой
                                           -- стрелки.
  return $ change_T_world t_2_translated world
  where
    leftFloat = fromIntegral (left_ world) :: Float
    rightFloat = fromIntegral (right_ world) :: Float
    topFloat = fromIntegral (top_ world) :: Float
    bottomFloat = fromIntegral (bottom_ world) :: Float

    x_translate = (width_ world - rightFloat + leftFloat) / 2
    y_translate = (height_ world - topFloat + bottomFloat) / 2

    t_translated = my_translate (-x_translate) (-y_translate) * _t_ world
    t_rotated = my_rotate 0.01 * t_translated
    t_2_translated = my_translate x_translate y_translate
                     * t_rotated


handleEvent (EventKey (Char 'e') Down _ _)  world =
                                           -- Если произошло нажатие клавиши "q"
                                           -- (раскладка в момент нажатия должна быть
                                           -- английской), то произойдёт поворот
                                           -- относительно центра окна по часовой
                                           -- стрелке.
  return $ change_T_world t_2_translated world
  where
    leftFloat = fromIntegral (left_ world) :: Float
    rightFloat = fromIntegral (right_ world) :: Float
    topFloat = fromIntegral (top_ world) :: Float
    bottomFloat = fromIntegral (bottom_ world) :: Float

    x_translate = (width_ world - rightFloat + leftFloat) / 2
    y_translate = (height_ world - topFloat + bottomFloat) / 2

    t_translated = my_translate (-x_translate) (-y_translate) * _t_ world
    t_rotated = my_rotate (-0.01) * t_translated
    t_2_translated = my_translate x_translate y_translate
                     * t_rotated


handleEvent (EventKey (Char 'y') Down _ _) world =
  return $ change_T_world t_2_translated world
  where
    leftFloat = fromIntegral (left_ world) :: Float
    rightFloat = fromIntegral (right_ world) :: Float
    topFloat = fromIntegral (top_ world) :: Float
    bottomFloat = fromIntegral (bottom_ world) :: Float

    x_translate = (width_ world - rightFloat + leftFloat) / 2
    y_translate = (height_ world - topFloat + bottomFloat) / 2

    t_translated = my_translate (-x_translate) (-y_translate) * _t_ world
    t_rotated = my_rotate (-0.05) * t_translated
    t_2_translated = my_translate x_translate y_translate
                     * t_rotated


handleEvent (EventKey (Char 'r') Down _ _) world =
  return $ change_T_world t_2_translated world
  where
    leftFloat = fromIntegral (left_ world) :: Float
    rightFloat = fromIntegral (right_ world) :: Float
    topFloat = fromIntegral (top_ world) :: Float
    bottomFloat = fromIntegral (bottom_ world) :: Float

    x_translate = (width_ world - rightFloat + leftFloat) / 2
    y_translate = (height_ world - topFloat + bottomFloat) / 2

    t_translated = my_translate (-x_translate) (-y_translate) * _t_ world
    t_rotated = my_rotate 0.05 * t_translated
    t_2_translated = my_translate x_translate y_translate
                     * t_rotated


handleEvent (EventKey (Char 'w') Down _ _) world =
  return $ change_T_world t_translated world
  where
    t_translated = my_translate 0 1 * _t_ world


handleEvent (EventKey (Char 's') Down _ _) world =
  return $ change_T_world t_translated world
  where
    t_translated = my_translate 0 (-1) * _t_ world


handleEvent (EventKey (Char 'a') Down _ _) world =
  return $ change_T_world t_translated world
  where
    t_translated = my_translate (-1) 0 * _t_ world


handleEvent (EventKey (Char 'd') Down _ _) world =
  return $ change_T_world t_translated world
  where
    t_translated = my_translate 1 0 * _t_ world


handleEvent (EventKey (Char 't') Down _ _) world =
  return $ change_T_world t_translated world
  where
    t_translated = my_translate 0 10 * _t_ world


handleEvent (EventKey (Char 'g') Down _ _) world =
  return $ change_T_world t_translated world
  where
    t_translated = my_translate 0 (-10) * _t_ world


handleEvent (EventKey (Char 'f') Down _ _) world =
  return $ change_T_world t_translated world
  where
    t_translated = my_translate (-10) 0 * _t_ world


handleEvent (EventKey (Char 'h') Down _ _) world =
  return $ change_T_world t_translated world
  where
    t_translated = my_translate 10 0 * _t_ world


handleEvent (EventKey (Char 'z') Down _ _) world =
  return $ change_T_world t_2_translated world
  where
    leftFloat = fromIntegral (left_ world) :: Float
    rightFloat = fromIntegral (right_ world) :: Float
    topFloat = fromIntegral (top_ world) :: Float
    bottomFloat = fromIntegral (bottom_ world) :: Float

    x_translate = (width_ world - rightFloat + leftFloat) / 2
    y_translate = (height_ world - topFloat + bottomFloat) / 2

    t_translated = my_translate (-x_translate) (-y_translate) * _t_ world
    t_scaled = my_scale 1.1 1.1 * t_translated
    t_2_translated = my_translate x_translate y_translate
                     * t_scaled


handleEvent (EventKey (Char 'x') Down _ _) world =
  return $ change_T_world t_2_translated world
  where
    leftFloat = fromIntegral (left_ world) :: Float
    rightFloat = fromIntegral (right_ world) :: Float
    topFloat = fromIntegral (top_ world) :: Float
    bottomFloat = fromIntegral (bottom_ world) :: Float

    x_translate = (width_ world - rightFloat + leftFloat) / 2
    y_translate = (height_ world - topFloat + bottomFloat) / 2

    t_translated = my_translate (-x_translate) (-y_translate) * _t_ world
    c = 1 / 1.1
    t_scaled = my_scale c c * t_translated
    t_2_translated = my_translate x_translate y_translate
                     * t_scaled


handleEvent (EventKey (Char 'i') Down _ _) world =
  return $ change_T_world t_2_translated world
  where
    leftFloat = fromIntegral (left_ world) :: Float
    rightFloat = fromIntegral (right_ world) :: Float
    topFloat = fromIntegral (top_ world) :: Float
    bottomFloat = fromIntegral (bottom_ world) :: Float

    x_translate = (width_ world - rightFloat + leftFloat) / 2
    y_translate = (height_ world - topFloat + bottomFloat) / 2

    t_translated = my_translate (-x_translate) (-y_translate) * _t_ world
    t_scaled = my_scale 1.1 1 * t_translated
    t_2_translated = my_translate x_translate y_translate
                     * t_scaled


handleEvent (EventKey (Char 'k') Down _ _) world =
  return $ change_T_world t_2_translated world
  where
    leftFloat = fromIntegral (left_ world) :: Float
    rightFloat = fromIntegral (right_ world) :: Float
    topFloat = fromIntegral (top_ world) :: Float
    bottomFloat = fromIntegral (bottom_ world) :: Float

    x_translate = (width_ world - rightFloat + leftFloat) / 2
    y_translate = (height_ world - topFloat + bottomFloat) / 2

    t_translated = my_translate (-x_translate) (-y_translate) * _t_ world
    t_scaled = my_scale (1 / 1.1) 1 * t_translated
    t_2_translated = my_translate x_translate y_translate
                     * t_scaled


handleEvent (EventKey (Char 'o') Down _ _) world =
  return $ change_T_world t_2_translated world
  where
    leftFloat = fromIntegral (left_ world) :: Float
    rightFloat = fromIntegral (right_ world) :: Float
    topFloat = fromIntegral (top_ world) :: Float
    bottomFloat = fromIntegral (bottom_ world) :: Float

    x_translate = (width_ world - rightFloat + leftFloat) / 2
    y_translate = (height_ world - topFloat + bottomFloat) / 2

    t_translated = my_translate (-x_translate) (-y_translate) * _t_ world
    t_scaled = my_scale 1 1.1 * t_translated
    t_2_translated = my_translate x_translate y_translate
                     * t_scaled


handleEvent (EventKey (Char 'l') Down _ _) world =
  return $ change_T_world t_2_translated world
  where
    leftFloat = fromIntegral (left_ world) :: Float
    rightFloat = fromIntegral (right_ world) :: Float
    topFloat = fromIntegral (top_ world) :: Float
    bottomFloat = fromIntegral (bottom_ world) :: Float

    x_translate = (width_ world - rightFloat + leftFloat) / 2
    y_translate = (height_ world - topFloat + bottomFloat) / 2

    t_translated = my_translate (-x_translate) (-y_translate) * _t_ world
    t_scaled = my_scale 1 (1 / 1.1) * t_translated
    t_2_translated = my_translate x_translate y_translate
                     * t_scaled


handleEvent (EventKey (Char 'u') Down _ _) world =
  return $ change_T_world t_2_translated world
  where
    leftFloat = fromIntegral (left_ world) :: Float
    rightFloat = fromIntegral (right_ world) :: Float
    topFloat = fromIntegral (top_ world) :: Float
    bottomFloat = fromIntegral (bottom_ world) :: Float

    x_translate = (width_ world - rightFloat + leftFloat) / 2
    y_translate = (height_ world - topFloat + bottomFloat) / 2

    t_translated = my_translate (-x_translate) (-y_translate) * _t_ world
    t_mirrored = my_mirrorX * t_translated
    t_2_translated = my_translate x_translate y_translate
                     * t_mirrored


handleEvent (EventKey (Char 'j') Down _ _) world =
  return $ change_T_world t_2_translated world
  where
    leftFloat = fromIntegral (left_ world) :: Float
    rightFloat = fromIntegral (right_ world) :: Float
    topFloat = fromIntegral (top_ world) :: Float
    bottomFloat = fromIntegral (bottom_ world) :: Float

    x_translate = (width_ world - rightFloat + leftFloat) / 2
    y_translate = (height_ world - topFloat + bottomFloat) / 2

    t_translated = my_translate (-x_translate) (-y_translate) * _t_ world
    t_mirrored = my_mirrorY * t_translated
    t_2_translated = my_translate x_translate y_translate
                     * t_mirrored


handleEvent (EventKey (SpecialKey KeyEsc) Down _ _) world =
  return $ change_T_world (initT_ world) world

handleEvent (EventKey (MouseButton LeftButton)
             Down _ (mouse_x, mouse_y)) world = do
  if (mouse_x >= w_div_2 - 50) && (mouse_x <= w_div_2 - 10) &&
     (mouse_y >= h_div_2 - 30) && (mouse_y <= h_div_2 - 10)
  then do
    putStrLn "Enter the file name with the extension (<name>.txt):"
    name <- inputFunc
    data_ <- readFile $ "Files/" ++ name
    return $ btnOpen_click (width_ world, height_ world)
                           (left_ world, right_ world,
                            top_ world, bottom_ world)
                           (lines data_)
  else return $ world
  where
    w_div_2 = width_ world / 2
    h_div_2 = height_ world / 2
    inputFunc = do
      str <- getLine
      if null str
      then do
        inputFunc
      else return str


handleEvent _ world = return world