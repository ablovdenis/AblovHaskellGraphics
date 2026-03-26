module App.HandleEvent where



import App.World (World,
                  get_width,
                  get_height,
                  get_T_matrix,

                  change_width_world,
                  change_height_world,
                  change_mp_lst_world,
                  change_T_matrix_world,
                  change_frame_size_world)

import Math.Matrix (create_diag_matrix)
import Math.TransForm (my_translate, my_rotate, my_scale,
                       my_mirrorX, my_mirrorY)

import Parsing.BtnOpen_click (btnOpen_click)

import Graphics.Gloss.Interface.IO.Game


handleEvent :: Event -> World -> IO World
handleEvent (EventResize (w, h)) world =
  return $ (change_width_world (fromIntegral w) $ change_height_world (fromIntegral h) world)


handleEvent (EventKey (Char 'q') Down _ _) world =
                                           -- Если произошло нажатие клавиши "q"
                                           -- (раскладка в момент нажатия должна быть
                                           -- английской), то произойдёт поворот
                                           -- относительно центра окна против часовой
                                           -- стрелки.
  return $ change_T_matrix_world t_2_translated world
  where
    w_div_2 = get_width world / 2
    h_div_2 = get_height world / 2
    t_translated = my_translate (-w_div_2) (-h_div_2) * get_T_matrix world
    t_rotated = my_rotate 0.01 * t_translated
    t_2_translated = my_translate w_div_2 h_div_2 * t_rotated


handleEvent (EventKey (Char 'e') Down _ _)  world =
                                           -- Если произошло нажатие клавиши "q"
                                           -- (раскладка в момент нажатия должна быть
                                           -- английской), то произойдёт поворот
                                           -- относительно центра окна по часовой
                                           -- стрелке.
  return $ change_T_matrix_world t_2_translated world
  where
    w_div_2 = get_width world / 2
    h_div_2 = get_height world / 2
    t_translated = my_translate (-w_div_2) (-h_div_2) * get_T_matrix world
    t_rotated = my_rotate (-0.01) * t_translated
    t_2_translated = my_translate w_div_2 h_div_2 * t_rotated


handleEvent (EventKey (Char 'y') Down _ _) world =
  return $ change_T_matrix_world t_2_translated world
  where
    w_div_2 = get_width world / 2
    h_div_2 = get_height world / 2
    t_translated = my_translate (-w_div_2) (-h_div_2) * get_T_matrix world
    t_rotated = my_rotate 0.05 * t_translated
    t_2_translated = my_translate w_div_2 h_div_2 * t_rotated


handleEvent (EventKey (Char 'r') Down _ _) world =
  return $ change_T_matrix_world t_2_translated world
  where
    w_div_2 = get_width world / 2
    h_div_2 = get_height world / 2
    t_translated = my_translate (-w_div_2) (-h_div_2) * get_T_matrix world
    t_rotated = my_rotate (-0.05) * t_translated
    t_2_translated = my_translate w_div_2 h_div_2 * t_rotated


handleEvent (EventKey (Char 'w') Down _ _) world =
  return $ change_T_matrix_world t_translated world
  where
    t_translated = my_translate 0 (-1) * get_T_matrix world


handleEvent (EventKey (Char 's') Down _ _) world =
  return $ change_T_matrix_world t_translated world
  where
    t_translated = my_translate 0 1 * get_T_matrix world


handleEvent (EventKey (Char 'a') Down _ _) world =
  return $ change_T_matrix_world t_translated world
  where
    t_translated = my_translate (-1) 0 * get_T_matrix world


handleEvent (EventKey (Char 'd') Down _ _) world =
  return $ change_T_matrix_world t_translated world
  where
    t_translated = my_translate 1 0 * get_T_matrix world


handleEvent (EventKey (Char 't') Down _ _) world =
  return $ change_T_matrix_world t_translated world
  where
    t_translated = my_translate 0 (-10) * get_T_matrix world


handleEvent (EventKey (Char 'g') Down _ _) world =
  return $ change_T_matrix_world t_translated world
  where
    t_translated = my_translate 0 10 * get_T_matrix world


handleEvent (EventKey (Char 'f') Down _ _) world =
  return $ change_T_matrix_world t_translated world
  where
    t_translated = my_translate (-10) 0 * get_T_matrix world


handleEvent (EventKey (Char 'h') Down _ _) world =
  return $ change_T_matrix_world t_translated world
  where
    t_translated = my_translate 10 0 * get_T_matrix world


handleEvent (EventKey (Char 'z') Down _ _) world =
  return $ change_T_matrix_world t_2_translated world
  where
    w_div_2 = get_width world / 2
    h_div_2 = get_height world / 2
    t_translated = my_translate (-w_div_2) (-h_div_2) * get_T_matrix world
    t_scaled = my_scale 1.1 1.1 * t_translated
    t_2_translated = my_translate w_div_2 h_div_2 * t_scaled


handleEvent (EventKey (Char 'x') Down _ _) world =
  return $ change_T_matrix_world t_2_translated world
  where
    sc = 1 / 1.1
    w_div_2 = get_width world / 2
    h_div_2 = get_height world / 2
    t_translated = my_translate (-w_div_2) (-h_div_2) * get_T_matrix world
    t_scaled = my_scale sc sc * t_translated
    t_2_translated = my_translate w_div_2 h_div_2 * t_scaled


handleEvent (EventKey (Char 'i') Down _ _) world =
  return $ change_T_matrix_world t_2_translated world
  where
    w_div_2 = get_width world / 2
    h_div_2 = get_height world / 2
    t_translated = my_translate (-w_div_2) (-h_div_2) * get_T_matrix world
    t_scaled = my_scale 1.1 1 * t_translated
    t_2_translated = my_translate w_div_2 h_div_2 * t_scaled


handleEvent (EventKey (Char 'k') Down _ _) world =
  return $ change_T_matrix_world t_2_translated world
  where
    sc = 1 / 1.1
    w_div_2 = get_width world / 2
    h_div_2 = get_height world / 2
    t_translated = my_translate (-w_div_2) (-h_div_2) * get_T_matrix world
    t_scaled = my_scale sc 1 * t_translated
    t_2_translated = my_translate w_div_2 h_div_2 * t_scaled


handleEvent (EventKey (Char 'o') Down _ _) world =
  return $ change_T_matrix_world t_2_translated world
  where
    w_div_2 = get_width world / 2
    h_div_2 = get_height world / 2
    t_translated = my_translate (-w_div_2) (-h_div_2) * get_T_matrix world
    t_scaled = my_scale 1 1.1 * t_translated
    t_2_translated = my_translate w_div_2 h_div_2 * t_scaled


handleEvent (EventKey (Char 'l') Down _ _) world =
  return $ change_T_matrix_world t_2_translated world
  where
    sc = 1 / 1.1
    w_div_2 = get_width world / 2
    h_div_2 = get_height world / 2
    t_translated = my_translate (-w_div_2) (-h_div_2) * get_T_matrix world
    t_scaled = my_scale 1 sc * t_translated
    t_2_translated = my_translate w_div_2 h_div_2 * t_scaled


handleEvent (EventKey (Char 'u') Down _ _) world =
  return $ change_T_matrix_world t_2_translated world
  where
    h_div_2 = get_height world / 2
    t_translated = my_translate 0 (-h_div_2) * get_T_matrix world
    t_mirrored = my_mirrorX * t_translated
    t_2_translated = my_translate 0 h_div_2 * t_mirrored


handleEvent (EventKey (Char 'j') Down _ _) world =
  return $ change_T_matrix_world t_2_translated world
  where
    w_div_2 = get_width world / 2
    t_translated = my_translate (-w_div_2) 0 * get_T_matrix world
    t_mirrored = my_mirrorY * t_translated
    t_2_translated = my_translate w_div_2 0 * t_mirrored


handleEvent (EventKey (SpecialKey KeyEsc) Down _ _) world =
  return $ change_T_matrix_world (create_diag_matrix 1) world

handleEvent (EventKey (MouseButton LeftButton)
             Down _ (mouse_x, mouse_y)) world = do
  if (mouse_x >= w_div_2 - 50) && (mouse_x <= w_div_2 - 10) &&
     (mouse_y >= h_div_2 - 30) && (mouse_y <= h_div_2 - 10)
  then do
    putStrLn "Enter the file name with the extension (<name>.txt):"
    name <- inputFunc
    file <- readFile $ "Files/" ++ name
    return $
      let (fr_new, mp_list) = btnOpen_click (lines file)
      in change_mp_lst_world mp_list $ (change_frame_size_world fr_new world)
  else return $ world
  where
    w_div_2 = get_width world / 2
    h_div_2 = get_height world / 2
    inputFunc = do
      str <- getLine
      if null str
      then do
        inputFunc
      else return str


handleEvent _ world = return world