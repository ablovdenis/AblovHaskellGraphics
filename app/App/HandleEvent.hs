module App.HandleEvent where



import App.World (World(..), ProjType(..))

import Math.Matrix (normalize_matr4,
                    product_of_matr3_by_vec3,
                    product_of_matr4_by_vec4)
import Math.TransForm (lookAt, my_rotate_3, my_rotateP)
import Math.Vector (Vec3(..), Vec4(..), normalize_vec4)

import Parsing.BtnOpen_click (btnOpen_click, initWorkPars)

import Graphics.Gloss.Interface.IO.Game


handleEvent :: Event -> World -> IO World
handleEvent (EventKey (MouseButton LeftButton)
             Down _ (mouse_x, mouse_y)) world = do
  if (mouse_x >= w_div_2 - 50) && (mouse_x <= w_div_2 - 10) &&
     (mouse_y >= h_div_2 - 30) && (mouse_y <= h_div_2 - 10)
  then do
    putStrLn "Enter the file name with the extension (<name>.txt):"
    name <- inputFunc
    data_ <- readFile $ "Files/" ++ name
    return $ btnOpen_click world (lines data_)
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

handleEvent (EventResize (w, h)) world =
  return $ world {width_ = fromIntegral w :: Float,
                  height_ = fromIntegral h :: Float}

handleEvent (EventKey (SpecialKey KeyEsc) Down _ _) world = do
  -- putStrLn $ "S = " ++ show (_S_ world)
  -- putStrLn $ "P = " ++ show (_P_ world)
  -- putStrLn $ "u = " ++ show (u_ world)
  -- putStrLn $ "dist = " ++ show (dist_ world)
  return $ initWorkPars world

handleEvent (EventKey (Char '1') Down _ _) world =
  return $ world {pType_ = Ortho}

handleEvent (EventKey (Char '2') Down _ _) world =
  return $ world {pType_ = Frustum}

handleEvent (EventKey (Char '3') Down _ _) world =
  return $ world {pType_ = Perspective}

handleEvent (EventKey (Char 'w') Down _ _) world =
  return $
    world {_T_ = (lookAt (Vec3 0 0 (-1))
                         (Vec3 0 0 (-2))
                         (Vec3 0 1 0)) * _T_ world}

handleEvent (EventKey (Char 's') Down _ _) world =
  return $
    world {_T_ = (lookAt (Vec3 0 0 1)
                         (Vec3 0 0 0)
                         (Vec3 0 1 0)) * _T_ world}

handleEvent (EventKey (Char 'a') Down _ _) world =
  return $
    world {_T_ = (lookAt (Vec3 (-1) 0 0)
                         (Vec3 (-1) 0 (-1))
                         (Vec3 0 1 0)) * _T_ world}

handleEvent (EventKey (Char 'd') Down _ _) world =
  return $
    world {_T_ = (lookAt (Vec3 1 0 0)
                         (Vec3 1 0 (-1))
                         (Vec3 0 1 0)) * _T_ world}

handleEvent (EventKey (Char 'W') Down _ _) world =
  return $
    world {_T_ = (lookAt (Vec3 0 0 (-0.1))
                         (Vec3 0 0 (-1.1))
                         (Vec3 0 1 0)) * _T_ world}

handleEvent (EventKey (Char 'S') Down _ _) world =
  return $
    world {_T_ = (lookAt (Vec3 0 0 0.1)
                         (Vec3 0 0 (-0.9))
                         (Vec3 0 1 0)) * _T_ world}

handleEvent (EventKey (Char 'A') Down _ _) world =
  return $
    world {_T_ = (lookAt (Vec3 (-0.1) 0 0)
                         (Vec3 (-0.1) 0 (-1))
                         (Vec3 0 1 0)) * _T_ world}

handleEvent (EventKey (Char 'D') Down _ _) world =
  return $
    world {_T_ = (lookAt (Vec3 0.1 0 0)
                         (Vec3 0.1 0 (-1))
                         (Vec3 0 1 0)) * _T_ world}

handleEvent (EventKey (Char 'r') Down _ _) world =
  return $
    world {_T_ = (lookAt (Vec3 0 0 0)
                         (Vec3 0 0 (-1))
                         u_new) * _T_ world}
  where 
    u_new =
      product_of_matr3_by_vec3
        (normalize_matr4 (my_rotate_3 0.1 (Vec3 0 0 1)))
        (Vec3 0 1 0)

handleEvent (EventKey (Char 'y') Down _ _) world =
  return $
    world {_T_ = (lookAt (Vec3 0 0 0)
                         (Vec3 0 0 (-1))
                         u_new) * _T_ world}
  where 
    u_new =
      product_of_matr3_by_vec3
        (normalize_matr4 (my_rotate_3 (-0.1) (Vec3 0 0 1)))
        (Vec3 0 1 0)

handleEvent (EventKey (Char 't') Down _ _) world =
  return $
    world {_T_ = (lookAt (Vec3 0 0 0)
                         _P_new
                         u_new) * _T_ world}
  where
    _M = my_rotate_3 0.1 (Vec3 1 0 0)
    u_new = product_of_matr3_by_vec3 (normalize_matr4 _M) (Vec3 0 1 0)
    _P_new =
      normalize_vec4
        (product_of_matr4_by_vec4
          _M (Vec4 0 0 (-1) 1))

handleEvent (EventKey (Char 'T') Down _ _) world =
  return $
    world {_T_ = (lookAt _S_new
                         vec_dist
                         u_new) * _T_ world}
  where
    vec_dist = Vec3 0 0 (-dist_ world)
    _M = my_rotateP 0.1 (Vec3 1 0 0) vec_dist
    u_new = product_of_matr3_by_vec3 (normalize_matr4 _M) (Vec3 0 1 0)
    _S_new =
      normalize_vec4
        (product_of_matr4_by_vec4
          _M (Vec4 0 0 0 1))

handleEvent (EventKey (Char 'g') Down _ _) world =
  return $
    world {_T_ = (lookAt (Vec3 0 0 0)
                         _P_new
                         u_new) * _T_ world}
  where
    _M = my_rotate_3 (-0.1) (Vec3 1 0 0)
    u_new = product_of_matr3_by_vec3 (normalize_matr4 _M) (Vec3 0 1 0)
    _P_new =
      normalize_vec4
        (product_of_matr4_by_vec4
          _M (Vec4 0 0 (-1) 1))

handleEvent (EventKey (Char 'G') Down _ _) world =
  return $
    world {_T_ = (lookAt _S_new
                         vec_dist
                         u_new) * _T_ world}
  where
    vec_dist = Vec3 0 0 (-dist_ world)
    _M = my_rotateP (-0.1) (Vec3 1 0 0) vec_dist
    u_new = product_of_matr3_by_vec3 (normalize_matr4 _M) (Vec3 0 1 0)
    _S_new =
      normalize_vec4
        (product_of_matr4_by_vec4
          _M (Vec4 0 0 0 1))

handleEvent (EventKey (Char 'f') Down _ _) world =
  return $
    world {_T_ = (lookAt (Vec3 0 0 0)
                         _P_new
                         (Vec3 0 1 0)) * _T_ world}
  where
    _M = my_rotate_3 0.1 (Vec3 0 1 0)
    _P_new =
      normalize_vec4
        (product_of_matr4_by_vec4
          _M (Vec4 0 0 (-1) 1))

handleEvent (EventKey (Char 'F') Down _ _) world =
  return $
    world {_T_ = (lookAt _S_new
                         vec_dist
                         (Vec3 0 1 0)) * _T_ world}
  where
    vec_dist = Vec3 0 0 (-dist_ world)
    _M = my_rotateP 0.1 (Vec3 0 1 0) vec_dist
    _S_new =
      normalize_vec4
        (product_of_matr4_by_vec4
          _M (Vec4 0 0 0 1))

handleEvent (EventKey (Char 'h') Down _ _) world =
  return $
    world {_T_ = (lookAt (Vec3 0 0 0)
                         _P_new
                         (Vec3 0 1 0)) * _T_ world}
  where
    _M = my_rotate_3 (-0.1) (Vec3 0 1 0)
    _P_new =
      normalize_vec4
        (product_of_matr4_by_vec4
          _M (Vec4 0 0 (-1) 1))

handleEvent (EventKey (Char 'H') Down _ _) world =
  return $
    world {_T_ = (lookAt _S_new
                         vec_dist
                         (Vec3 0 1 0)) * _T_ world}
  where
    vec_dist = Vec3 0 0 (-dist_ world)
    _M = my_rotateP (-0.1) (Vec3 0 1 0) vec_dist
    _S_new =
      normalize_vec4
        (product_of_matr4_by_vec4
          _M (Vec4 0 0 0 1))

handleEvent (EventKey (Char 'i') Down _ _) world =
  return $ world {t_ = t_ world + 1}

handleEvent (EventKey (Char 'I') Down _ _) world =
  return $ world {t_ = t_ world - 1}

handleEvent (EventKey (Char 'j') Down _ _) world =
  return $ world {l_ = l_ world - 1}

handleEvent (EventKey (Char 'J') Down _ _) world =
  return $ world {l_ = l_ world + 1}

handleEvent (EventKey (Char 'k') Down _ _) world =
  return $ world {b_ = b_ world - 1}

handleEvent (EventKey (Char 'K') Down _ _) world =
  return $ world {b_ = b_ world + 1}

handleEvent (EventKey (Char 'l') Down _ _) world =
  return $ world {r_ = r_ world + 1}

handleEvent (EventKey (Char 'L') Down _ _) world =
  return $ world {r_ = r_ world - 1}

handleEvent (EventKey (Char 'u') Down _ _) world =
  return $ world {n_ = if n >= (f_ world - 0.3)
                       then (f_ world - 0.1) else n + 0.2}
  where n = n_ world

handleEvent (EventKey (Char 'U') Down _ _) world =
  return $ world {n_ = if n <= 0.3 then 0.1 else n - 0.2}
  where n = n_ world

handleEvent (EventKey (Char 'o') Down _ _) world =
  return $ world {f_ = f_ world + 0.2}

handleEvent (EventKey (Char 'O') Down _ _) world =
  return $ world {f_ = if f <= (n + 0.3) then n + 0.1
                       else f - 0.2}
  where f = f_ world
        n = n_ world

handleEvent (EventKey (Char 'b') Down _ _) world =
  return $ world {dist_ = dist_ world + 0.2}

handleEvent (EventKey (Char 'B') Down _ _) world =
  return $ world {dist_ = if dist <= 0.3 then 0.1
                       else dist - 0.2}
  where dist = dist_ world

handleEvent (EventKey (Char 'z') Down _ _) world =
  return $ world {fovy_work_ = if fovy_work >= 2.7 then 3
                       else fovy_work + 0.2}
  where fovy_work = fovy_work_ world

handleEvent (EventKey (Char 'Z') Down _ _) world =
  return $ world {fovy_work_ = if fovy_work <= 0.5 then 0.3
                       else fovy_work - 0.2}
  where fovy_work = fovy_work_ world

handleEvent (EventKey (Char 'x') Down _ _) world =
  return $ world {aspect_work_ = aspect_work_ world + 0.05}

handleEvent (EventKey (Char 'X') Down _ _) world =
  return $ world {aspect_work_ = if aspect_work <= 0.06 then 0.01
                       else aspect_work - 0.05}
  where aspect_work = aspect_work_ world

handleEvent _ world = return world