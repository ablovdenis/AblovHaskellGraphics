module App.Render (render) where



import App.World

import Math.Matrix (product_of_matrix_by_vector)
import Math.TransForm (my_translate, my_scale)
import Math.Vector (Vec3(..), vec3_from_vec2, normalize)

import MyGraphics.Figure (MyPath(..))
import MyGraphics.ThickLine (thickBrokenLine)
import MyGraphics.ToLeftCoordSyst (toLeftCoord)

import Graphics.Gloss


render :: World -> IO Picture
render world = return $ Pictures [
  Color black $ Pictures (mypath_lst_to_picture_lst $ get_mypath_list world),
  button
  ]
  where
    (vx_, vy_) = get_frame_size world

    wx_ = get_width world
    wy_ = get_height world

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
    initT = toLeftCoordMat * get_T_matrix world * my_translate 0 ty_ * my_scale s_ (-s_)

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