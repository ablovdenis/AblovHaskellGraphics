module Parsing.BtnOpen_click (btnOpen_click) where



import App.World

import Math.Matrix
import Math.TransForm
import Math.Vector (Vec2(..), Vec3(..))

import MyGraphics.Figure (Model(..), MyPath(..))


destroy_unnecessary_points :: String -> String -- Уничтожить лишние точки.
destroy_unnecessary_points "." = ""
destroy_unnecessary_points "" = ""
destroy_unnecessary_points (s0 : st@(s1 : _))
  | [s0, s1] == ". " = destroy_unnecessary_points st
  | otherwise = s0 : destroy_unnecessary_points st
destroy_unnecessary_points st = st


btnOpen_click :: (Float, Float) -> (Int, Int, Int, Int)
                 -> [String] -> World
btnOpen_click (width, height)
              (left, right, top, bottom)
              [] = World width height -- Значение по-умолчанию.
                         0 0
                         0
                         []
                         (create_diag_matrix 1)
                         (create_diag_matrix 1)
                         left right top bottom
btnOpen_click (width, height)
              (left, right, top, bottom)
              data_ = while1 -- Внешний цикл. Параметры и их начальные значения:
                        0 0
                        0
                        (create_diag_matrix 1)
                        (create_diag_matrix 1)
                        []
                        (create_diag_matrix 1)
                        (create_diag_matrix 1)
                        []
                        []
                        2
                        0 0 0
                        data_
  where
    while1 _vx _vy
           aspectFig
           initT
           _t
           models
           _ _ _ _ _ _ _ _
           [] = World width height
                      _vx _vy
                      aspectFig
                      models
                      _t
                      initT
                      left right top bottom
    while1 _vx _vy    -- Размеры рисунка.
           aspectFig  -- Соотношение сторон.
           initT      -- Матрица начального преобразования.
           _t         -- Матрица, в которой накапливаются все преобразования.
           models     -- Список рисунков.
           _m         -- Матрица для получения модельной матрицы.
           initM      -- Матрица для начального преобразования каждого рисунка.
           transforms -- Стек матриц преобразований.
           figure     -- Список ломаных очередного рисунка.
           thickness  -- Толщина со значением по умолчанию 2.
           r g b      -- Составляющие цвета.
           (str       -- Строка, в которую считываем строки файла.
            : other_str)
      | null str || head str == '#' = while1 _vx _vy
                                             aspectFig
                                             initT
                                             _t
                                             models
                                             _m
                                             initM
                                             transforms
                                             figure
                                             thickness
                                             r g b
                                             other_str
      | otherwise = case words str of
        "frame" : _vx_new : _vy_new : _ ->
          let _vx_new_float = read _vx_new :: Float
              _vy_new_float = read _vy_new :: Float
              leftFloat = fromIntegral left :: Float -- = _wcx
              rightFloat = fromIntegral right :: Float
              topFloat = fromIntegral top :: Float
              bottomFloat = fromIntegral bottom :: Float
              _wx = width - leftFloat - rightFloat
              _wy = height - topFloat - bottomFloat

              aspectFig_new = _vx_new_float / _vy_new_float

              aspectRect = _wx / _wy
              _t1 = my_translate (-_vx_new_float / 2) (-_vy_new_float / 2)
              _s = if aspectFig_new < aspectRect
                   then _wy / _vy_new_float else _wx / _vx_new_float
              _s1 = my_scale _s _s
              _t2 = my_translate (leftFloat + _wx / 2) (bottomFloat + _wy / 2)
              initT_new = _t2 * _s1 * _t1
          in while1 _vx_new_float _vy_new_float
                    aspectFig_new
                    initT_new
                    initT_new -- _t = initT_new
                    models
                    _m
                    initM
                    transforms
                    figure
                    thickness
                    r g b
                    other_str
        "color" : r_new : g_new : b_new : _ ->
          while1 _vx _vy
                  aspectFig
                  initT
                  _t
                  models
                  _m
                  initM
                  transforms
                  figure
                  thickness
                  (read r_new :: Float)
                  (read g_new :: Float)
                  (read b_new :: Float)
                  other_str
        "thickness" : thickness_new : _ ->
          while1 _vx _vy
                  aspectFig
                  initT
                  _t
                  models
                  _m
                  initM
                  transforms
                  figure
                  (read thickness_new :: Float)
                  r g b
                  other_str
        "path" : _n : _ ->
          let while2 0 other_str1 vertices = (other_str1, vertices)
              while2 n (str1 : other_str1) vertices
                | null str1 || head str1 == '#' =
                  while2 n other_str1 vertices
                | otherwise = case words str1 of
                  x : y : _ ->
                    while2 (n - 1) other_str1
                           (Vec2 (read (destroy_unnecessary_points x) :: Float)
                                 (read (destroy_unnecessary_points y) :: Float)
                            : vertices)
                  _ -> error "PARSING PATH ERROR"
              while2 _ _ _ = error "PARSING PATH ERROR"
              (other_str_new, vertices1) = while2 (read _n :: Int)
                                                 other_str []
          in while1 _vx _vy
                    aspectFig
                    initT
                    _t
                    models
                    _m
                    initM
                    transforms
                    (MyPath vertices1 (Vec3 r g b) thickness
                     : figure)
                    thickness
                    r g b
                    other_str_new
        "model" : mVcx : mVcy : mVx : mVy : _ ->
          let mVx_float = read mVx :: Float
              mVy_float = read mVy :: Float
              mVcx_float = read mVcx :: Float
              mVcy_float = read mVcy :: Float
              _s = if mVx_float / mVy_float < 1
                   then 2 / mVy_float else 2 / mVx_float
              initM_new = my_scale _s _s * my_translate (-mVcx_float)
                                                        (-mVcy_float)
          in while1 _vx _vy
                    aspectFig
                    initT
                    _t
                    models
                    _m
                    initM_new
                    transforms
                    []
                    thickness
                    r g b
                    other_str
        "figure" : _ ->
          while1 _vx _vy
                 aspectFig
                 initT
                 _t
                 (Model figure (_m * initM) : models)
                 _m
                 initM
                 transforms
                 figure
                 thickness
                 r g b
                 other_str
        "translate" : _tx : _ty : _ ->
          let _m_new = my_translate (read _tx :: Float)
                                    (read _ty :: Float)
                       * _m
          in while1 _vx _vy
                    aspectFig
                    initT
                    _t
                    models
                    _m_new
                    initM
                    transforms
                    figure
                    thickness
                    r g b
                    other_str
        "scale" : _s : _ ->
          let _s_float = read _s :: Float
              _m_new = my_scale _s_float _s_float
                       * _m
          in while1 _vx _vy
                    aspectFig
                    initT
                    _t
                    models
                    _m_new
                    initM
                    transforms
                    figure
                    thickness
                    r g b
                    other_str
        "rotate" : theta : _ ->
          let _m_new = my_rotate ((read theta :: Float) / 180 * pi)
                       * _m
          in while1 _vx _vy
                    aspectFig
                    initT
                    _t
                    models
                    _m_new
                    initM
                    transforms
                    figure
                    thickness
                    r g b
                    other_str
        "pushTransform" : _ ->
          while1 _vx _vy
                 aspectFig
                 initT
                 _t
                 models
                 _m
                 initM
                 (_m : transforms)
                 figure
                 thickness
                 r g b
                 other_str
        "popTransform" : _ ->
          case transforms of
            (_m_new : popped_transforms) ->
              while1 _vx _vy
                      aspectFig
                      initT
                      _t
                      models
                      _m_new
                      initM
                      popped_transforms
                      figure
                      thickness
                      r g b
                      other_str
            _ -> error "NULL TRANSFORMS ERROR"
        _ -> error "PARSING ERROR."