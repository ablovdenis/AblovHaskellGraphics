module Parsing.BtnOpen_click (btnOpen_click, initWorkPars) where



import App.World (World(..))

import Math.Vector (Vec3(..), length_vec)
import Math.Matrix (Mat4, create_diag_matrix)
import Math.TransForm (my_translate_3, my_scale_3, my_rotate_3,
                       lookAt)


import MyGraphics.Figure (Model(..), MyPath(..))


destroy_unnecessary_points :: String -> String -- Уничтожить лишние точки.
destroy_unnecessary_points "." = ""
destroy_unnecessary_points "" = ""
destroy_unnecessary_points (s0 : st@(s1 : _))
  | [s0, s1] == ". " = destroy_unnecessary_points st
  | otherwise = s0 : destroy_unnecessary_points st
destroy_unnecessary_points st = st


initWorkPars :: World -> World
initWorkPars world =
  world {n_ = near,
         f_ = far_ world,
         fovy_work_ = fovy,
         aspect_work_ = aspect,
         l_ = (-_Vx_div_2),
         r_ = _Vx_div_2,
         b_ = (-_Vy_div_2),
         t_ = _Vy_div_2,
         dist_ = length_vec (_P - _S),
         _T_ = lookAt _S _P (u_ world)
         }
  where
    fovy = fovy_ world
    aspect = aspect_ world
    near = near_ world
    _Vy_div_2 = near * tan (fovy / 2)
    _Vx_div_2 = aspect * _Vy_div_2
    _S = _S_ world
    _P = _P_ world


btnOpen_click :: World -> [String] -> World
btnOpen_click world0 data_ =
  while1 -- Внешний цикл. Параметры и их начальные значения:
    (world0 {models_ = []})
    (create_diag_matrix 1 :: Mat4)
    (create_diag_matrix 1 :: Mat4)
    []
    []
    2
    0 0 0
    data_
  where
    while1 world _ _ _ _ _ _ _ _ [] = initWorkPars world
    while1 world
           _M
           initM
           transforms
           figure
           thickness
           r g b
           (str       -- Строка, в которую считываем строки файла.
            : other_str)
      | null str || head str == '#' =
        while1 world _M initM transforms
               figure thickness r g b other_str
      | otherwise = case words str of
        "camera" : _Sx : _Sy : _Sz
                 : _Px : _Py : _Pz
                 : ux : uy : uz : _ ->
          let
            _Sx_Float = read _Sx :: Float
            _Sy_Float = read _Sy :: Float
            _Sz_Float = read _Sz :: Float
            _S = Vec3 _Sx_Float _Sy_Float _Sz_Float
            _Px_Float = read _Px :: Float
            _Py_Float = read _Py :: Float
            _Pz_Float = read _Pz :: Float
            _P = Vec3 _Px_Float _Py_Float _Pz_Float
            ux_Float = read ux :: Float
            uy_Float = read uy :: Float
            uz_Float = read uz :: Float
            u = Vec3 ux_Float uy_Float uz_Float
          in while1 (world {_S_ = _S, _P_ = _P, u_ = u})
                    _M
                    initM
                    transforms
                    figure
                    thickness
                    r g b
                    other_str
        "screen" : fovy_work : aspect : near : far : _ ->
          let
            fovy_work_Float = read fovy_work :: Float
            fovy = fovy_work_Float / 180 * pi
            aspect_Float = read aspect :: Float
            near_Float = read near :: Float
            far_Float = read far :: Float
          in while1 (world {fovy_ = fovy,
                            fovy_work_ = fovy_work_Float,
                            aspect_ = aspect_Float,
                            near_ = near_Float,
                            far_ = far_Float})
                    _M
                    initM
                    transforms
                    figure
                    thickness
                    r g b
                    other_str
        "color" : r_new : g_new : b_new : _ ->
          let
            r_Float = read r_new :: Float
            g_Float = read g_new :: Float
            b_Float = read b_new :: Float
          in while1 world
                    _M
                    initM
                    transforms
                    figure
                    thickness
                    r_Float g_Float b_Float
                    other_str
        "thickness" : thickness_new : _ ->
          while1 world
            _M
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
                  x : y : z : _ ->
                    while2 (n - 1) other_str1
                           (Vec3 (read (destroy_unnecessary_points x) :: Float)
                                 (read (destroy_unnecessary_points y) :: Float)
                                 (read (destroy_unnecessary_points z) :: Float)
                            : vertices)
                  _ -> error "PARSING PATH COORD ERROR"
              while2 _ _ _ = error "PARSING PATH ERROR"
              (other_str_new, vertices1) = while2 (read _n :: Int)
                                                  other_str []
          in while1 world
                    _M
                    initM
                    transforms
                    ((MyPath vertices1
                             (Vec3 r g b)
                             thickness) : figure)
                    thickness
                    r g b
                    other_str_new
        "model" : mVcx : mVcy : mVcz
                : mVx : mVy : _ ->
          let mVcx_Float = read mVcx :: Float
              mVcy_Float = read mVcy :: Float
              mVcz_Float = read mVcz :: Float
              mVx_Float = read mVx :: Float
              mVy_Float = read mVy :: Float
              _S = if mVx_Float / mVy_Float < 1
                   then 2 / mVy_Float else 2 / mVx_Float
              initM_new = my_scale_3 _S _S _S
                          * my_translate_3 (-mVcx_Float)
                                           (-mVcy_Float)
                                           (-mVcz_Float)
          in while1 world
                    _M
                    initM_new
                    transforms
                    []
                    thickness
                    r g b
                    other_str
        "figure" : _ ->
          while1
            world {models_ = (Model figure (_M * initM)) : (models_ world)}
            _M
            initM
            transforms
            figure
            thickness
            r g b
            other_str
        "translate" : _Tx : _Ty : _Tz : _ ->
          let _M_new = my_translate_3 (read _Tx :: Float)
                                      (read _Ty :: Float)
                                      (read _Tz :: Float)
                       * _M
          in while1 world
                    _M_new
                    initM
                    transforms
                    figure
                    thickness
                    r g b
                    other_str
        "scale" : _S : _ ->
          let _S_Float = read _S :: Float
              _M_new = my_scale_3 _S_Float _S_Float _S_Float
                       * _M
          in while1 world
                    _M_new
                    initM
                    transforms
                    figure
                    thickness
                    r g b
                    other_str
        "rotate" : theta : nx : ny : nz : _ ->
          let theta_rad_Float =
                (read theta :: Float) / 180 * pi
              nx_Float = read nx :: Float
              ny_Float = read ny :: Float
              nz_Float = read nz :: Float
              _M_new =
                my_rotate_3 theta_rad_Float
                            (Vec3 nx_Float ny_Float nz_Float)
                * _M
          in while1 world
                    _M_new
                    initM
                    transforms
                    figure
                    thickness
                    r g b
                    other_str
        "pushTransform" : _ ->
          while1 world
            _M
            initM
            (_M : transforms)
            figure
            thickness
            r g b
            other_str
        "popTransform" : _ ->
          case transforms of
            (_M_new : popped_transforms) ->
              while1
                world
                _M_new
                initM
                popped_transforms
                figure
                thickness
                r g b
                other_str
            _ -> error "NULL TRANSFORMS ERROR"
        _ -> error "PARSING ERROR."