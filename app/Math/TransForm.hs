module Math.TransForm where

import Math.Vector (Vec3(..), Vec4(..), Vec2(..),
                    vec4_from_vec3, norm_vec3)
import Math.Matrix (Mat3(..), Mat4(..),
                    crossM, cross, mult_by_num_matr,
                    change_coord_mat3, create_diag_matrix)

-- import Debug.Trace -- Для тестирования.



-- Операции для 2-мерного пространства:
my_translate_2 :: Float -> Float -> Mat3 -- Получить матрицу переноса.
my_translate_2 tx_ ty_ = ((change_coord_mat3 0 2 tx_) . 
                         (change_coord_mat3 1 2 ty_)) (create_diag_matrix 1)


my_scale_2 :: Float -> Float -> Mat3 -- Получить матрицу масштабирования.
my_scale_2 sx_ sy_ = ((change_coord_mat3 0 0 sx_) . 
                     (change_coord_mat3 1 1 sy_)) (create_diag_matrix 1)


my_rotate_2 :: Float -> Mat3 -- Получить матрицу поворота.
my_rotate_2 theta = Mat3 (Vec3 cos_theta (-sin_theta) 0)
                         (Vec3 sin_theta cos_theta 0)
                         (Vec3 0 0 1)
  where
    cos_theta = cos theta
    sin_theta = sin theta


my_mirrorX :: Mat3
my_mirrorX = Mat3 (Vec3 1 0 0) -- Матрица отражения по оси Ox.
                  (Vec3 0 (-1) 0)
                  (Vec3 0 0 1)


my_mirrorY :: Mat3
my_mirrorY = Mat3 (Vec3 (-1) 0 0) -- Матрица отражения по оси Oy.
                  (Vec3 0 1 0)
                  (Vec3 0 0 1)


-- Операции для 3-мерного пространства:
my_translate_3 :: Float -> Float -> Float -> Mat4 -- Получить матрицу переноса.
my_translate_3 _Tx _Ty _Tz = Mat4 (Vec4 1 0 0 _Tx)
                                  (Vec4 0 1 0 _Ty)
                                  (Vec4 0 0 1 _Tz)
                                  (Vec4 0 0 0 1)


my_scale_3 :: Float -> Float -> Float -> Mat4 -- Получить матрицу масштабирования.
my_scale_3 _Sx _Sy _Sz = Mat4 (Vec4 _Sx 0 0 0)
                              (Vec4 0 _Sy 0 0)
                              (Vec4 0 0 _Sz 0)
                              (Vec4 0 0 0 1)


my_rotate_3 :: Float -> Vec3 -> Mat4 -- Получить матрицу поворота.
my_rotate_3 theta n =
  Mat4 (vec4_from_vec3 v1 0) (vec4_from_vec3 v2 0)
       (vec4_from_vec3 v3 0) (Vec4 0 0 0 1)
  where
    crossM_n = crossM $ norm_vec3 n
    Mat3 v1 v2 v3 =
      create_diag_matrix 1 + mult_by_num_matr (sin theta) crossM_n
      + mult_by_num_matr (1 - cos theta) (crossM_n ^ (2 :: Word))


my_rotateP :: Float -> Vec3 -> Vec3 -> Mat4
my_rotateP theta n (Vec3 px py pz) =
  my_translate_3 px py pz * my_rotate_3 theta n
  * my_translate_3 (-px) (-py) (-pz)


lookAt :: Vec3 -> Vec3 -> Vec3 -> Mat4
lookAt (_S@(Vec3 xs ys zs)) _P u = _R * _T
  where
    _T = Mat4 (Vec4 1 0 0 (-xs))
              (Vec4 0 1 0 (-ys))
              (Vec4 0 0 1 (-zs))
              (Vec4 0 0 0 1)
    e_3 = norm_vec3 (_S - _P)
    e_1 = norm_vec3 (cross u e_3)
    e_2 = norm_vec3 (cross e_3 e_1)
    _R = Mat4 (vec4_from_vec3 e_1 0)
              (vec4_from_vec3 e_2 0)
              (vec4_from_vec3 e_3 0)
              (Vec4 0 0 0 1)


-- Матрицы проекций:
ortho :: Float -> Float -> Float -> Float
         -> Float -> Float -> Mat4
ortho l r b t zn zf =
  Mat4 (Vec4 (2 / r_minus_l) 0 0 (- (r + l) / r_minus_l))
       (Vec4 0 (2 / t_minus_b) 0 (- (t + b) / t_minus_b))
       (Vec4 0 0 (2 / zf_minus_zn) ((zf + zn) / zf_minus_zn))
       (Vec4 0 0 0 1)
  where
    r_minus_l = r - l
    t_minus_b = t - b
    zf_minus_zn = zn - zf

frustum :: Float -> Float -> Float -> Float
         -> Float -> Float -> Mat4
frustum l r b t n f =
  Mat4 (Vec4 (n_mult_2 / r_minus_l) 0 ((r + l) / r_minus_l) 0)
       (Vec4 0 (n_mult_2 / t_minus_b) ((t + b) / t_minus_b) 0)
       (Vec4 0 0 ((f + n) / n_minus_f) ((n_mult_2 * f) / n_minus_f))
       (Vec4 0 0 (-1) 0)
  where
    n_mult_2 = 2 * n
    r_minus_l = r - l
    t_minus_b = t - b
    n_minus_f = n - f


perspective :: Float -> Float -> Float -> Float -> Mat4
perspective fovy aspect n f =
  Mat4 (Vec4 (ctg_fovy_div_2 / aspect) 0 0 0)
       (Vec4 0 ctg_fovy_div_2 0 0)
       (Vec4 0 0 ((f + n) / n_minus_f) ((2 * f * n) / n_minus_f))
       (Vec4 0 0 (-1) 0)
  where
    ctg_fovy_div_2 = 1 / (tan (fovy / 2))
    n_minus_f = n - f


-- cadrRL :: Vec2 -> Vec2 -> Vec2 -> Vec2 -> Mat3
-- cadrRL (Vec2 _Vcx _Vcy) (Vec2 _Vx _Vy)
--        (Vec2 _Wcx _Wcy) (Vec2 _Wx _Wy) =
--   my_translate_2 (_Wcx - _Wx / 2) (_Wcy - _Wy / 2)
--   * my_scale_2 (_Wx / _Vx) (_Wy / _Vy)
--   * my_translate_2 (-_Vcx) (-_Vcy)

cadrRL :: Vec2 -> Vec2 -> Vec2 -> Vec2 -> Mat3
cadrRL (Vec2 _Vcx _Vcy) (Vec2 _Vx _Vy)
       (Vec2 _Wcx _Wcy) (Vec2 _Wx _Wy) =
  my_translate_2 _Wcx _Wcy
  * my_scale_2 (_Wx / _Vx) (_Wy / _Vy)
  * my_translate_2 (-_Vcx) (-_Vcy)
