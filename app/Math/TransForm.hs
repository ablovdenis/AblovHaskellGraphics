module Math.TransForm where

import Math.Vector (Vec3(..))
import Math.Matrix (Mat3(..), change_coord_mat3, create_diag_matrix)



my_translate :: Float -> Float -> Mat3 -- Получить матрицу переноса.
my_translate tx_ ty_ = ((change_coord_mat3 0 2 tx_) . 
                       (change_coord_mat3 1 2 ty_)) (create_diag_matrix 1)


my_scale :: Float -> Float -> Mat3 -- Получить матрицу масштабирования.
my_scale sx_ sy_ = ((change_coord_mat3 0 0 sx_) . 
                   (change_coord_mat3 1 1 sy_)) (create_diag_matrix 1)


my_rotate :: Float -> Mat3 -- Получить матрицу поворота.
my_rotate theta = Mat3 (Vec3 cos_theta (-sin_theta) 0)
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