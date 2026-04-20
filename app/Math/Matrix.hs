module Math.Matrix where

import Math.Vector (Vec2(..), Vec3(..), Vec4(..),
                    change_coord_vec3, dot, mult_by_num_vec)



class ClassMatrix matr where
  create_diag_matrix :: Float -> matr
      -- Создание единичной матрицы.
  transpose :: matr -> matr -- Транспонирование.
  mult_by_num_matr :: Float -> matr -> matr -- Домножение на число.


data Mat2 = Mat2 Vec2 Vec2 deriving Eq

data Mat3 = Mat3 Vec3 Vec3 Vec3 deriving Eq

data Mat4 = Mat4 Vec4 Vec4 Vec4 Vec4 deriving Eq


instance Show Mat2 where
  show (Mat2 v1 v2) = "Mat2[\n|" ++ show v1 ++ "\n|"
                      ++ show v2 ++ "\n]"

instance Show Mat3 where
  show (Mat3 v1 v2 v3) = "Mat3[\n|" ++ show v1 ++ "\n|"
                         ++ show v2 ++ "\n|" ++ show v3 ++ "\n]"

instance Show Mat4 where
  show (Mat4 v1 v2 v3 v4) = "Mat4[\n|" ++ show v1
                            ++ "\n|" ++ show v2
                            ++ "\n|" ++ show v3
                            ++ "\n|" ++ show v4 ++ "\n]"


instance ClassMatrix Mat2 where
  create_diag_matrix a = Mat2 (Vec2 a 0)
                              (Vec2 0 a)
  transpose (Mat2 (Vec2 x1 y1)
                  (Vec2 x2 y2)) =
    Mat2 (Vec2 x1 x2)
         (Vec2 y1 y2)
  mult_by_num_matr num (Mat2 v1 v2) =
    Mat2 (mult_by_num_vec num v1) (mult_by_num_vec num v2)                                      

instance ClassMatrix Mat3 where
  create_diag_matrix a =
    Mat3 (Vec3 a 0 0)
         (Vec3 0 a 0)
         (Vec3 0 0 a)
  transpose (Mat3 (Vec3 x1 y1 z1)
                  (Vec3 x2 y2 z2)
                  (Vec3 x3 y3 z3)) =
    Mat3 (Vec3 x1 x2 x3)
         (Vec3 y1 y2 y3)
         (Vec3 z1 z2 z3)
  mult_by_num_matr num (Mat3 v1 v2 v3) =
    Mat3 (mult_by_num_vec num v1) (mult_by_num_vec num v2)
         (mult_by_num_vec num v3)

instance ClassMatrix Mat4 where
  create_diag_matrix a =
    Mat4 (Vec4 a 0 0 0)
         (Vec4 0 a 0 0)
         (Vec4 0 0 a 0)
         (Vec4 0 0 0 a)
  transpose (Mat4 (Vec4 x1 y1 z1 a1)
                  (Vec4 x2 y2 z2 a2)
                  (Vec4 x3 y3 z3 a3)
                  (Vec4 x4 y4 z4 a4)) =
    Mat4 (Vec4 x1 x2 x3 x4)
         (Vec4 y1 y2 y3 y4)
         (Vec4 z1 z2 z3 z4)
         (Vec4 a1 a2 a3 a4)
  mult_by_num_matr num (Mat4 v1 v2 v3 v4) =
    Mat4 (mult_by_num_vec num v1) (mult_by_num_vec num v2)
         (mult_by_num_vec num v3) (mult_by_num_vec num v4)


instance Num Mat2 where
  (*) mat1 mat2 = transpose $ Mat2 v1 v2
    where
      Mat2 col0_of_mat2 col1_of_mat2 = transpose mat2
      v1 = product_of_matr2_by_vec2 mat1 col0_of_mat2
      v2 = product_of_matr2_by_vec2 mat1 col1_of_mat2
  (+) (Mat2 vec1_0 vec1_1)
      (Mat2 vec2_0 vec2_1) =
        Mat2 (vec1_0 + vec2_0)
             (vec1_1 + vec2_1)
  negate (Mat2 v1 v2) = Mat2 (-v1) (-v2)
  (-) mat1 mat2 = mat1 + (-mat2)
  abs _ = error "Функция не определена."
  signum _ = error "Функция не определена."
  fromInteger _ = error "Функция не определена."

instance Num Mat3 where
  (*) mat1 mat2 = transpose $ Mat3 v0 v1 v2
    where
      Mat3 col0_of_mat2 col1_of_mat2 col2_of_mat2 = transpose mat2
      v0 = product_of_matr3_by_vec3 mat1 col0_of_mat2
      v1 = product_of_matr3_by_vec3 mat1 col1_of_mat2
      v2 = product_of_matr3_by_vec3 mat1 col2_of_mat2
  (+) (Mat3 vec1_0 vec1_1 vec1_2)
      (Mat3 vec2_0 vec2_1 vec2_2) =
        Mat3 (vec1_0 + vec2_0)
             (vec1_1 + vec2_1)
             (vec1_2 + vec2_2)
  negate (Mat3 v1 v2 v3) = Mat3 (-v1) (-v2) (-v3)
  (-) mat1 mat2 = mat1 + (-mat2)
  abs _ = error "Функция не определена."
  signum _ = error "Функция не определена."
  fromInteger _ = error "Функция не определена."

instance Num Mat4 where
  (*) mat1 mat2 = transpose $ Mat4 v1 v2 v3 v4
    where
      Mat4 col0_of_mat2 col1_of_mat2
           col2_of_mat2 col3_of_mat2 = transpose mat2
      v1 = product_of_matr4_by_vec4 mat1 col0_of_mat2
      v2 = product_of_matr4_by_vec4 mat1 col1_of_mat2
      v3 = product_of_matr4_by_vec4 mat1 col2_of_mat2
      v4 = product_of_matr4_by_vec4 mat1 col3_of_mat2
  (+) (Mat4 vec1_0 vec1_1 vec1_2 vec1_3)
      (Mat4 vec2_0 vec2_1 vec2_2 vec2_3) =
        Mat4 (vec1_0 + vec2_0)
             (vec1_1 + vec2_1)
             (vec1_2 + vec2_2)
             (vec1_3 + vec2_3)
  negate (Mat4 v1 v2 v3 v4) = Mat4 (-v1) (-v2) (-v3) (-v4)
  (-) mat1 mat2 = mat1 + (-mat2)
  abs _ = error "Функция не определена."
  signum _ = error "Функция не определена."
  fromInteger _ = error "Функция не определена."


change_vec_mat3 :: Int -> Vec3 -> Mat3 -> Mat3 -- Получение изменённой в одном
                                               -- векторе матрицы Mat3.
change_vec_mat3 0 vec (Mat3 _ v1 v2) = Mat3 vec v1 v2
change_vec_mat3 1 vec (Mat3 v0 _ v2) = Mat3 v0 vec v2
change_vec_mat3 2 vec (Mat3 v0 v1 _) = Mat3 v0 v1 vec
change_vec_mat3 _ _ _ =
  error "Неправильные параметры для change_vec_mat3."


get_vec3_of_matrix :: Int -> Mat3 -> Vec3
get_vec3_of_matrix 0 (Mat3 v _ _) = v
get_vec3_of_matrix 1 (Mat3 _ v _) = v
get_vec3_of_matrix 2 (Mat3 _ _ v) = v
get_vec3_of_matrix _ _ =
  error "Неправильные параметры для get_vec3_of_matrix."


change_coord_mat3 :: Int -> Int -> Float -> Mat3 -> Mat3 
                  -- Получение изменённой в одной
                  -- координате матрицы Mat3.
change_coord_mat3 i j value mat =
  change_vec_mat3 i
                  (change_coord_vec3 j value
                                     (get_vec3_of_matrix i
                                                         mat))
                  mat


crossM :: Vec3 -> Mat3
crossM (Vec3 x y z) = Mat3 (Vec3 0 (-z) y)
                           (Vec3 z 0 (-x))
                           (Vec3 (-y) x 0)

cross :: Vec3 -> Vec3 -> Vec3 -- Векторное произведение.
cross v1 v2 = product_of_matr3_by_vec3 (crossM v1) v2


product_of_matr2_by_vec2 :: Mat2 -> Vec2 -> Vec2
product_of_matr2_by_vec2 (Mat2 v1 v2) vec =
  Vec2 (dot v1 vec) (dot v2 vec)

product_of_matr3_by_vec3 :: Mat3 -> Vec3 -> Vec3
product_of_matr3_by_vec3 (Mat3 v1 v2 v3) vec =
  Vec3 (dot v1 vec) (dot v2 vec) (dot v3 vec)

product_of_matr4_by_vec4 :: Mat4 -> Vec4 -> Vec4
product_of_matr4_by_vec4 (Mat4 v1 v2 v3 v4) vec =
  Vec4 (dot v1 vec) (dot v2 vec) (dot v3 vec) (dot v4 vec)


normalize_matr3 :: Mat3 -> Mat2
normalize_matr3 (Mat3 (Vec3 x1 y1 _)
                      (Vec3 x2 y2 _)
                      _) =
  Mat2 (Vec2 x1 y1) (Vec2 x2 y2)

normalize_matr4 :: Mat4 -> Mat3
normalize_matr4 (Mat4 (Vec4 x1 y1 z1 _)
                      (Vec4 x2 y2 z2 _)
                      (Vec4 x3 y3 z3 _)
                      _) =
  Mat3 (Vec3 x1 y1 z1) (Vec3 x2 y2 z2) (Vec3 x3 y3 z3)