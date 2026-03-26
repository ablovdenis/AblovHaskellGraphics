module Math.Matrix where

import Math.Vector (Vec3(..), change_coord_vec3, dot)



data Mat3 = Mat3 Vec3 Vec3 Vec3 deriving (Eq)


change_vec_mat3 :: Int -> Vec3 -> Mat3 -> Mat3 -- Получение изменённой в одном
                                               -- векторе матрицы Mat3.
change_vec_mat3 0 vec (Mat3 _ v1 v2) = Mat3 vec v1 v2
change_vec_mat3 1 vec (Mat3 v0 _ v2) = Mat3 v0 vec v2
change_vec_mat3 2 vec (Mat3 v0 v1 _) = Mat3 v0 v1 vec
change_vec_mat3 _ _ _ =
  error "Неправильные параметры для change_vec_mat3."


create_diag_matrix :: Float -> Mat3
create_diag_matrix a = Mat3 (Vec3 a 0 0)
                            (Vec3 0 a 0)
                            (Vec3 0 0 a)


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


transpose :: Mat3 -> Mat3
transpose mat3 = cycleFunc 0 mat3 ([], [], [])
                            -- Реализация функции 
                            -- через цикло-рекурсию.
  where
    rev_lst_to_vec3 [c, y, x] = Vec3 x y c
    rev_lst_to_vec3 _ =
      error "Неправильные параметры для rev_lst_to_vec3."

    cycleFunc 3 _ (lst1, lst2, lst3) =
      Mat3 (rev_lst_to_vec3 lst1)
           (rev_lst_to_vec3 lst2)
           (rev_lst_to_vec3 lst3)
    cycleFunc i mat (lst1, lst2, lst3) =
      cycleFunc (i + 1) mat (x : lst1, y : lst2, c : lst3)
      where
        Vec3 x y c = get_vec3_of_matrix i mat3


product_of_matrix_by_vector :: Mat3 -> Vec3 -> Vec3
product_of_matrix_by_vector mat3 vec3 = Vec3 dot0 dot1 dot2
  where
    dot0 = dot (get_vec3_of_matrix 0 mat3) vec3
    dot1 = dot (get_vec3_of_matrix 1 mat3) vec3
    dot2 = dot (get_vec3_of_matrix 2 mat3) vec3


instance Num Mat3 where
  (*) mat1 mat2 = transpose $ Mat3 v0 v1 v2
    where
      Mat3 col0_of_mat2 col1_of_mat2 col2_of_mat2 = transpose mat2
      v0 = product_of_matrix_by_vector mat1 col0_of_mat2
      v1 = product_of_matrix_by_vector mat1 col1_of_mat2
      v2 = product_of_matrix_by_vector mat1 col2_of_mat2
  (+) (Mat3 vec1_0 vec1_1 vec1_2)
      (Mat3 vec2_0 vec2_1 vec2_2) =
        Mat3 (vec1_0 + vec2_0)
             (vec1_1 + vec2_1)
             (vec1_2 + vec2_2)
  negate (Mat3 v1 v2 v3) = Mat3 (-v1) (-v2) (-v3)
  (-) mat1 mat2 = mat1 + (-mat2)
  abs _ = create_diag_matrix 0
  signum _ = create_diag_matrix 0
  fromInteger value = create_diag_matrix $ fromInteger value


instance Show Mat3 where
  show (Mat3 v0 v1 v2) = "Mat3[\n|" ++ show v0 ++ "\n|" ++
                         show v1 ++ "\n|" ++ show v2 ++ "\n]"
