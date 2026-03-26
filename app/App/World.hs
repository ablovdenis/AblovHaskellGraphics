module App.World where



import Math.Matrix (Mat3(..))
import MyGraphics.Figure (MyPath(..))


data World = World Float -- Ширина окна.
                   Float -- Высота окна.
                   [MyPath] -- Хранит картинку в виде списка
                               -- конфигурационных объектов.
                   Mat3 -- Матрица, в которой накапливаются все преобразования.
                   (Float, Float) -- Размер изображения (Vx и Vy).


get_width :: World -> Float
get_width (World w _ _ _ _) = w


get_height :: World -> Float
get_height (World _ h _ _ _) = h


get_mypath_list :: World -> [MyPath]
get_mypath_list (World _ _ mp_lst _ _) = mp_lst


get_T_matrix :: World -> Mat3
get_T_matrix (World _ _ _ t_ _) = t_


get_frame_size :: World -> (Float, Float)
get_frame_size (World _ _ _ _ frame_size) = frame_size


change_width_world :: Float -> World -> World
change_width_world w (World _ h mp_lst t_ frame_size) = World w h mp_lst t_ frame_size


change_height_world :: Float -> World -> World
change_height_world h (World w _ mp_lst t_ frame_size) = World w h mp_lst t_ frame_size


change_mp_lst_world :: [MyPath] -> World -> World
change_mp_lst_world mp_lst (World w h _ t_ frame_size) = World w h mp_lst t_ frame_size


change_T_matrix_world :: Mat3 -> World -> World
change_T_matrix_world t_ (World w h mp_lst _ frame_size) = World w h mp_lst t_ frame_size


change_frame_size_world :: (Float, Float) -> World -> World
change_frame_size_world frame_size (World w h mp_lst t_ _) = World w h mp_lst t_ frame_size