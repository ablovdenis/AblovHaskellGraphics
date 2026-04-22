module App.World where



import Math.Vector (Vec3(..))
import Math.Matrix (Mat4(..))
import MyGraphics.Figure (Model(..))


data ProjType = Ortho | Frustum | Perspective
  deriving (Show, Eq, Ord)


data World = World {
                   width_ :: Float,      -- Ширина окна.
                   height_ :: Float,     -- Высота окна.

                   models_ :: [Model],   -- Описание списка рисунков.
                   _T_ :: Mat4,          -- Матрица, в которой накапливаются все преобразования.
                   _S_ :: Vec3,          -- Коорд-ы точки наблюдения.
                   _P_ :: Vec3,          -- Коорд-ы точки, в которую
                                         -- направлен вектор наблюдения.
                   u_ :: Vec3,           -- Коорд-ы вектора направления вверх.
                   dist_ :: Float,       -- Расстояние между S и P.
                   fovy_ :: Float,       -- Угол обзора.
                   aspect_ :: Float,     -- Соотношение сторон окна наблюдения.
                   fovy_work_ :: Float,  -- Рабочая переменная для fovy.
                   aspect_work_ :: Float,-- Рабочая переменная для aspect.
                   near_ :: Float,       -- Расстояние до окна наблюдения.
                   far_ :: Float,        -- Расстояние до горизонта.
                   n_ :: Float,          -- Рабочая переменная для near.
                   f_ :: Float,          -- Рабочая переменная для far.
                   l_ :: Float,
                   r_ :: Float,
                   t_ :: Float,
                   b_ :: Float,          -- Рабочие вспомогательные переменные
                                         -- для значений координат левой, правой,
                                         -- нижней и верхней координаты в СКН.
                   pType_ :: ProjType,   -- Тип трехмерной проекции.

                   initT_ :: Mat4,       -- Матрица начального преобразования.
                   left_ :: Int,
                   right_ :: Int,
                   top_ :: Int,
                   bottom_ :: Int        -- Расстояния до границ окна.
                   } deriving (Show)