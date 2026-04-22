module MyGraphics.Figure where



import Math.Matrix (Mat4)
import Math.Vector (Vec3)


data MyPath = MyPath [Vec3] -- Последовательность точек.
                     Vec3 -- Цвет, разбитый на составляющие RGB.
                     Float -- Толщина линии.
  deriving (Show)

data Model = Model [MyPath] -- Составляющие рисунка.
                   Mat4 -- Модельная матрица.
  deriving (Show)