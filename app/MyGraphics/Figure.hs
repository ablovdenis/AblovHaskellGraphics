module MyGraphics.Figure where



import Math.Matrix (Mat3)
import Math.Vector (Vec2, Vec3)


data MyPath = MyPath [Vec2] -- Последовательность точек.
                     Vec3 -- Цвет, разбитый на составляющие RGB.
                     Float -- Толщина линии.
  deriving (Show)

data Model = Model [MyPath] -- Составляющие рисунка.
                   Mat3 -- Модельная матрица.
  deriving (Show)