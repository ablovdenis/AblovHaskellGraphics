module Figure where



import Vector


data MyPath = MyPath [Vec2] -- Последовательность точек.
                     Vec3 -- Цвет, разбитый на составляющие RGB.
                     Float -- Толщина линии.
  deriving (Show)