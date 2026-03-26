module MyGraphics.ToLeftCoordSyst (toLeftCoord) where



import Math.Vector (Vec3(..))
import Math.Matrix (Mat3(..))


-- Функция преобразования координатной системы Gloss (правая с началом
-- в центре окна) в левую координатную систему (как в лабораторных).
toLeftCoord :: Float -> Float -> Mat3
toLeftCoord width height = Mat3 (Vec3 1 0 (- width / 2))
                                (Vec3 0 (-1) (height / 2))
                                (Vec3 0 0 1)