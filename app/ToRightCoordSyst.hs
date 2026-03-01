module ToRightCoordSyst (toRightCoord) where

-- Функция преобразования координат координатной системы с началом в центре
-- окна в координаты координатной системы с началом в левом нижнем углу окна.
toRightCoord :: Float -> Float -> (Float, Float) -> (Float, Float)
toRightCoord width height (x, y) = (x - width / 2, y - height / 2)