module Utils.MapStrict (mapT) where



-- map' :: (a1 -> a2) -> [a1] -> [a2]
-- map' _ [] = []
-- map' func (el : tl) =
--   let func_Elem = func el
--   in func_Elem `seq` func_Elem : map' func tl

mapT :: (t -> a) -> [t] -> [a]
mapT func lst = helpFunc lst [] -- Реализация через хвостовую рекурсию. Потом сравню с map'.
  where
    helpFunc [] accum = reverse accum
    helpFunc (el : tl) accum = helpFunc tl (func el : accum)