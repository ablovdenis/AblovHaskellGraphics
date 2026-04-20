module MyGraphics.ThickLine (thickLine, thickBrokenLine, thickBrokenLinePairs) where



import Math.Vector (Vec2(..))

import Graphics.Gloss (Picture(Pictures, Polygon))


-- Это функция, создающая линию толщиной thickness.
thickLine :: Float -> Vec2 -> Vec2 -> Picture
thickLine thickness v0 v = Polygon listPoints
  where
    Vec2 x0 y0 = v0
    Vec2 x y = v
    dx = x - x0
    ticDiv2 = thickness / 2
    listPoints =
      if dx == 0
      then [(x0 - ticDiv2, y0),
            (x0 + ticDiv2, y0),
            (x + ticDiv2, y),
            (x - ticDiv2, y)]
      else [(x0 - dxOB, y0 - dyOB),
            (x0 + dxOB, y0 + dyOB),
            (x + dxOB, y + dyOB),
            (x - dxOB, y - dyOB)]
      where
        dy = y - y0
        tgAlph = dy / dx
        cosAlph = if dx >= 0 then 1 / sqrt (tgAlph * tgAlph + 1)
                  else (- (1 / sqrt (tgAlph * tgAlph + 1)))
        sinAlph = tgAlph * cosAlph
        dxOB = ticDiv2 * sinAlph
        dyOB = - (ticDiv2 * cosAlph)

-- Это функция, создающая ломанную толщиной thickness (реализация через хвостовую рекурсию).
thickBrokenLine :: Float -> [Vec2] -> Picture
thickBrokenLine thickness lst_vec2 = Pictures $ helpFunc lst_vec2 []
  where
    helpFunc (v0 : otherPoints@(v : _)) accum =
      helpFunc otherPoints (thickLine thickness v0 v : accum)
    helpFunc _ accum = accum

-- -- Это функция, создающая ломанную толщиной thickness.
-- thickBrokenLine :: Float -> [Vec2] -> Picture
-- thickBrokenLine thickness lst_vec2 = Pictures $ helpFunc lst_vec2
--   where
--     helpFunc [v0, v] = [thickLine thickness v0 v]
--     helpFunc (v0 : otherPoints@(v : _)) =
--       thickLine thickness v0 v : helpFunc otherPoints
--     helpFunc _ = []

-- Эта функция, создающая отрезки толщиной thickness по точкам их концов.
thickBrokenLinePairs :: Float -> [(Vec2, Vec2)] -> Picture
thickBrokenLinePairs thickness lst_pairs = Pictures $ helpFunc lst_pairs []
  where
    helpFunc [] accum = accum
    helpFunc ((v0, v) : otherLine) accum =
      helpFunc otherLine (thickLine thickness v0 v : accum)