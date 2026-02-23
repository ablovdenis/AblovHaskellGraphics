module ThickLine (thickLine, thickBrokenLine) where

import Graphics.Gloss

-- Это функция, создающая линию толщиной thickness.
thickLine :: Float -> (Float, Float) -> (Float, Float) -> Picture
thickLine thickness (x0, y0) (x, y) = Polygon listPoints
  where
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
          cosAlph = if dx >= 0 then (1 / sqrt (tgAlph * tgAlph + 1))
                    else (- 1 / sqrt (tgAlph * tgAlph + 1))
          sinAlph = tgAlph * cosAlph
          dxOB = ticDiv2 * sinAlph
          dyOB = - ticDiv2 * cosAlph

-- Это функция, создающая ломанную толщиной thickness.
thickBrokenLine :: Float -> [(Float, Float)] -> Picture
thickBrokenLine thickness points = Pictures $ helpFunc points
  where
    helpFunc [p0, p] = [thickLine thickness p0 p]
    helpFunc ((x0, y0) : otherPoints@((x, y) : _)) =
      (thickLine thickness (x0, y0) (x, y)) :
      helpFunc otherPoints
    helpFunc _ = [Blank]