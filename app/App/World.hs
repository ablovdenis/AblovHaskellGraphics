module App.World where



import Math.Matrix (Mat3(..))
import MyGraphics.Figure (Model(..))


data World = World {
                   width_ :: Float,     -- Ширина окна.
                   height_ :: Float,    -- Высота окна.
                   _vx_ :: Float,       -- Размер рисунка по горизонтали.
                   _vy_ :: Float,       -- Размер рисунка по вертикали.
                   aspectFig_ :: Float, -- Соотношение сторон рисунка.
                   models_ :: [Model],  -- Описание списка рисунков.
                   _t_ :: Mat3,         -- Матрица, в которой накапливаются все преобразования.
                   initT_ :: Mat3,      -- Матрица начального преобразования.
                   left_ :: Int,
                   right_ :: Int,
                   top_ :: Int,
                   bottom_ :: Int       -- Расстояния до границ окна.
                   } deriving (Show)


change_width_world :: Float -> World -> World
change_width_world width1
  (World _ height0
         _vx0 _vy0
         aspectFig0
         models0
         _t0 initT0
         left0 right0 top0 bottom0) =
  World width1 height0
        _vx0 _vy0
        aspectFig0
        models0
        _t0 initT0
        left0 right0 top0 bottom0


change_height_world :: Float -> World -> World
change_height_world height1
  (World width0 _
         _vx0 _vy0
         aspectFig0
         models0
         _t0 initT0
         left0 right0 top0 bottom0) =
  World width0 height1
        _vx0 _vy0
        aspectFig0
        models0
        _t0 initT0
        left0 right0 top0 bottom0

change_frame_size_world :: (Float, Float) -> World -> World
change_frame_size_world (_vx1, _vy1)
  (World width0 height0
         _ _
         aspectFig0
         models0
         _t0 initT0
         left0 right0 top0 bottom0) =
  World width0 height0
        _vx1 _vy1
        aspectFig0
        models0
        _t0 initT0
        left0 right0 top0 bottom0

change_models_world :: [Model] -> World -> World
change_models_world models1
  (World width0 height0
         _vx0 _vy0
         aspectFig0
         _
         _t0 initT0
         left0 right0 top0 bottom0) =
  World width0 height0
        _vx0 _vy0
        aspectFig0
        models1
        _t0 initT0
        left0 right0 top0 bottom0


change_T_world :: Mat3 -> World -> World
change_T_world _t1
  (World width0 height0
         _vx0 _vy0
         aspectFig0
         models0
         _ initT0
         left0 right0 top0 bottom0) =
  World width0 height0
        _vx0 _vy0
        aspectFig0
        models0
        _t1 initT0
        left0 right0 top0 bottom0

change_initT_world :: Mat3 -> World -> World
change_initT_world initT1
  (World width0 height0
         _vx0 _vy0
         aspectFig0
         models0
         _t0 _
         left0 right0 top0 bottom0) =
  World width0 height0
        _vx0 _vy0
        aspectFig0
        models0
        _t0 initT1
        left0 right0 top0 bottom0