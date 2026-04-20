module App.Render (render) where



import App.World (World(..), ProjType(..))

import Math.Vector (Vec2(..), Vec3(..),
                    vec3_from_vec2, vec4_from_vec3, vec2_from_vec3,
                    normalize_vec3, normalize_vec4)
import Math.Matrix (product_of_matr3_by_vec3,
                    product_of_matr4_by_vec4)
import Math.TransForm (my_translate_2,
                       ortho, frustum, perspective, cadrRL)


import MyGraphics.Figure (MyPath(..), Model(..))
import MyGraphics.ThickLine (thickBrokenLine, thickBrokenLinePairs)

import Utils.Clip (clip)
import Utils.MapStrict (mapT)

import Graphics.Gloss

-- import Debug.Trace -- Для тестирования.

render :: World -> IO Picture
render world = do
  -- print $ models_ world
  return $ Pictures [
    Color black $ Pictures (models_to_pictures (models_ world)),
    button, rectPen,
    edge_check -- Для проверки на правильность расположения границ
              -- прямоугольника относительно формы.
    ]
  where
    edge_check =
      if minX > maxX || minY > maxY
      then error "Слишком маленький размер окна приложения.!"
      else Blank
      -- Костыльное ограничение на изменение размера окна приложения.

    button = Pictures [ -- Кнопка.
      Color white $
            (Translate widthDiv_2_minus_30
                       heightDiv_2_Minu_s20
                       $ rectangleSolid 40 20),
      Color blue $
            (Translate widthDiv_2_minus_30
                       heightDiv_2_Minu_s20
                       $ rectangleWire 40 20),
      Color black $
            (Translate (widthDiv_2_minus_30 - 15)
                       (heightDiv_2_Minu_s20 - 5) $
                       (Scale 0.1 0.1 $ Text "Open"))
      ]
      where
        widthDiv_2_minus_30 = width_div_2 - 30
        heightDiv_2_Minu_s20 = height_div_2 - 20

    rectPen = -- Область видимости рисунка.
      Color black $
        thickBrokenLine 2 $
        map toWindowsSystCoord
          [vec2_leftFloat_topFloat,
            Vec2 maxX minY,
            Vec2 maxX maxY,
            Vec2 minX maxY,
            vec2_leftFloat_topFloat]
      where vec2_leftFloat_topFloat = Vec2 minX minY

    left = left_ world
    right = right_ world
    top = top_ world
    bottom = bottom_ world

    width = width_ world -- Ширина формы.
    height = height_ world -- Высота формы.
    
    minX = fromIntegral left :: Float
    maxX = width - fromIntegral right :: Float
    minY = fromIntegral bottom :: Float
    maxY = height - fromIntegral top :: Float

    width_div_2 = width / 2
    height_div_2 = height / 2
    
    _Wx = maxX - minX
    _Wy = maxY - minY
    _Wcx = minX
    _Wcy = minY

    vector_transformation matr v = a
      where
        _a = vec3_from_vec2 v 1
        a = normalize_vec3 (product_of_matr3_by_vec3 matr _a)
    
    toWindowsSystCoord =
      vector_transformation $ my_translate_2 (-width_div_2)
                                             (-height_div_2)

    

    proj
      | pT == Ortho = ortho l r b t (-n) (-f)
      | pT == Frustum = frustum l r b t n f
      | otherwise =
        perspective (fovy_work_ world) (aspect_work_ world) n f
      where
        pT = pType_ world
        l = l_ world
        r = r_ world
        b = b_ world
        t = t_ world
        n = n_ world
        f = f_ world

    cdr = cadrRL (Vec2 (-1) (-1)) (Vec2 2 2)
                 (Vec2 _Wcx _Wcy) (Vec2 _Wx _Wy)
      -- Матрица кадрирования.
    
    _C = proj * _T_ world
      -- Матрица перехода от мировых координат в
      -- пространство отсечения.

    figure_to_picture_lst _TM figure = cycle1 figure []
      where
        cycle1 [] accum = accum
        cycle1 (lines1 : other_lines) accum =
          cycle1 other_lines
                 ((Color
                     col
                     (thickBrokenLinePairs
                       thick
                       toWindowsSystCoord_lst_points)) : accum)
          where
            MyPath lst_vec3 col_vec3 thick = lines1
            Vec3 re gr bl = col_vec3
            col = makeColor (re / 255) (gr / 255) (bl / 255) 1
            processed_3D vec3 =
              normalize_vec4
                (product_of_matr4_by_vec4
                  _TM (vec4_from_vec3 vec3 1))
            _3D_to_2D vec3 =
              normalize_vec3
                (product_of_matr3_by_vec3
                  cdr (vec3_from_vec2
                        (vec2_from_vec3
                          (processed_3D vec3)) 1))
            lst_vec2 = mapT _3D_to_2D lst_vec3
            lst_clipped_points = clipping lst_vec2
            toWindowsSystCoord_lst_points =
              mapT (\(v1, v2) -> (toWindowsSystCoord v1,
                                  toWindowsSystCoord v2))
                   lst_clipped_points
            clipping (v1 : lst_@(v2 : _)) =
              case clip v1 v2 minX minY maxX maxY of
                Just x -> x : clipping lst_
                _      -> clipping lst_
            clipping _ = []
    
    models_to_pictures :: [Model] -> [Picture]
    models_to_pictures [] = []
    models_to_pictures (model : models) =
      (Pictures $ figure_to_picture_lst _TM figure)
      : models_to_pictures models
      where
        Model figure modelM = model
        _TM = _C * modelM
