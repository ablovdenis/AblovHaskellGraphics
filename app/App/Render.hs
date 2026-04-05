module App.Render (render) where



import App.World

import Math.Matrix (Mat3, product_of_matrix_by_vector)
import Math.TransForm (my_translate)
import Math.Vector (Vec2(..), Vec3(..), vec3_from_vec2, normalize)

import MyGraphics.Figure (Model(..), MyPath(..))
import MyGraphics.ThickLine (thickBrokenLine, thickBrokenLinePairs)

import Utils.Clip (clip)
import Utils.MapStrict (map')

import Graphics.Gloss

-- import Debug.Trace -- Для тестирования.

render :: World -> IO Picture
render world = return $ Pictures [
  Color black $ Pictures (models_to_pictures $ models_ world),
  button, rectPen,
  edge_check -- Для проверки на правильность расположения границ
             -- прямоугольника относительно формы.
  ]
  where
    edge_check = if minX > maxX || minY > maxY
                 then error "Error edge!" else Blank

    button = Pictures [ -- Кнопка.
      Color white $
            (Translate widthDiv_2_Minu_s30
                       heightDiv_2_Minu_s20
                       $ rectangleSolid 40 20),
      
      Color blue $
            (Translate widthDiv_2_Minu_s30
                       heightDiv_2_Minu_s20
                       $ rectangleWire 40 20),
      Color black $
            (Translate (widthDiv_2_Minu_s30 - 15)
                       (heightDiv_2_Minu_s20 - 5) $
                       (Scale 0.1 0.1 $ Text "Open"))
      ]
      where
        widthDiv_2_Minu_s30 = width_div_2 - 30
        heightDiv_2_Minu_s20 = height_div_2 - 20

    toWindowsSystCoord = vector_transformation $ my_translate (-width_div_2) (-height_div_2)

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

    minX = fromIntegral left :: Float
    maxX = width - fromIntegral right :: Float
    minY = fromIntegral bottom :: Float
    maxY = height - fromIntegral top :: Float

    width = width_ world -- Ширина формы.
    height = height_ world -- Высота формы.

    width_div_2 = width / 2
    height_div_2 = height / 2

    vector_transformation matr v = a
      where
        _a = vec3_from_vec2 v 1
        a = normalize (product_of_matrix_by_vector matr _a)

    mypath_lst_to_picture_lst :: Mat3 -> [MyPath] -> [Picture]
    mypath_lst_to_picture_lst _ [] = []
    mypath_lst_to_picture_lst _tM (mp : lst) =
      (Color col $ thickBrokenLinePairs thick toWindowsSystCoord_lst_points) :
      mypath_lst_to_picture_lst _tM lst
      where
        MyPath lst_vec2 col_vec3 thick = mp
        Vec3 re gr bl = col_vec3
        col = makeColor (re / 255) (gr / 255) (bl / 255) 1
        lst_points = map' (vector_transformation _tM) lst_vec2
        lst_clipped_points = clipping lst_points
        toWindowsSystCoord_lst_points = map (\(v1, v2) -> (toWindowsSystCoord v1, toWindowsSystCoord v2))
                                            lst_clipped_points

        clipping (v1 : lst_@(v2 : _)) =
          case clip v1 v2 minX minY maxX maxY of
            Just x -> x : clipping lst_
            _      -> clipping lst_
        clipping _ = []
    
    models_to_pictures :: [Model] -> [Picture]
    models_to_pictures [] = []
    models_to_pictures (model : models) =
      (Pictures $ mypath_lst_to_picture_lst _tM mp_list)
      : models_to_pictures models
      where
        Model mp_list modelM = model
        _tM = _t_ world * modelM