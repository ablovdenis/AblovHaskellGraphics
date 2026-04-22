module Utils.Clip (clip) where



import Math.Matrix ()
import Math.Vector (Vec2(..), x_vec2, y_vec2)

import Data.Bits ((.&.), (.|.))

-- import Debug.Trace -- Для тестирования.

codeKS :: Vec2 -> Float -> Float -> Float -> Float -> Int
codeKS p_ minX minY maxX maxY = c1 + c2 + c3 + c4
  where
    c1 = if x_vec2 p_ < minX then 1 else 0
    c2 = if x_vec2 p_ > maxX then 2 else 0
    c3 = if y_vec2 p_ < minY then 4 else 0
    c4 = if y_vec2 p_ > maxY then 8 else 0

clip :: Vec2 -> Vec2 -> Float -> Float
        -> Float -> Float -> Maybe (Vec2, Vec2)
clip a_ b_ minX minY maxX maxY = while a_ b_
  where
    while a_vec b_vec
      | codeA .|. codeB -- trace ("(cA, cB)=" ++ show (codeA, codeB)
                        -- ++ "  (a_v, b_v)=" ++ show (a_vec, b_vec))
        == 0 = Just (a_vec, b_vec)
      | codeA .&. codeB /= 0 = Nothing
      | codeA == 0 =
        case while b_vec a_vec of
          Just (v1, v2) -> Just (v2, v1)
          _             -> Nothing
      | codeA .&. 1 /= 0 =
        let
          Vec2 x_a y_a = a_vec -- trace "Flag1" 
          Vec2 x_b y_b = b_vec
        in while (Vec2 minX (y_a + (y_b - y_a) * (minX - x_a)
                             / (x_b - x_a))) b_vec
      | codeA .&. 2 /= 0 =
        let
          Vec2 x_a y_a = a_vec -- trace "Flag2" 
          Vec2 x_b y_b = b_vec
        in while (Vec2 maxX (y_a + (y_b - y_a) * (maxX - x_a)
                             / (x_b - x_a))) b_vec
      | codeA .&. 4 /= 0 =
        let
          Vec2 x_a y_a = a_vec -- trace "Flag4" 
          Vec2 x_b y_b = b_vec
        in while (Vec2 (x_a + (x_b - x_a) * (minY - y_a)
                        / (y_b - y_a)) minY) b_vec
      | otherwise =
        let
          Vec2 x_a y_a = a_vec -- trace "Flag8" 
          Vec2 x_b y_b = b_vec
        in while (Vec2 (x_a + (x_b - x_a) * (maxY - y_a)
                        / (y_b - y_a)) maxY) b_vec
      where
        codeA = codeKS a_vec minX minY maxX maxY
        codeB = codeKS b_vec minX minY maxX maxY