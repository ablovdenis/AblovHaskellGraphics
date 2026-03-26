module Math.Vector where



data Vec2 = Vec2 Float Float
  deriving (Eq, Show)


x_vec2 :: Vec2 -> Float
x_vec2 (Vec2 x _) = x


y_vec2 :: Vec2 -> Float
y_vec2 (Vec2 _ y) = y


-- Однородные координаты (это координаты, обладающие тем свойством, что определяемый
-- ими объект не меняется, когда все координаты умножаются на одно и то же число;
-- в нашем случае - на третий параметр Vec3).

-- Третий параметр нужен для возможности представлять операцию параллельного
-- переноса в виде матрицы.
data Vec3 = Vec3 Float Float Float deriving (Eq, Show)


change_coord_vec3 :: Int -> Float -> Vec3 -> Vec3 -- Получение изменённого в одной
                                                  -- координате вектора Vec3.
change_coord_vec3 0 value (Vec3 _ y c) = Vec3 value y c
change_coord_vec3 1 value (Vec3 x _ c) = Vec3 x value c
change_coord_vec3 2 value (Vec3 x y _) = Vec3 x y value
change_coord_vec3 _ _ _ =
  error "Неправильные параметры для change_coord_vec3."


instance Num Vec3 where
  (*) (Vec3 x1 y1 c1) (Vec3 x2 y2 c2) = Vec3 (x1 * x2)
                                             (y1 * y2)
                                             (c1 * c2)
  (+) (Vec3 x1 y1 c1) (Vec3 x2 y2 c2) = Vec3 (x1 + x2)
                                             (y1 + y2)
                                             (c1 + c2)
  (-) vec1 vec2 = vec1 + (-vec2)
  negate (Vec3 x y c) = Vec3 (-x) (-y) (-c)
  abs (Vec3 x y c) = Vec3 (sqrt (x ** 2 + y ** 2 + c ** 2)) 0 0
  signum _ = Vec3 0 0 0
  fromInteger _ = Vec3 0 0 0


get_coord_of_vec3 :: Int -> Vec3 -> Float -- Получение i-ой координаты вектора.
get_coord_of_vec3 0 (Vec3 x _ _) = x
get_coord_of_vec3 1 (Vec3 _ y _) = y
get_coord_of_vec3 2 (Vec3 _ _ c) = c
get_coord_of_vec3 _ _ =
  error "Неправильные параметры для get_coord_of_vec3."


vec3_from_vec2 :: Vec2 -> Float -> Vec3
vec3_from_vec2 (Vec2 x y) c = Vec3 x y c


-- Переход из однородных координат в евклидовы.
-- Исходит из того факта, что однородные координаты (x, y, c)
-- описывают конечные координаты (x / c, y / c).
normalize :: Vec3 -> Vec2
normalize (Vec3 x y c) = Vec2 (x / c) (y / c)


dot :: Vec3 -> Vec3 -> Float -- Скалярное произведение векторов.
dot (Vec3 x1 y1 c1) (Vec3 x2 y2 c2) = x1 * x2 + y1 * y2 + c1 * c2
