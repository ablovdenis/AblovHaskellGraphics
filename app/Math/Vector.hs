module Math.Vector where



class ClassVector vec where
  dot :: vec -> vec -> Float -- Скалярное произведение.
  coord_wise_mult :: vec -> vec -> vec
      -- Покоординатное произведение.
  mult_by_num_vec :: Float -> vec -> vec -- Домножение на число.
  length_vec :: vec -> Float -- Получение длины вектора.
  length_vec v = sqrt $ dot v v -- Реализация по-умолчанию.


data Vec2 = Vec2 Float Float
  deriving (Eq, Show)

data Vec3 = Vec3 Float Float Float
  deriving (Eq, Show)

data Vec4 = Vec4 Float Float Float Float
  deriving (Eq, Show)


instance ClassVector Vec2 where
  dot (Vec2 x1 y1) (Vec2 x2 y2) = x1 * x2 + y1 * y2
  coord_wise_mult (Vec2 x1 y1) (Vec2 x2 y2) =
    Vec2 (x1 * x2) (y1 * y2)
  mult_by_num_vec num (Vec2 x y) = Vec2 (x * num) (y * num)

instance ClassVector Vec3 where
  dot (Vec3 x1 y1 z1) (Vec3 x2 y2 z2) =
    x1 * x2 + y1 * y2 + z1 * z2
  coord_wise_mult (Vec3 x1 y1 z1) (Vec3 x2 y2 z2) =
    Vec3 (x1 * x2) (y1 * y2) (z1 * z2)
  mult_by_num_vec num (Vec3 x y z) =
    Vec3 (x * num) (y * num) (z * num)


instance ClassVector Vec4 where
  dot (Vec4 x1 y1 z1 a1) (Vec4 x2 y2 z2 a2) =
    x1 * x2 + y1 * y2 + z1 * z2 + a1 * a2
  coord_wise_mult (Vec4 x1 y1 z1 a1) (Vec4 x2 y2 z2 a2) =
    Vec4 (x1 * x2) (y1 * y2) (z1 * z2) (a1 * a2)
  mult_by_num_vec num (Vec4 x y z a) =
    Vec4 (x * num) (y * num) (z * num) (a * num)


instance Num Vec2 where
  (*) _ _ = error "Функция не определена."
  (+) (Vec2 x1 y1) (Vec2 x2 y2) = Vec2 (x1 + x2) (y1 + y2)
  (-) vec1 vec2 = vec1 + (-vec2)
  negate (Vec2 x y) = Vec2 (-x) (-y)
  abs _ = error "Функция не определена."
  signum _ = error "Функция не определена."
  fromInteger _ = error "Функция не определена."

instance Num Vec3 where
  (*) _ _ = error "Функция не определена."
  (+) (Vec3 x1 y1 z1) (Vec3 x2 y2 z2) =
    Vec3 (x1 + x2) (y1 + y2) (z1 + z2)
  (-) vec1 vec2 = vec1 + (-vec2)
  negate (Vec3 x y z) = Vec3 (-x) (-y) (-z)
  abs _ = error "Функция не определена."
  signum _ = error "Функция не определена."
  fromInteger _ = error "Функция не определена."

instance Num Vec4 where
  (*) _ _ = error "Функция не определена."
  (+) (Vec4 x1 y1 z1 a1) (Vec4 x2 y2 z2 a2) =
    Vec4 (x1 + x2) (y1 + y2) (z1 + z2) (a1 + a2)
  (-) vec1 vec2 = vec1 + (-vec2)
  negate (Vec4 x y z a) = Vec4 (-x) (-y) (-z) (-a)
  abs _ = error "Функция не определена."
  signum _ = error "Функция не определена."
  fromInteger _ = error "Функция не определена."


x_vec2 :: Vec2 -> Float
x_vec2 (Vec2 x _) = x

y_vec2 :: Vec2 -> Float
y_vec2 (Vec2 _ y) = y

change_coord_vec2 :: Int -> Float -> Vec2 -> Vec2 -- Получение изменённого в одной
                                                  -- координате вектора Vec2.
change_coord_vec2 0 value (Vec2 _ y) = Vec2 value y
change_coord_vec2 1 value (Vec2 x _) = Vec2 x value
change_coord_vec2 _ _ _ =
  error "Неправильные параметры для change_coord_vec2."

change_coord_vec3 :: Int -> Float -> Vec3 -> Vec3 -- Получение изменённого в одной
                                                  -- координате вектора Vec3.
change_coord_vec3 0 value (Vec3 _ y c) = Vec3 value y c
change_coord_vec3 1 value (Vec3 x _ c) = Vec3 x value c
change_coord_vec3 2 value (Vec3 x y _) = Vec3 x y value
change_coord_vec3 _ _ _ =
  error "Неправильные параметры для change_coord_vec3."


-- get_coord_of_vec3 :: Int -> Vec3 -> Float -- Получение i-ой координаты вектора.
-- get_coord_of_vec3 0 (Vec3 x _ _) = x
-- get_coord_of_vec3 1 (Vec3 _ y _) = y
-- get_coord_of_vec3 2 (Vec3 _ _ c) = c
-- get_coord_of_vec3 _ _ =
--   error "Неправильные параметры для get_coord_of_vec3."


vec2_from_vec3 :: Vec3 -> Vec2
vec2_from_vec3 (Vec3 x y _) = Vec2 x y

vec3_from_vec2 :: Vec2 -> Float -> Vec3
vec3_from_vec2 (Vec2 x y) z = Vec3 x y z

vec4_from_vec3 :: Vec3 -> Float -> Vec4
vec4_from_vec3 (Vec3 x y z) a = Vec4 x y z a


-- Переход из однородных координат в евклидовы.
-- Исходит из того факта, что однородные координаты (x, y, z)
-- описывают конечные координаты (x / z, y / z).
normalize_vec3 :: Vec3 -> Vec2
normalize_vec3 (Vec3 _ _ 0) =
  error "Деление на 0 при переводе Vec3 в Vec2."
normalize_vec3 (Vec3 x y z) = Vec2 (x / z) (y / z)

normalize_vec4 :: Vec4 -> Vec3
normalize_vec4 (Vec4 _ _ _ 0) =
  error "Деление на 0 при переводе Vec4 в Vec3."
normalize_vec4 (Vec4 x y z a) = Vec3 (x / a) (y / a) (z / a)


-- Нормализация вектора (приведение к
-- сонаправленному единичному вектору).
norm_vec3 :: Vec3 -> Vec3
norm_vec3 vec =
  normalize_vec4 $ vec4_from_vec3 vec $ length_vec vec