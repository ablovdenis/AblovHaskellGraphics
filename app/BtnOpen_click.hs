module BtnOpen_click (btnOpen_click) where



import Figure (MyPath(..))
import Vector (Vec2(..), Vec3(..))


destroy_comments :: String -> String -- Уничтожить комментарии.
destroy_comments "" = ""
destroy_comments (s : st)
  | s == '#' = ""
  | otherwise = s : destroy_comments st


destroy_unnecessary_points :: String -> String -- Уничтожить лишние точки.
destroy_unnecessary_points "." = ""
destroy_unnecessary_points "" = ""
destroy_unnecessary_points (s0 : st@(s1 : _))
  | [s0, s1] == ". " = destroy_unnecessary_points st
  | otherwise = s0 : destroy_unnecessary_points st
destroy_unnecessary_points st = st


btnOpen_click :: [String] -> ((Float, Float), [MyPath])
btnOpen_click [] = ((8.5, 8.5), [])
btnOpen_click data_ = helpFunc (map destroy_comments data_)
                               (MyPath [] (Vec3 0 0 0) 0) []
                               (0, 0)

  where
    helpFunc [] mp list_mypath fr = (fr, mp : list_mypath)
    helpFunc (str : lst) (MyPath lst_v col th) list_mypath fr
      | null str = helpFunc lst (MyPath lst_v col th)
                            list_mypath fr
      -- | head str == '#' = helpFunc lst (MyPath lst_v col th)
      --                              list_mypath
      | take 5 str == "frame" =
        let
          list_to_tuple2 [a, b] = (a, b)
          list_to_tuple2 _ =
            error "Error converting the list to a double tuple."
          coord = (list_to_tuple2 . map (\v -> read v :: Float))
                  (words $ drop 6 str)
        in helpFunc lst (MyPath lst_v col th) list_mypath coord
      | take 5 str == "color" =
        let
          list_to_vec3 [a, b, c] = Vec3 a b c
          list_to_vec3 _ =
            error "Error converting the list to a Vec3."
          rgb = (list_to_vec3 . map (\v -> read v :: Float))
                (words $ drop 6 str)
        in
        helpFunc lst (MyPath lst_v rgb th) list_mypath fr
      | take 9 str == "thickness" =
        let readF s = read s :: Float
        in
        helpFunc lst
                 (MyPath lst_v col
                         (readF $ (head .
                                  words .
                                  drop 10) str))
                 list_mypath fr
      | take 4 str == "path" =
        let
          readI s = read s :: Int
          transformed lst_ 0 read_lst = (lst_, reverse read_lst)
          transformed (st : lst_) n read_lst
            | head st == '#' = transformed lst_ n read_lst
            | otherwise = transformed lst_ (n - 1)
                                      (vec2 : read_lst)
            where
              list_to_vec2 [a, b] = Vec2 a b
              list_to_vec2 _ =
                error "Error converting the list to a Vec2."
              vec2 = (list_to_vec2 . map (\v -> read v :: Float) .
                      words . destroy_unnecessary_points) st
          transformed _ _ _ = error "Coordinate parsing error."
          (droped_lst, result_lst_v) = transformed lst
                                                   ((readI .
                                                    head .
                                                    words .
                                                    drop 5) str)
                                                   []
        in
        helpFunc droped_lst
                 (MyPath [] col th)
                 ((MyPath result_lst_v col th) :
                  list_mypath) fr
      | otherwise = error "Parsing error."