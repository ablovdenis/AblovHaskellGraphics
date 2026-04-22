module App.Update (update) where



update :: p -> world -> IO world
update _ world = return world