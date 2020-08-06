aplicarDnv :: (a->a) -> a -> a
aplicarDnv f a = f (f a)


mapp :: (a -> b) -> [a] -> [b]
mapp f [] = []
mapp f (h:t) = (f h) : map f t


filterr :: (a -> Bool) -> [a] -> [a  vs]
filterr f [] = []
filterr f (h:t) | f h = h:filterr f t
                | otherwise = filterr f t 