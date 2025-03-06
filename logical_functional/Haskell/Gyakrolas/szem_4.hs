-- tipusok
-- f x = x : f(f x)   helytelen


-- iterate' :: (a -> a) -> a -> [a]
iterate' f x = x : iterate' f (f x)


-- sqrt(a) : x0 = 1
        -- x(n+1) = 1/2(xn + a/xn)

sqrt' a = head $ dropWhile(\ z -> abs(z*z -a) >= 1e-20) ss
    where
        f x = (x + a/x) / 2
        ss = iterate' f 1 


sqrt'' a = fst $ head $ dropWhile(\ (x, xp) -> abs(x - xp) > 1e-20) ss2
    where
        f x = (x + a/x) / 2
        ss = iterate' f 1 
        ss2 = zip ss (tail ss)

-- folr/l

-- foldr (:) [] lista     => lista

-- foldl (\s x -> x:s) [] lista   => forditott lista
-- reverse' = foldl (\s x -> x:s) []



-- expo
-- e^z = SUM{k=0, inf} z^k/(k!)
expo' z =  foldr (\ (y,_) s -> s + y) 0 $ takeWhile (\(y,_) -> abs(y) > 1e-80) ss
    where
        f (x,kp) = (x*z/kp, kp+1)
        ss = iterate' f (1,1) 



data Either a b = Left a | Right b

-- Either () () = Bool ?    -> igaz
-- from :: Either () () -> Bool
-- from (Left ()) = True
-- from (Right ()) = False

-- to :: Bool -> Either () ()
-- to False = Right ()
-- to True = Left ()
