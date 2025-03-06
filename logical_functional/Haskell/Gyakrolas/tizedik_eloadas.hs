data C = C Double Double
 
instance Show C where
    show (C r i) = show r ++ " + i*" ++ show i
 
 
instance Num C where
    (C r1 i1) + (C r2 i2) = C (r1+r2) (i1 + i2)
    (C r1 i1) * (C r2 i2) = C (r1*r2 - i1*i2) (r1*i2 + r2*i1)
    (C r1 i1) - (C r2 i2) = C (r1-r2) (i1 - i2)
    fromInteger z = (C (fromInteger z) 0)
   
instance Enum C where
    succ (C r im) = C (r+1) im
    enumFrom k = k : enumFrom (succ k)
    enumFromTo k1@(C r1 im1) k2@(C r2 im2)
        | r1 > r2   = []
        | otherwise = k1 : enumFromTo (succ k1) k2