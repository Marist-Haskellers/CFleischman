{-
ws = ([1..], [])

type ListZipper a = ([a], [a])

goFoward (x:xs, bs) = (xs, x:bs)
goBack (xs, b:bs) = (b:xs, bs)

getCurrent $ goFoward $ goFoward $ goFoward ws

:{
changeValue :: ListZipper a -> a -> ListZipper a
changeValue (x:xs, bs) y = (y:ys, bs)
:}

getCurrent ws

goFoward ws
([5,6,7,8], [4,3,2,1,0])

goBack (changeValue (goFoward ws) 10)
([4,10,6,7,8],[3,2,1,0])
-}