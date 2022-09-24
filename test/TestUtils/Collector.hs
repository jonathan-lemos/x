module TestUtils.Collector where


data Collector a b = Collector [a] b

instance Functor (Collector a) where
    fmap f (Collector a b) = Collector a (f b)

instance Applicative (Collector a) where
    pure = Collector []
    (Collector xa f) <*> (Collector ya b) = Collector (xa <> ya) (f b)

instance Monad (Collector a) where
    (Collector as b) >>= f =
        let (Collector cs d) = f b
         in Collector (as <> cs) d

singleton :: a -> Collector a ()
singleton x = Collector [x] ()

getList :: Collector a b -> [a]
getList (Collector as _) = as

modifyLast :: (a -> a) -> Collector a b -> Collector a b
modifyLast f (Collector list val) =
    let modifyListLast [] = []
        modifyListLast [x] = [f x]
        modifyListLast (x : xs) = x : modifyListLast xs
     in Collector (modifyListLast list) val

type Title = String

class ModifyAssertionTitle a where
    modifyAssertionTitle :: a -> Title -> a

withTitle :: (ModifyAssertionTitle a) => Collector a b -> Title -> Collector a b
withTitle c title =
    modifyLast (`modifyAssertionTitle` title) c

