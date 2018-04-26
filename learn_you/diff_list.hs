import Control.Monad.Writer

newtype DiffList a = DiffList { getDiffList :: [a] -> [a] }

instance Monoid (DiffList a) where
    mempty = DiffList ([] ++)
    mappend (DiffList f) (DiffList g) = DiffList (f . g)

toDiffList :: [a] -> DiffList a
toDiffList xs = DiffList (xs ++)

fromDiffList :: DiffList a -> [a]
fromDiffList (DiffList f) = f []

finalCountDown :: Int -> Writer (DiffList String) ()
finalCountDown 0 = do
    tell (toDiffList ["0"])
finalCountDown x = do
    finalCountDown (x - 1)
    tell (toDiffList [show x])

main = mapM_ putStrLn . fromDiffList . snd . runWriter $ finalCountDown 50000
