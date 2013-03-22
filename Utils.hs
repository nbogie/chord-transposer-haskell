module Utils where
-- bool fold
bool :: a -> a -> Bool -> a
bool t f b = if b then t else f

