{-# LANGUAGE DeriveFunctor, MultiParamTypeClasses, RankNTypes, FlexibleInstances #-}
module Collision (guard', Figure(..), collisions, Basic(..)) where
import Control.Applicative
import Data.Foldable
import Data.Monoid
import Linear

data Figure p v = Primitive (p v) | Figures [Figure p v] | Intersect (Figure p v) (Figure p v) deriving Functor

instance Monoid (Figure p v) where
    mappend p q = Figures [p, q]
    mempty = Figures []

collisions :: (PrimitiveFigure v p, Alternative f) => Figure p v -> Figure p v -> f v
collisions (Primitive p) (Primitive q) = collision p q
collisions p (Intersect q r) = collisions p q *> collisions p r
collisions (Intersect q r) p = collisions p q *> collisions p r
collisions p@(Primitive _) (Figures fs) = asum $ collisions p <$> fs
collisions (Figures fs) q@(Primitive _) = asum $ flip collisions q <$> fs
collisions (Figures fs) (Figures gs) = asum $ collisions <$> fs <*> gs

data Basic v = Circle Float v
    | LineSegment (V2 Float) v
    | Custom (Basic (V2 Float) -> Maybe v) deriving Functor

class PrimitiveFigure v p where
    collision :: Alternative f => p v -> p v -> f v

guard' :: Alternative f => Bool -> a -> f a
guard' False _ = empty
guard' True a = pure a

instance PrimitiveFigure (V2 Float) Basic where
    collision (Custom f) p = case f p of
        Just a -> pure a
        Nothing -> empty
    collision (Circle r p) (Circle r' q) = guard' (distance p q < r + r')
        $ (p ^+^ q) ^* 0.5
    collision (LineSegment v p) (LineSegment w q) = guard' (0 <= s && s <= 1 && 0 <= t && t <= 1)
        $ p ^+^ s *^ v
        where
            r = q ^-^ p
            s = cross r v / cross v w
            t = cross r w / cross v w
            cross (V2 x0 y0) (V2 x1 y1) = x0 * y1 - x1 * y0
    collision (Circle r p) (LineSegment v q)
        = guard' (norm qp < r) q
        <|> guard' (norm q'p < r) q'
        <|> guard' (s > 0 && t < 0 && distance qp v' < r) (q ^+^ v')
        where
            q' = q ^+^ v
            qp = p ^-^ q
            q'p = p ^-^ q'
            s = dot qp v
            t = dot q'p v
            v' = (s / norm v) *^ normalize v
    collision l r = collision r l
