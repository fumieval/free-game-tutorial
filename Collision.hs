{-# LANGUAGE DeriveFunctor, MultiParamTypeClasses, RankNTypes #-}
module Collision (guard', Figure(..), collisions, Basic(..)) where
import Data.Vect
import Control.Applicative
import Data.Foldable
import Data.Monoid

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
    | LineSegment Vec2 v
    | Custom (Basic Vec2 -> Maybe v) deriving Functor

class PrimitiveFigure v p where
    collision :: Alternative f => p v -> p v -> f v

guard' :: Alternative f => Bool -> a -> f a
guard' False _ = empty
guard' True a = pure a

instance PrimitiveFigure Vec2 Basic where
    collision (Custom f) p = case f p of
        Just a -> pure a
        Nothing -> empty
    collision (Circle r p) (Circle r' q) = guard' (normsqr (p &- q) < (r + r') ^ 2)
        $ (p &+ q) &* 0.5
    collision (LineSegment v p) (LineSegment w q) = guard' (0 <= s && s <= 1 && 0 <= t && t <= 1)
        $ p &+ s *& v
        where
            r = q &- p
            s = cross r v / cross v w
            t = cross r w / cross v w
            cross (Vec2 x0 y0) (Vec2 x1 y1) = x0 * y1 - x1 * y0
    collision (Circle r p) (LineSegment v q)
        = guard' (normsqr qp < r ^ 2) q
        <|> guard' (normsqr q'p < r ^ 2) q'
        <|> guard' (s > 0 && t < 0 && normsqr (qp &- v') < r ^ 2) (q &+ v')
        where
            q' = q &+ v
            qp = p &- q
            q'p = p &- q'
            s = qp &. v
            t = q'p &. v
            v' = (s / normsqr v) *& v
    collision l r = collision r l
