{-# LANGUAGE TemplateHaskell #-}
module Types where
import Data.Vect
import Graphics.FreeGame
import Control.Monad.State
import Control.Applicative
import Control.Lens
import Collision

type Fig = Figure Basic Vec2

data Block = Block
    { _blockPos :: Vec2
    , _blockNumber :: Integer
    }
makeLenses ''Block

blockShape :: Getter Block Fig
blockShape = blockPos . to fig where
    fig p@(Vec2 px py) = Primitive $ Custom $ \f -> case f of
        s@(Circle r q@(Vec2 x y))
            | x0 < x && x < x1 -> guard' (abs (y0 - 4 - y) < r) (Vec2 x y0) <|> guard' (abs (y - y1 - 4) < r) (Vec2 x y1)
            | y0 < y && y < y1 -> guard' (abs (x0 - 4 - x) < r) (Vec2 x0 y) <|> guard' (abs (x - x1 - 4) < r) (Vec2 x1 y)
            | otherwise -> collisions (Primitive s) $ Figures $ map Primitive
                [ Circle 8 (Vec2 x0 y0)
                , Circle 8 (Vec2 x1 y0)
                , Circle 8 (Vec2 x0 y1)
                , Circle 8 (Vec2 x1 y1)]
        _ -> error "unsupported operation"
        where
            x0 = px - 32
            x1 = px + 32
            y0 = py - 8
            y1 = py + 8

data Pack = Pack
    { _packPos :: Vec2
    , _packVel :: Vec2
    , _packNumber :: Integer
    }

makeLenses ''Pack

packShape :: Getter Pack Fig
packShape = packPos . to (Primitive . Circle 15)

data Bar = Bar { _barPos :: Vec2 }

makeLenses ''Bar

barShape :: Getter Bar Fig
barShape = barPos . to fig where
    fig origin = fmap (&+origin) $ Figures $ map Primitive
        [ Circle 12 $ Vec2 (-88) 0
        , Circle 12 $ Vec2 88 0
        , LineSegment (Vec2 176 0) (Vec2 (-88) 12)
        , LineSegment (Vec2 (-176) 0) (Vec2 88 (-12))
        ]

data World = World
    { _packs :: [Pack]
    , _blocks :: [Block]
    , _bar :: Bar
    }

defaultWorld :: World
defaultWorld = World
    { _packs = []
    , _blocks = []
    , _bar = error "The bar is uninitialized"
    }

makeLenses ''World

wallShape :: Fig
wallShape = Figures $ map Primitive
    [ LineSegment (Vec2 640 0) (Vec2 0 0)
    , LineSegment (Vec2 0 480) (Vec2 0 0)
    , LineSegment (Vec2 0 (-480)) (Vec2 640 480)
    ]

bottomShape :: Fig
bottomShape = Primitive $ LineSegment (Vec2 (-640) 0) (Vec2 640 480)

type TheGame = StateT World Game

addPack :: Pack -> TheGame ()
addPack p = packs %= (p:)

addBlock :: Block -> TheGame ()
addBlock b = blocks %= (b:)
