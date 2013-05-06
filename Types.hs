{-# LANGUAGE TemplateHaskell #-}
module Types where
import Graphics.UI.FreeGame
import Control.Monad.State
import Control.Applicative
import Control.Lens
import Collision

type Fig = Figure Basic (V2 Float)

data Block = Block
    { _blockPos :: V2 Float
    }
makeLenses ''Block

blockShape :: Getter Block Fig
blockShape = blockPos . to fig where
    fig p@(V2 px py) = Primitive $ Custom $ \f -> case f of
        s@(Circle r q@(V2 x y))
            | x0 < x && x < x1 -> guard' (abs (y0 - 4 - y) < r) (V2 x y0) <|> guard' (abs (y - y1 - 4) < r) (V2 x y1)
            | y0 < y && y < y1 -> guard' (abs (x0 - 4 - x) < r) (V2 x0 y) <|> guard' (abs (x - x1 - 4) < r) (V2 x1 y)
            | otherwise -> collisions (Primitive s) $ Figures $ map Primitive
                [ Circle 8 (V2 x0 y0)
                , Circle 8 (V2 x1 y0)
                , Circle 8 (V2 x0 y1)
                , Circle 8 (V2 x1 y1)]
        _ -> error "unsupported operation"
        where
            x0 = px - 32
            x1 = px + 32
            y0 = py - 8
            y1 = py + 8

data Pack = Pack
    { _packPos :: V2 Float
    , _packVel :: V2 Float
    }

makeLenses ''Pack

packShape :: Getter Pack Fig
packShape = packPos . to (Primitive . Circle 15)

data Bar = Bar { _barPos :: V2 Float }

makeLenses ''Bar

barShape :: Getter Bar Fig
barShape = barPos . to fig where
    fig origin = fmap (^+^origin) $ Figures $ map Primitive
        [ Circle 12 $ V2 (-88) 0
        , Circle 12 $ V2 88 0
        , LineSegment (V2 176 0) (V2 (-88) 12)
        , LineSegment (V2 (-176) 0) (V2 88 (-12))
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
    [ LineSegment (V2 640 0) (V2 0 0)
    , LineSegment (V2 0 480) (V2 0 0)
    , LineSegment (V2 0 (-480)) (V2 640 480)
    ]

bottomShape :: Fig
bottomShape = Primitive $ LineSegment (V2 (-640) 0) (V2 640 480)

type TheGame = StateT World Game

addPack :: Pack -> TheGame ()
addPack p = packs %= (p:)

addBlock :: Block -> TheGame ()
addBlock b = blocks %= (b:)
