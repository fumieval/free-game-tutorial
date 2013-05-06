{-# LANGUAGE ImplicitParams, TemplateHaskell, MonadComprehensions, FlexibleContexts #-}
import Graphics.UI.FreeGame
import Control.Monad.State
import Control.Applicative
import Control.Lens

import Collision (collisions)
import Types

loadBitmaps "images"

primes :: [Integer]
primes = 2 : filter isPrime [3,5..]

isPrime ::　Integer -> Bool
isPrime n = foldr (\m r -> m * m > n || n `mod` m /= 0 && r) undefined primes

updatePacks :: TheGame ()
updatePacks = use packs >>= \ps -> (packs<~) $ forM ps $ execStateT $ do
    vel <- use packVel
    origin <- use packPos
    translate origin (fromBitmap _pack_png)

    col <- collisions <$> use packShape
    packPos .= origin ^+^ vel
    unless (null $ col bottomShape) quit
    cols <- concat <$> sequence
        [ liftM col . lift . use $ bar.barShape
        , liftM msum . liftM (map col) . lift . use $ blocks . each . blockShape . to pure 
        , pure (col wallShape)]
    case cols of
        [] -> return ()
        (t:_) -> do
            let u = normalize (t ^-^ origin)
                n = (u `dot` vel) *^ negate u
            when (abs (angle (t ^-^ origin) vel) < pi / 2) $ packVel += 2 *^ n
    packPos <~ (^+^origin) <$> use packVel

angle a b = acos $ dot a b / norm a / norm b

updateBlocks :: TheGame ()
updateBlocks = zoom (blocks . each) $ use blockPos >>= (`translate` fromBitmap _block_png)

updateBar :: TheGame ()
updateBar = zoom bar $ do
    let barSpeed = 4
    origin <- use barPos
    (,) <$> keySpecial KeyLeft <*> keySpecial KeyRight >>= \r -> case r of
        (True, False) -> barPos += V2 (-barSpeed) 0
        (False, True) -> barPos += V2 barSpeed 0
        _ -> return ()
    collisions wallShape <$> use barShape >>= \r -> case r of
        Just _ -> barPos .= origin
        Nothing -> return ()

    use barPos >>= (`translate` fromBitmap _bar_png)

gameMain :: TheGame ()
gameMain = do
    addPack $ Pack (V2 320 360) (V2 4 (-3))
    forM_ [60,140,220] $ \y -> do
        addBlock $ Block (V2 60 y)
        addBlock $ Block (V2 160 y)
        addBlock $ Block (V2 480 y)
        addBlock $ Block (V2 580 y)
    forever $ do
        updateBar
        updatePacks
        updateBlocks
        tick

main :: IO (Maybe ())
main = runGame def $ evalStateT gameMain
    $ defaultWorld & bar .~ Bar (V2 320 400)
