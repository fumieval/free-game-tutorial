{-# LANGUAGE ImplicitParams, TemplateHaskell, MonadComprehensions, FlexibleContexts #-}
import Graphics.FreeGame
import Control.Monad.Free
import Control.Monad.State
import Control.Applicative
import Data.Vect hiding (_1, _2)
import Data.Either
import Control.Lens

import Collision (collisions)
import Types

loadBitmaps "images"

updatePacks :: TheGame ()
updatePacks = use packs >>= \ps -> (packs<~) $ forM ps $ execStateT $ do
    vel <- use packVel
    origin <- use packPos
    drawPicture $ Translate origin (Bitmap _pack_png)

    col <- collisions <$> use packShape
    packPos .= origin &+ vel
    unless (null $ col bottomShape) (void quitGame)
    cols <- concat <$> sequence
        [ liftM col . lift . use $ bar.barShape
        , liftM msum . liftM (map col) . lift . use $ blocks . each . blockShape . to pure 
        , pure (col wallShape)]
    case cols of
        [] -> return ()
        (t:_) -> do
            let u = normalize (t &- origin)
                n = (u &. vel) *& neg u
            when (abs (angle (t &- origin) vel) < pi / 2)
                $ packVel %= (&+ 2 *& n)
    packPos <~ (&+origin) <$> use packVel

updateBlocks :: TheGame ()
updateBlocks = zoom (blocks . each)
    $ drawPicture =<< (`Translate`Bitmap _block_png) <$> use blockPos

updateBar :: TheGame ()
updateBar = zoom bar $ do
    let barSpeed = 4
    origin <- use barPos
    (,) <$> getButtonState KeyLeft <*> getButtonState KeyRight >>= \r -> case r of
        (True, False) -> barPos %= (&+Vec2 (-barSpeed) 0)
        (False, True) -> barPos %= (&+Vec2 barSpeed 0)
        _ -> return ()
    collisions wallShape <$> (use barShape) >>= \r -> case r of
        Just _ -> barPos .= origin
        Nothing -> return ()

    drawPicture =<< (`Translate`Bitmap _bar_png) <$> use barPos

gameMain :: TheGame ()
gameMain = do
    addPack $ Pack (Vec2 320 360) (Vec2 4 (-3))
    forM_ [60,140,220] $ \y -> do
        addBlock $ Block (Vec2 60 y)
        addBlock $ Block (Vec2 160 y)
        addBlock $ Block (Vec2 480 y)
        addBlock $ Block (Vec2 580 y)
    forever $ do
        updateBar
        updatePacks
        updateBlocks
        tick

main :: IO (Maybe ())
main = runGame defaultGameParam $ evalStateT gameMain
    $ defaultWorld & bar .~ Bar (Vec2 320 400)
