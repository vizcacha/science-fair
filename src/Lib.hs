{-# LANGUAGE Arrows #-}

module Lib
    ( bifurcate
    ) where

import HSoM
import FRP.UISF
import Euterpea

grow :: Double -> Double -> Double
grow r x = r * x * (1 - x)

popToNote :: Double -> [MidiMessage]
popToNote x = [ANote 0 n 64 0.05]
    where n = truncate (x * 127)

bifurcateUI :: UISF () ()
bifurcateUI = proc _ -> do
    mo <- selectOutput -< ()
    f <- title "Frequency" $ withDisplay (hSlider (1,10) 1) -< ()
    tick <- timer -< 1 / f
    r <- title "Growth rate" $ withDisplay (hSlider (2.4, 4.0) 2.4) -< ()
    pop <- accum 0.1 -< fmap (const (grow r)) tick
    _ <- title "Population" $ display -< pop
    midiOut -< (mo, fmap (const (popToNote pop)) tick)

bifurcate = runMUI (defaultMUIParams {uiTitle = "Bifurcate", uiSize = (300,500)}) bifurcateUI


someFunc :: IO ()
someFunc = putStrLn "someFunc"
