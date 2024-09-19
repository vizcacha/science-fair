{-# LANGUAGE Arrows #-}

module Lib
    ( bifurcate
    ) where

import HSoM
import FRP.UISF
import FRP.UISF.UISF
import FRP.UISF.UITypes
import FRP.UISF.Widget.Construction
import FRP.UISF.Graphics.Graphic
import Euterpea

keyPressWidget :: UISF () (SEvent [MidiMessage])
keyPressWidget = withDisplay $ mkWidget [ANote 0 69 64 0.05] layout process draw
  where  
    layout = (Layout 0 0 0 0 0 0 1)
    process a s rect evt = case evt of
      SKey KeyEnter _ True -> (Just [ANote 0 74 64 0.05], [ANote 0 74 64 0.05], True)
      SKey KeyShiftR _ True -> (Just [ANote 0 69 64 0.05], [ANote 0 69 64 0.05], True)
      _ -> (Nothing, [], False)
    draw _ _ _ = nullGraphic

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
    midi <- keyPressWidget -< ()
    midi2 <- hold [] -< midi
    midiOut -< (mo, fmap (const midi2) tick)

bifurcate = runMUI (defaultMUIParams {uiTitle = "Bifurcate", uiSize = (300,500)}) bifurcateUI
