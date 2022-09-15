--------------------------------------------
import XMonad

import XMonad.Util.Ungrab
import XMonad.Util.EZConfig
import XMonad.Layout.ThreeColumns
import XMonad.Layout.Magnifier
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP
--------------------------------------------
myLayout = tiled ||| Mirror tiled ||| Full
  where
    tiled = Tall nmaster delta ratio
    nmaster = 1 -- Default number of windows in the master
    ratio = 1/2 -- Default proportion of screen occupied
    delta = 3/100 -- Percent of screen to increment by resize
--------------------------------------------

main :: IO ()
main = xmonad $ ewmhFullscreen $ ewmh $ xmobarProp $ myConfig
  { modMask = mod1Mask -- Alt Key 
  , layoutHook = myLayout
  }

myConfig = def 
  { modMask = mod1Mask
  , layoutHook = myLayout 
  }
