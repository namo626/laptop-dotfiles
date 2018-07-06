{-# LANGUAGE FlexibleContexts #-}

import XMonad
import qualified Data.Map as M
import qualified XMonad.StackSet as W
import XMonad.Layout.Renamed
import XMonad.Actions.Navigation2D
import XMonad.Layout.PerWorkspace
import XMonad.Layout.LayoutModifier
import XMonad.Layout.TwoPane
import XMonad.Layout.Spacing
import XMonad.Layout.Grid
import XMonad.Layout.ToggleLayouts
import XMonad.Layout.ComboP
import XMonad.Layout.ThreeColumns
import XMonad.Util.Run
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.InsertPosition
import XMonad.Hooks.ManageDocks
import System.IO
import XMonad.Layout.ResizableTile
import XMonad.Layout.SubLayouts
import XMonad.Layout.Tabbed
import XMonad.Layout.WindowNavigation
import XMonad.Layout.Simplest
import XMonad.Actions.CycleWS
import XMonad.Actions.GridSelect
import XMonad.Actions.WindowBringer
import XMonad.Prompt.Window
import XMonad.Hooks.EwmhDesktops (ewmh)
import XMonad.Layout.Fullscreen
import XMonad.Util.NamedScratchpad


main = do
  xmproc <- spawnPipe "~/.local/bin/xmobar ~/xmobar.config"
  xmonad =<< statusBar "xmobar ~/xmobar.config" xmobarPP toggleStrutsKey (withNavigation2DConfig def {defaultTiledNavigation = hybridNavigation} $ ewmh myConfig)

toggleStrutsKey XConfig {XMonad.modMask = modMask} = (modMask, xK_b)

myPP = namedScratchpadFilterOutWorkspacePP
     $ xmobarPP { ppOrder = \(ws:l:t:_) -> [ws, t]
                , ppCurrent = xmobarColor "yellow" "green" . wrap "" ""
, ppTitle = xmobarColor "green" "" . shorten 30}

myConfig = def
  { terminal = "terminator"
  , modMask = mod4Mask
  , keys = myKeys <+> keys def
  , layoutHook = avoidStruts $ toggleLayouts Full $ myLayouts
  -- , logHook = dynamicLogWithPP $ xmobarPP { ppOutput = hPutStrLn p }
  , handleEventHook = handleEventHook def <+> docksEventHook <+> fullscreenEventHook
  , borderWidth = 4
  , focusedBorderColor = solBlue
  , normalBorderColor = darkBlue
  , manageHook = fullscreenManageHook <+> manageDocks <+> myManageHook <+> manageHook def
  , XMonad.workspaces = myWorkspaces
  }

myWorkspaces = [ "conf", "term", "docs", "matlab", "media", "prog1", "prog2", "misc", "web" ]
-- layouts
-- SUBLAYOUT IS THE CAUSE OF FLOAT FOCUS LOST!!!
myLayouts =
  onWorkspace "conf" confLayout
  $ onWorkspace "term" terminalLayout
  $ onWorkspace "docs" readingLayout
  $ onWorkspace "matlab" matlabLayout
  $ onWorkspace "media" mediaLayout
  $ onWorkspace "prog1" codingLayout
  $ onWorkspace "prog2" codingLayout
  $ onWorkspace "web" readingLayout
  $ readingLayout

mySpacing = spacingWithEdge 7

confLayout =
  renamed [Replace "Conf"]
  $ addSub
  $ simpleTall 53 ||| simpleThree 46

terminalLayout =
  renamed [Replace "Terminals"] $
  mySpacing $
  simpleTall 50 |||
  simpleThree 40 |||
  (Mirror $ simpleTall 60)

codingLayout =
  renamed [Replace "Coding"] $
  twoPaneTabbed |||
  twoPaneTall |||
  simpleTall 50

matlabLayout =
  renamed [Replace "MATLAB"]
  $ addSub
  $ simpleTall 50 ||| simpleThree 40

mediaLayout =
  renamed [Replace "Media"] $
  mySpacing $
  Grid ||| simpleThree 50 ||| (Mirror $ simpleThree 50)

readingLayout =
  renamed [Replace "Reading"] $
  mySpacing $
  simpleTwo 50 ||| simpleThree 50

-- twoPaneFlex =
--   windowNavigation
--   $ subLayout [] Full
--   $ ResizableTall 2 (3/100) (53/100) []

{- Base modifiers -}
--addSub :: (LayoutClass l a) => l a -> ModifiedLayout m l a
addSub l =
  configurableNavigation noNavigateBorders
  $ addTabs shrinkText def
  $ mySpacing
  $ subLayout [] Simplest
  $ l

{- Base layouts to be combined -}

twoPaneTabbed =
  windowNavigation $
  combineTwoP (mySpacing $ TwoPane 0.03 0.50)
	      (Full)
 	      (tabbed shrinkText def)
	      (ClassName "Firefox-esr" `Or` ClassName "qpdfview")

twoPaneTall =
  windowNavigation $
  combineTwoP (TwoPane 0.03 0.50) (Full) (Mirror $ simpleThree 60) (ClassName "Firefox-esr" `Or` ClassName "qpdfview")

--simpleTall :: Rational -> ResizableTall a
simpleTall n = ResizableTall 1 (3/100) (n/100) []

simpleThree :: Rational -> ThreeCol a
simpleThree n = ThreeCol 1 (3/100) (n/100)

simpleTwo :: Rational -> TwoPane a
simpleTwo n = TwoPane (3/100) (n/100)



-- named colors
solBlue = "#268bd2"
darkBlue = "#073642"

--manage hooks
myManageHook = composeOne
               [ name =? "Terminator Preferences" -?> insertPosition Above Newer <+> doCenterFloat
               , isDialog -?> insertPosition Above Newer <+> doCenterFloat
               , className =? "Eog" -?> insertPosition Below Older
               , className =? "MATLAB R2017b - academic use" -?> insertPosition Below Older
               -- , className =? "Thunar" -?> doCenterFloat
               , className =? "matplotlib" -?> insertPosition Below Older
               , return True -?> insertPosition Below Newer]

  where name = stringProperty "WM_NAME"

altMask = mod1Mask
myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList
        -- basic navigation
	[ ((altMask, xK_l), windowGo R False)
 	, ((altMask, xK_j), windowGo D False)
 	, ((altMask, xK_k), windowGo U False)
 	, ((altMask, xK_h), windowGo L False)
        , ((modm, xK_u), goToSelected def)
        --, ((modm, xK_o), windowPrompt def Goto wsWindows)

        -- launching apps
        , ((modm, xK_e), spawn "emacsclient -c")
        , ((modm .|. altMask, xK_l), spawn "slock")

        -- resizing tall
        , ((modm, xK_a), sendMessage MirrorShrink)
        , ((modm, xK_z), sendMessage MirrorExpand)

        -- swapping windows
        , ((modm .|. shiftMask, xK_h), windowSwap L False)
        , ((modm .|. shiftMask, xK_l), windowSwap R False)
        , ((modm .|. shiftMask, xK_k), windows W.swapUp)
        , ((modm .|. shiftMask, xK_j), windows W.swapDown)

        -- combo layout
        , ((modm .|. shiftMask, xK_Right), sendMessage $ Move R)
        , ((modm .|. shiftMask, xK_Left), sendMessage $ Move L)
        , ((modm .|. shiftMask, xK_Up), sendMessage $ Move U)
        , ((modm .|. shiftMask, xK_Down), sendMessage $ Move D)
        , ((modm .|. shiftMask, xK_s), sendMessage $ SwapWindow)

        -- pulling windows into sublayouts
        , ((modm .|. controlMask, xK_h), sendMessage $ pullGroup L)
        , ((modm .|. controlMask, xK_l), sendMessage $ pullGroup R)
        , ((modm .|. controlMask, xK_k), sendMessage $ pullGroup U)
        , ((modm .|. controlMask, xK_j), sendMessage $ pullGroup D)
        , ((modm .|. controlMask, xK_m), withFocused (sendMessage . MergeAll))
        , ((modm .|. controlMask, xK_u), withFocused (sendMessage . UnMerge))

        --easy switching of workspaces
        , ((modm, xK_Left), prevWS)
        , ((modm, xK_Right), nextWS)

        --, ((altMask, xK_Tab), cycleRecentWS [xK_Alt_L] xK_Tab xK_grave)
        , ((modm, xK_Tab), toggleWS' ["NSP"])

        , ((modm, xK_grave), sendMessage $ ToggleLayout)
        ]
