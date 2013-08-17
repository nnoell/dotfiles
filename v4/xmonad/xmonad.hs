--------------------------------------------------------------------------------------------
-- File   : ~/.xmonad/xmonad.hs                                                           --
-- Author : Nnoell <nnoell3[at]gmail.com>                                                 --
-- Deps   : DzenBoxLogger.hs                                                              --
-- Desc   : My XMonad config                                                              --
-- Note   : Do not use "xmonad --recompile", it will throw errors because of non-official --
--          modules. Compile it manually with "ghc -o <outputName> xmonad.hs". EG:        --
--          $ cd ~/.xmonad/                                                               --
--          $ ghc -o xmonad-x86_64-linux xmonad.hs                                        --
-- Note2  : I use a little hack (thanks to DarthFennec) to send an X event after a        --
--          specified time period (one second) in order to get always updated my status   --
--          bars. This may crash xmonad if you try to reload the configuration on the fly --
--          with "xmonad --restart" or mod+q. I suggest to stop xmonad and start it again --
--          when changing xmonad.hs. You can disable this by removing clockEventHook and  --
--          handleTimerEvent from "myHandleEventHook" and (startTimer 1 >>= XS.put . TID) --
--			from "startupHook".                                                           --
--------------------------------------------------------------------------------------------

-- Language
{-# LANGUAGE DeriveDataTypeable, NoMonomorphismRestriction, MultiParamTypeClasses, ImplicitParams #-}

-- Modules
import XMonad
import XMonad.Layout
import XMonad.Layout.IM
import XMonad.Layout.Named
import XMonad.Layout.Tabbed
import XMonad.Layout.OneBig
import XMonad.Layout.Master
import XMonad.Layout.Reflect
import XMonad.Layout.MosaicAlt
import XMonad.Layout.NoFrillsDecoration
import XMonad.Layout.SimplestFloat
import XMonad.Layout.NoBorders
import XMonad.Layout.ResizableTile
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.PerWorkspace (onWorkspace)
import XMonad.Layout.Minimize
import XMonad.Layout.Maximize
import XMonad.Layout.ToggleLayouts
import XMonad.Layout.ComboP
import XMonad.Layout.MagicFocus
import XMonad.Layout.WindowNavigation
import XMonad.Layout.WindowSwitcherDecoration
import XMonad.Layout.DraggingVisualizer
import XMonad.StackSet (RationalRect(..), currentTag)
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.DynamicHooks
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.SetWMName
import XMonad.Hooks.ManageHelpers
import XMonad.Prompt
import XMonad.Prompt.Shell
import XMonad.Prompt.XMonad
import XMonad.Prompt.Man
import XMonad.Util.Timer
import XMonad.Util.Cursor
import XMonad.Util.Loggers
import XMonad.Util.Run (spawnPipe)
import XMonad.Util.Scratchpad
import XMonad.Util.NamedScratchpad
import XMonad.Actions.CycleWS
import XMonad.Actions.ShowText
import XMonad.Actions.GridSelect
import XMonad.Actions.MouseResize
import Data.Monoid
import Data.List
import Graphics.X11.ExtraTypes.XF86
import System.Exit
import System.IO (Handle, hPutStrLn)
import Control.Exception as E
import qualified XMonad.StackSet as W
import qualified Data.Map as M
import qualified XMonad.Actions.FlexibleResize as Flex
import qualified XMonad.Util.ExtensibleState as XS

-- non-official modules
import DzenBoxLoggers


-- Main
main :: IO ()
main = do
	topLeftBar  <- spawnPipe $ dzenFlagsToStr dzenTopLeftFlags
	topRightBar <- spawnPipe $ dzenFlagsToStr dzenTopRightFlags
	botLeftBar  <- spawnPipe $ dzenFlagsToStr dzenBotLeftFlags
	botRightBar <- spawnPipe $ dzenFlagsToStr dzenBotRightFlags
	xmonad $ myUrgencyHook $ defaultConfig
		{ terminal           = "urxvtc"
		, modMask            = mod4Mask
		, focusFollowsMouse  = True
		, clickJustFocuses   = True
		, borderWidth        = 1
		, normalBorderColor  = colorBlackAlt
		, focusedBorderColor = colorWhiteAlt2
		, layoutHook         = myLayoutHook
		, workspaces         = myWorkspaces
		, manageHook         = myManageHook <+> manageScratchPad <+> manageDocks <+> dynamicMasterHook
		, logHook            = myBotLeftLogHook botLeftBar <+> myBotRightLogHook botRightBar <+> myTopLeftLogHook topLeftBar <+> myTopRightLogHook topRightBar <+> ewmhDesktopsLogHook >> setWMName "LG3D"
		, handleEventHook    = myHandleEventHook
		, keys               = myKeys
		, mouseBindings      = myMouseBindings
		, startupHook        = spawn "/home/nnoell/.xmonad/apps/haskell-cpu-usage.out 5" <+> setDefaultCursor xC_left_ptr <+> (startTimer 1 >>= XS.put . TID)
		}


--------------------------------------------------------------------------------------------
-- LOOK AND FEEL CONFIG                                                                   --
--------------------------------------------------------------------------------------------

-- Colors, fonts and paths
dzenFont       = "-*-montecarlo-medium-r-normal-*-11-*-*-*-*-*-*-*"
colorBlack     = "#020202" --Background
colorBlackAlt  = "#1c1c1c" --Black Xdefaults
colorGray      = "#444444" --Gray
colorGrayAlt   = "#101010" --Gray dark
colorGrayAlt2  = "#404040"
colorGrayAlt3  = "#252525"
colorWhite     = "#a9a6af" --Foreground
colorWhiteAlt  = "#9d9d9d" --White dark
colorWhiteAlt2 = "#b5b3b3"
colorWhiteAlt3 = "#707070"
colorMagenta   = "#8e82a2"
colorBlue      = "#44aacc"
colorBlueAlt   = "#3955c4"
colorRed       = "#f7a16e"
colorRedAlt    = "#e0105f"
colorGreen     = "#66ff66"
colorGreenAlt  = "#558965"
boxLeftIcon    = "/home/nnoell/.icons/xbm_icons/subtle/boxleft.xbm"   --left icon of dzen logger boxes
boxLeftIcon2   = "/home/nnoell/.icons/xbm_icons/subtle/boxleft2.xbm"  --left icon2 of dzen logger boxes
boxRightIcon   = "/home/nnoell/.icons/xbm_icons/subtle/boxright.xbm"  --right icon of dzen logger boxes
xRes           = 1366
yRes           = 768
panelHeight    = 14  --height of top and bottom panels
boxHeight      = 14  --height of dzen logger box
topPanelSepPos = 950 --left-right alignment pos of top panel
botPanelSepPos = 400 --left-right alignment pos of bottom panel

-- Title theme
myTitleTheme :: Theme
myTitleTheme = defaultTheme
	{ fontName            = dzenFont
	, inactiveBorderColor = colorGrayAlt2
	, inactiveColor       = colorGrayAlt3
	, inactiveTextColor   = colorWhiteAlt3
	, activeBorderColor   = colorGrayAlt2
	, activeColor         = colorGrayAlt2
	, activeTextColor     = colorWhiteAlt2
	, urgentBorderColor   = colorGray
	, urgentTextColor     = colorGreen
	, decoHeight          = 14
	}

-- Prompt theme
myXPConfig :: XPConfig
myXPConfig = defaultXPConfig
	{ font              = dzenFont
	, bgColor           = colorBlack
	, fgColor           = colorWhite
	, bgHLight          = colorBlue
	, fgHLight          = colorBlack
	, borderColor       = colorGrayAlt
	, promptBorderWidth = 1
	, height            = panelHeight
	, position          = Top
	, historySize       = 100
	, historyFilter     = deleteConsecutive
	, autoComplete      = Nothing
	}

-- GridSelect color scheme
myColorizer :: Window -> Bool -> X (String, String)
myColorizer = colorRangeFromClassName
	(0x00,0x00,0x00) --lowest inactive bg
	(0x1C,0x1C,0x1C) --highest inactive bg
	(0x44,0xAA,0xCC) --active bg
	(0xBB,0xBB,0xBB) --inactive fg
	(0x00,0x00,0x00) --active fg

-- GridSelect theme
myGSConfig :: t -> GSConfig Window
myGSConfig colorizer = (buildDefaultGSConfig myColorizer)
	{ gs_cellheight  = 50
	, gs_cellwidth   = 200
	, gs_cellpadding = 10
	, gs_font        = dzenFont
	}

-- Flash text config
myTextConfig :: ShowTextConfig
myTextConfig = STC
	{ st_font = dzenFont
	, st_bg   = colorBlack
	, st_fg   = colorWhite
	}

-- Dzen logger box pretty printing themes
gray2BoxPP :: BoxPP
gray2BoxPP = BoxPP
	{ bgColorBPP   = colorBlack
	, fgColorBPP   = colorGray
	, boxColorBPP  = colorGrayAlt
	, leftIconBPP  = boxLeftIcon2
	, rightIconBPP = boxRightIcon
	, boxHeightBPP = boxHeight
	}

blueBoxPP :: BoxPP
blueBoxPP = BoxPP
	{ bgColorBPP   = colorBlack
	, fgColorBPP   = colorBlue
	, boxColorBPP  = colorGrayAlt
	, leftIconBPP  = boxLeftIcon
	, rightIconBPP = boxRightIcon
	, boxHeightBPP = boxHeight
	}

blue2BoxPP :: BoxPP
blue2BoxPP = BoxPP
	{ bgColorBPP   = colorBlack
	, fgColorBPP   = colorBlue
	, boxColorBPP  = colorGrayAlt
	, leftIconBPP  = boxLeftIcon2
	, rightIconBPP = boxRightIcon
	, boxHeightBPP = boxHeight
	}

whiteBoxPP :: BoxPP
whiteBoxPP = BoxPP
	{ bgColorBPP   = colorBlack
	, fgColorBPP   = colorWhiteAlt
	, boxColorBPP  = colorGrayAlt
	, leftIconBPP  = boxLeftIcon
	, rightIconBPP = boxRightIcon
	, boxHeightBPP = boxHeight
	}

blackBoxPP :: BoxPP
blackBoxPP = BoxPP
	{ bgColorBPP   = colorBlack
	, fgColorBPP   = colorBlack
	, boxColorBPP  = colorGrayAlt
	, leftIconBPP  = boxLeftIcon
	, rightIconBPP = boxRightIcon
	, boxHeightBPP = boxHeight
	}

white2BBoxPP :: BoxPP
white2BBoxPP = BoxPP
	{ bgColorBPP   = colorBlack
	, fgColorBPP   = colorBlack
	, boxColorBPP  = colorWhiteAlt
	, leftIconBPP  = boxLeftIcon2
	, rightIconBPP = boxRightIcon
	, boxHeightBPP = boxHeight
	}

blue2BBoxPP :: BoxPP --current workspace
blue2BBoxPP = BoxPP
	{ bgColorBPP   = colorBlack
	, fgColorBPP   = colorBlack
	, boxColorBPP  = colorBlue
	, leftIconBPP  = boxLeftIcon2
	, rightIconBPP = boxRightIcon
	, boxHeightBPP = boxHeight
	}

green2BBoxPP :: BoxPP --urgent workspace
green2BBoxPP = BoxPP
	{ bgColorBPP   = colorBlack
	, fgColorBPP   = colorBlack
	, boxColorBPP  = colorGreen
	, leftIconBPP  = boxLeftIcon2
	, rightIconBPP = boxRightIcon
	, boxHeightBPP = boxHeight
	}

-- Dzen logger clickable areas
calendarCA :: CA
calendarCA = CA
	{ leftClickCA   = "/home/nnoell/bin/dzencal.sh"
	, middleClickCA = "/home/nnoell/bin/dzencal.sh"
	, rightClickCA  = "/home/nnoell/bin/dzencal.sh"
	, wheelUpCA     = "/home/nnoell/bin/dzencal.sh"
	, wheelDownCA   = "/home/nnoell/bin/dzencal.sh"
	}

layoutCA :: CA
layoutCA = CA
	{ leftClickCA   = "/usr/bin/xdotool key super+space"
	, middleClickCA = "/usr/bin/xdotool key super+v"
	, rightClickCA  = "/usr/bin/xdotool key super+shift+space"
	, wheelUpCA     = "/usr/bin/xdotool key super+f"
	, wheelDownCA   = "/usr/bin/xdotool key super+control+f"
	}

workspaceCA :: CA
workspaceCA = CA
	{ leftClickCA   = "/usr/bin/xdotool key super+1"
	, middleClickCA = "/usr/bin/xdotool key super+g"
	, rightClickCA  = "/usr/bin/xdotool key super+0"
	, wheelUpCA     = "/usr/bin/xdotool key ctrl+alt+Right"
	, wheelDownCA   = "/usr/bin/xdotool key ctrl+alt+Left"
	}

focusCA :: CA
focusCA = CA
	{ leftClickCA   = "/usr/bin/xdotool key super+m"
	, middleClickCA = "/usr/bin/xdotool key super+c"
	, rightClickCA  = "/usr/bin/xdotool key super+shift+m"
	, wheelUpCA     = "/usr/bin/xdotool key super+shift+j"
	, wheelDownCA   = "/usr/bin/xdotool key super+shift+k"
	}

-- Workspace index
myWorkspaces :: [WorkspaceId]
myWorkspaces = map show $ [1..9] ++ [0]

-- Workspace names
workspaceNames :: [WorkspaceId]
workspaceNames = ["Terminal", "Network", "Development", "Graphics", "Chatting", "Video", "Alternate", "Alternate", "Alternate", "Alternate"]

-- Layout names (must be one word name and not equal to: Mirror, ReflectX, ReflectY, Switcher, Normal and Unique)
myTileName = "Tiled"
myMirrName = "Mirror"
myMosAName = "Mosaic"
myOneBName = "OneBig"
myMTabName = "MstrTab"
myChatName = "Chat"
myTabbName = "Tabbed"
myTTabName = "TwoTab"
myFTabName = "Full"
myFloaName = "Float"


--------------------------------------------------------------------------------------------
-- LAYOUT CONFIG                                                                          --
--------------------------------------------------------------------------------------------

-- Tabbed transformer (W+f)
data TABBED = TABBED deriving (Read, Show, Eq, Typeable)
instance Transformer TABBED Window where
	transform TABBED x k = k myFTab (\_ -> x)

-- Floated transformer (W+ctl+f)
data FLOATED = FLOATED deriving (Read, Show, Eq, Typeable)
instance Transformer FLOATED Window where
	transform FLOATED x k = k myFloa (\_ -> x)

-- Switcher Layouts
myTile = smartBorders $ toggleLayouts (named ("Switcher " ++ myTileName) myTileS) $ named ("Normal " ++ myTileName) $ ResizableTall 1 0.03 0.5 [] where
	myTileS = windowSwitcherDecoration shrinkText myTitleTheme (draggingVisualizer $ ResizableTall 1 0.03 0.5 [])
myMirr = smartBorders $ toggleLayouts (named ("Switcher " ++ myMirrName) myMirrS) $ named ("Normal " ++ myMirrName) $ Mirror $ ResizableTall 1 0.03 0.5 [] where
	myMirrS = windowSwitcherDecoration shrinkText myTitleTheme (draggingVisualizer $ Mirror $ ResizableTall 1 0.03 0.5 [])
myMosA = smartBorders $ toggleLayouts (named ("Switcher " ++ myMosAName) myMosAS) $ named ("Normal " ++ myMosAName) $ MosaicAlt M.empty where
	myMosAS = windowSwitcherDecoration shrinkText myTitleTheme (draggingVisualizer $ MosaicAlt M.empty)
myOneB = smartBorders $ toggleLayouts (named ("Switcher " ++ myOneBName) myOneBS) $ named ("Normal " ++ myOneBName) $ OneBig 0.75 0.65 where
	myOneBS = windowSwitcherDecoration shrinkText myTitleTheme (draggingVisualizer $ OneBig 0.75 0.65)
myMTab = smartBorders $ toggleLayouts (named ("Switcher " ++ myMTabName) myMTabS) $ named ("Normal " ++ myMTabName) $ mastered 0.01 0.4 $ tabbed shrinkText myTitleTheme where
	myMTabS = windowSwitcherDecoration shrinkText myTitleTheme (draggingVisualizer $ mastered 0.01 0.4 $ tabbed shrinkText myTitleTheme)
myChat = smartBorders $ toggleLayouts (named ("Switcher " ++ myChatName) $ withIM (0.20) (Title "Buddy List") myChatS) (named ("Normal " ++ myChatName) $ withIM (0.20) (Title "Buddy List") $ MosaicAlt M.empty) where
	myChatS = windowSwitcherDecoration shrinkText myTitleTheme (draggingVisualizer $ MosaicAlt M.empty)

-- Unique Layouts
myTabb = smartBorders $ named ("Unique " ++ myTabbName) $ tabbed shrinkText myTitleTheme
myTTab = smartBorders $ named ("Unique " ++ myTTabName) $ combineTwoP (OneBig 0.75 0.75) (tabbed shrinkText myTitleTheme) (tabbed shrinkText myTitleTheme) (ClassName "Chromium")
myFTab = smartBorders $ named ("Unique " ++ myFTabName) $ tabbedAlways shrinkText myTitleTheme
myFloa = named ("Unique " ++ myFloaName) $ mouseResize $ noFrillsDeco shrinkText myTitleTheme simplestFloat

-- Layout hook
myLayoutHook = avoidStruts
	$ configurableNavigation noNavigateBorders
	$ minimize
	$ maximize
	$ mkToggle (single TABBED)
	$ mkToggle (single FLOATED)
	$ mkToggle (single MIRROR)
	$ mkToggle (single REFLECTX)
	$ mkToggle (single REFLECTY)
	$ onWorkspace (myWorkspaces !! 1) webLayouts
	$ onWorkspace (myWorkspaces !! 2) codeLayouts
	$ onWorkspace (myWorkspaces !! 4) chatLayouts
	$ allLayouts where
		webLayouts  = myTabb ||| myTTab
		codeLayouts = myMTab ||| myOneB ||| myTile
		chatLayouts = myChat
		allLayouts  = myTile ||| myOneB ||| myMirr ||| myMosA ||| myMTab


--------------------------------------------------------------------------------------------
-- HANDLE EVENT HOOK CONFIG                                                               --
--------------------------------------------------------------------------------------------

-- wrapper for the Timer id, so it can be stored as custom mutable state
data TidState = TID TimerId deriving Typeable

instance ExtensionClass TidState where
	initialValue = TID 0

-- Handle event hook
myHandleEventHook = fullscreenEventHook <+> docksEventHook <+> clockEventHook <+> handleTimerEvent <+> notFocusFloat where
	clockEventHook e = do                 --thanks to DarthFennec
		(TID t) <- XS.get                 --get the recent Timer id
		handleTimer t e $ do              --run the following if e matches the id
		    startTimer 1 >>= XS.put . TID --restart the timer, store the new id
		    ask >>= logHook . config      --get the loghook and run it
		    return Nothing                --return required type
		return $ All True                 --return required type
	notFocusFloat = followOnlyIf (fmap not isFloat) where --Do not focusFollowMouse on Float layout
		isFloat = fmap (isSuffixOf myFloaName) $ gets (description . W.layout . W.workspace . W.current . windowset)


--------------------------------------------------------------------------------------------
-- MANAGE HOOK CONFIG                                                                     --
--------------------------------------------------------------------------------------------

-- Scratchpad (W+º)
manageScratchPad :: ManageHook
manageScratchPad = scratchpadManageHook $ W.RationalRect (0) (panelHeight/yRes) (1) (3/4)
scratchPad = scratchpadSpawnActionCustom "urxvtc -name scratchpad"

-- Manage hook
myManageHook :: ManageHook
myManageHook = composeAll . concat $
	[ [ resource  =? r --> doIgnore                    | r <- myIgnores ]
	, [ className =? c --> doShift (myWorkspaces !! 1) | c <- myWebS    ]
	, [ className =? c --> doShift (myWorkspaces !! 2) | c <- myCodeS   ]
	, [ className =? c --> doShift (myWorkspaces !! 4) | c <- myChatS   ]
	, [ className =? c --> doShift (myWorkspaces !! 3) | c <- myGfxS    ]
	, [ className =? c --> doShift (myWorkspaces !! 7) | c <- myAlt3S   ]
	, [ className =? c --> doCenterFloat               | c <- myFloatCC ]
	, [ name      =? n --> doCenterFloat               | n <- myFloatCN ]
	, [ name      =? n --> doSideFloat NW              | n <- myFloatSN ]
	, [ className =? c --> doF W.focusDown             | c <- myFocusDC ]
	, [ isFullscreen   --> doFullFloat ]
	] where
		name            = stringProperty "WM_NAME"
		myIgnores       = ["desktop","desktop_window"]
		myWebS          = ["Chromium","Firefox", "Opera"]
		myCodeS         = ["NetBeans IDE 7.3"]
		myGfxS          = ["Gimp", "gimp", "GIMP"]
		myChatS         = ["Pidgin", "Xchat"]
		myAlt3S         = ["Amule", "Transmission-gtk"]
		myFloatCC       = ["MPlayer", "mplayer2", "File-roller", "zsnes", "Gcalctool", "Exo-helper-1", "Gksu", "PSX", "Galculator", "Nvidia-settings", "XFontSel"
						  , "XCalc", "XClock", "Ossxmix", "Xvidcap", "Main", "Wicd-client.py"]
		myFloatCN       = ["Choose a file", "Open Image", "File Operation Progress", "Firefox Preferences", "Preferences", "Search Engines", "Set up sync"
						  ,"Passwords and Exceptions", "Autofill Options", "Rename File", "Copying files", "Moving files", "File Properties", "Replace", ""]
		myFloatSN       = ["Event Tester"]
		myFocusDC       = ["Event Tester", "Notify-osd"]


--------------------------------------------------------------------------------------------
-- DZEN STATUS BARS CONFIG                                                                --
--------------------------------------------------------------------------------------------

-- urgencyHook
myUrgencyHook :: LayoutClass l Window => XConfig l -> XConfig l
myUrgencyHook = withUrgencyHook dzenUrgencyHook
	{ duration = 2000000
	, args     = ["-x", "0", "-y", "0", "-h", show panelHeight, "-w", show topPanelSepPos, "-fn", dzenFont, "-bg", colorBlack, "-fg", colorGreen]
	}

-- Dzen top left bar flags
dzenTopLeftFlags :: DF
dzenTopLeftFlags = DF
	{ xPosDF       = 0
	, yPosDF       = 0
	, widthDF      = topPanelSepPos
	, heightDF     = panelHeight
	, alignementDF = "l"
	, fgColorDF    = colorWhiteAlt
	, bgColorDF    = colorBlack
	, fontDF       = dzenFont
	, eventDF      = "onstart=lower"
	, extrasDF     = "-p"
	}

-- Top left bar logHook
myTopLeftLogHook :: Handle -> X ()
myTopLeftLogHook h = dynamicLogWithPP $ defaultPP
	{ ppOutput = hPutStrLn h
	, ppOrder = \(_:_:_:x) -> x
	, ppSep = " "
	, ppExtras = [ myLayoutL, myWorkspaceL, myFocusL ]
	}

-- Dzen top right bar flags
dzenTopRightFlags :: DF
dzenTopRightFlags = DF
	{ xPosDF       = topPanelSepPos
	, yPosDF       = 0
	, widthDF      = xRes - topPanelSepPos
	, heightDF     = panelHeight
	, alignementDF = "r"
	, fgColorDF    = colorWhiteAlt
	, bgColorDF    = colorBlack
	, fontDF       = dzenFont
	, eventDF      = "onstart=lower"
	, extrasDF     = "-p"
	}

-- Top right bar logHook
myTopRightLogHook :: Handle -> X ()
myTopRightLogHook h = dynamicLogWithPP $ defaultPP
	{ ppOutput  = hPutStrLn h
	, ppOrder = \(_:_:_:x) -> x
	, ppSep = " "
	, ppExtras  = [ myUptimeL, myDateL ]
	}

-- Dzen bottom left bar flags
dzenBotLeftFlags :: DF
dzenBotLeftFlags = DF
	{ xPosDF       = 0
	, yPosDF       = yRes - panelHeight
	, widthDF      = botPanelSepPos
	, heightDF     = panelHeight
	, alignementDF = "l"
	, fgColorDF    = colorWhiteAlt
	, bgColorDF    = colorBlack
	, fontDF       = dzenFont
	, eventDF      = "onstart=lower"
	, extrasDF     = "-p"
	}

-- Bottom left bar logHook
myBotLeftLogHook :: Handle -> X ()
myBotLeftLogHook h = dynamicLogWithPP . namedScratchpadFilterOutWorkspacePP $ defaultPP
	{ ppOutput          = hPutStrLn h
	, ppOrder           = \(ws:l:_:x) -> [ws] ++ x
	, ppSep             = " "
	, ppWsSep           = ""
	, ppCurrent         = dzenBoxStyle blue2BBoxPP
	, ppUrgent          = dzenBoxStyle green2BBoxPP . dzenClickWorkspace
	, ppVisible         = dzenBoxStyle blackBoxPP . dzenClickWorkspace
	, ppHiddenNoWindows = dzenBoxStyle blackBoxPP . dzenClickWorkspace
	, ppHidden          = dzenBoxStyle whiteBoxPP . dzenClickWorkspace
	, ppExtras          = [ myFsL ]
	} where
		dzenClickWorkspace ws = "^ca(1," ++ xdo "w;" ++ xdo index ++ ")" ++ "^ca(3," ++ xdo "w;" ++ xdo index ++ ")" ++ ws ++ "^ca()^ca()" where
			wsIdxToString Nothing = "1"
			wsIdxToString (Just n) = show $ mod (n+1) $ length myWorkspaces
			index = wsIdxToString (elemIndex ws myWorkspaces)
			xdo key = "/usr/bin/xdotool key super+" ++ key

-- Dzen bottom right bar flags
dzenBotRightFlags :: DF
dzenBotRightFlags = DF
	{ xPosDF       = botPanelSepPos
	, yPosDF       = yRes - panelHeight
	, widthDF      = xRes - botPanelSepPos
	, heightDF     = panelHeight
	, alignementDF = "r"
	, fgColorDF    = colorWhiteAlt
	, bgColorDF    = colorBlack
	, fontDF       = dzenFont
	, eventDF      = "onstart=lower"
	, extrasDF     = "-p"
	}

-- Bottom right bar logHook
myBotRightLogHook :: Handle -> X ()
myBotRightLogHook h = dynamicLogWithPP $ defaultPP
	{ ppOutput          = hPutStrLn h
	, ppOrder           = \(_:_:_:x) -> x
	, ppSep             = " "
	, ppExtras          = [ myCpuL, myMemL, myTempL, myBrightL, myWifiL, myBatL ]
	}


--------------------------------------------------------------------------------------------
-- LOGGERS CONFIG                                                                         --
--------------------------------------------------------------------------------------------

myBatL       = (dzenBoxStyleL gray2BoxPP $ labelL "BATTERY") ++! (dzenBoxStyleL blueBoxPP batPercent) ++! (dzenBoxStyleL whiteBoxPP batStatus)
myWifiL      = (dzenBoxStyleL gray2BoxPP $ labelL "WIFI") ++! (dzenBoxStyleL blueBoxPP wifiSignal)
myBrightL    = (dzenBoxStyleL gray2BoxPP $ labelL "BRIGHT") ++! (dzenBoxStyleL blueBoxPP brightPerc)
myTempL      = (dzenBoxStyleL gray2BoxPP $ labelL "TEMP") ++! (dzenBoxStyleL blueBoxPP cpuTemp)
myMemL       = (dzenBoxStyleL gray2BoxPP $ labelL "MEM") ++! (dzenBoxStyleL blueBoxPP memUsage)
myCpuL       = (dzenBoxStyleL gray2BoxPP $ labelL "CPU") ++! (dzenBoxStyleL blueBoxPP $ cpuUsage "/tmp/haskell-cpu-usage.txt")
myFsL        = (dzenBoxStyleL blue2BoxPP $ labelL "ROOT") ++! (dzenBoxStyleL whiteBoxPP $ fsPerc "/") ++! (dzenBoxStyleL blueBoxPP $ labelL "HOME") ++! (dzenBoxStyleL whiteBoxPP $ fsPerc "/home")
myDateL      = (dzenBoxStyleL white2BBoxPP $ date "%A") ++! (dzenBoxStyleL whiteBoxPP $ date $ "%Y^fg(" ++ colorGray ++ ").^fg()%m^fg(" ++ colorGray ++ ").^fg()^fg(" ++ colorBlue ++ ")%d^fg() ^fg(" ++ colorGray ++ ")-^fg() %H^fg(" ++ colorGray ++ "):^fg()%M^fg(" ++ colorGray ++ "):^fg()^fg(" ++ colorGreen ++ ")%S^fg()") ++! (dzenClickStyleL calendarCA $ dzenBoxStyleL blueBoxPP $ labelL "CALENDAR")
myUptimeL    = (dzenBoxStyleL blue2BoxPP $ labelL "UPTIME") ++! (dzenBoxStyleL whiteBoxPP uptime)
myFocusL     = (dzenClickStyleL focusCA $ dzenBoxStyleL white2BBoxPP $ labelL "FOCUS") ++! (dzenBoxStyleL whiteBoxPP $ shortenL 100 logTitle)
myLayoutL    = (dzenClickStyleL layoutCA $ dzenBoxStyleL blue2BoxPP $ labelL "LAYOUT") ++! (dzenBoxStyleL whiteBoxPP $ onLogger (layoutText . removeWord . removeWord) logLayout) where
	removeWord = tail . dropWhile (/= ' ')
	layoutText xs
		| isPrefixOf "Mirror" xs   = layoutText $ removeWord xs ++ " ^fg(" ++ colorBlue ++ ")M^fg(" ++ colorGray ++ ")|^fg(" ++ colorWhiteAlt ++ ")"
		| isPrefixOf "ReflectY" xs = layoutText $ removeWord xs ++ " ^fg(" ++ colorBlue ++ ")Y^fg(" ++ colorGray ++ ")|^fg(" ++ colorWhiteAlt ++ ")"
		| isPrefixOf "ReflectX" xs = layoutText $ removeWord xs ++ " ^fg(" ++ colorBlue ++ ")X^fg(" ++ colorGray ++ ")|^fg(" ++ colorWhiteAlt ++ ")"
		| isPrefixOf "Switcher" xs = layoutText $ removeWord xs ++ " ^fg(" ++ colorRed ++ ")S^fg(" ++ colorGray ++ ")|^fg(" ++ colorWhiteAlt ++ ")"
		| isPrefixOf "Normal" xs   = layoutText $ removeWord xs ++ " ^fg(" ++ colorGreen ++ ")N^fg(" ++ colorGray ++ ")|^fg(" ++ colorWhiteAlt ++ ")"
		| isPrefixOf "Unique" xs   = layoutText $ removeWord xs ++ " ^fg(" ++ colorGreen ++ ")U^fg(" ++ colorGray ++ ")|^fg(" ++ colorWhiteAlt ++ ")"
		| otherwise                = concat $ reverse $ words xs
myWorkspaceL = (dzenClickStyleL workspaceCA $ dzenBoxStyleL blue2BoxPP $ labelL "WORKSPACE") ++! (dzenBoxStyleL whiteBoxPP $ onLogger namedWorkspaces logCurrent) where
	namedWorkspaces w
		| (elem w $ map show [0..9]) == True = "^fg(" ++ colorGreen ++ ")" ++ w ++ "^fg(" ++ colorGray ++ ")|^fg()" ++ workspaceNames !! (mod ((read w::Int) - 1) 10)
		| otherwise                          = "^fg(" ++ colorRed ++ ")x^fg(" ++ colorGray ++ ")|^fg()" ++ w


--------------------------------------------------------------------------------------------
-- BINDINGS CONFIG                                                                        --
--------------------------------------------------------------------------------------------

-- Key bindings
myKeys :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
myKeys conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $
	--Xmonad bindings
	[((modMask .|. shiftMask, xK_q), io (exitWith ExitSuccess))          --Quit xmonad
	, ((modMask, xK_q), restart "xmonad" True)                           --Restart xmonad
	, ((mod1Mask, xK_F2), shellPrompt myXPConfig)                        --Launch Xmonad shell prompt
	, ((modMask, xK_F2), xmonadPrompt myXPConfig)                        --Launch Xmonad prompt
	, ((mod1Mask, xK_F3), manPrompt myXPConfig)                          --Launch man prompt
	, ((modMask, xK_g), goToSelected $ myGSConfig myColorizer)           --Launch GridSelect
	, ((modMask, xK_masculine), scratchPad)                              --Scratchpad
	, ((modMask .|. shiftMask, xK_Return), spawn $ XMonad.terminal conf) --Launch default terminal
	--Window management bindings
	, ((modMask, xK_c), kill)                                              --Close focused window
	, ((mod1Mask, xK_F4), kill)
	, ((modMask, xK_n), refresh)                                           --Resize viewed windows to the correct size
	, ((modMask, xK_Tab), windows W.focusDown)                             --Move focus to the next window
	, ((modMask, xK_j), windows W.focusDown)
	, ((mod1Mask, xK_Tab), windows W.focusDown)
	, ((modMask, xK_k), windows W.focusUp)                                 --Move focus to the previous window
	, ((modMask, xK_a), windows W.focusMaster)                             --Move focus to the master window
	, ((modMask .|. shiftMask, xK_a), windows W.swapMaster)                --Swap the focused window and the master window
	, ((modMask .|. shiftMask, xK_j), windows W.swapDown)                  --Swap the focused window with the next window
	, ((modMask .|. shiftMask, xK_k), windows W.swapUp)                    --Swap the focused window with the previous window
	, ((modMask, xK_h), sendMessage Shrink)                                --Shrink the master area
	, ((modMask .|. shiftMask, xK_Left), sendMessage Shrink)
	, ((modMask, xK_l), sendMessage Expand)                                --Expand the master area
	, ((modMask .|. shiftMask, xK_Right), sendMessage Expand)
	, ((modMask .|. shiftMask, xK_h), sendMessage MirrorShrink)            --MirrorShrink the master area
	, ((modMask .|. shiftMask, xK_Down), sendMessage MirrorShrink)
	, ((modMask .|. shiftMask, xK_l), sendMessage MirrorExpand)            --MirrorExpand the master area
	, ((modMask .|. shiftMask, xK_Up), sendMessage MirrorExpand)
	, ((modMask, xK_t), withFocused $ windows . W.sink)                    --Push window back into tiling
	, ((modMask .|. shiftMask, xK_t), rectFloatFocused)                    --Push window into float
	, ((modMask, xK_m), withFocused minimizeWindow)                        --Minimize window
	, ((modMask, xK_b), withFocused (sendMessage . maximizeRestore))       --Maximize window
	, ((modMask .|. shiftMask, xK_m), sendMessage RestoreNextMinimizedWin) --Restore window
	, ((modMask .|. shiftMask, xK_f), fullFloatFocused)                    --Push window into full screen
	, ((modMask, xK_comma), sendMessage (IncMasterN 1))                    --Increment the number of windows in the master area
	, ((modMask, xK_period), sendMessage (IncMasterN (-1)))                --Deincrement the number of windows in the master area
	, ((modMask, xK_Right), sendMessage $ Go R)                            --Change focus to right
	, ((modMask, xK_Left ), sendMessage $ Go L)                            --Change focus to left
	, ((modMask, xK_Up   ), sendMessage $ Go U)                            --Change focus to up
	, ((modMask, xK_Down ), sendMessage $ Go D)                            --Change focus to down
	, ((modMask .|. controlMask, xK_Right), sendMessage $ Swap R)          --Swap focused window to right
	, ((modMask .|. controlMask, xK_Left ), sendMessage $ Swap L)          --Swap focused window to left
	, ((modMask .|. controlMask, xK_Up   ), sendMessage $ Swap U)          --Swap focused window to up
	, ((modMask .|. controlMask, xK_Down ), sendMessage $ Swap D)          --Swap focused window to down
	--Layout management bindings
	, ((modMask, xK_space), sendMessage NextLayout)                                                                                    --Rotate through the available layout algorithms
	, ((modMask, xK_v ), sendMessage ToggleLayout)                                                                                     --Toggle window titles (can click drag to move windows)
	, ((modMask .|. shiftMask, xK_space ), flashText myTextConfig 1 " Set to Default Layout " >> (setLayout $ XMonad.layoutHook conf)) --Reset layout to workspaces default
	, ((modMask, xK_f), sendMessage $ XMonad.Layout.MultiToggle.Toggle TABBED)                                                         --Push layout into tabbed
	, ((modMask .|. controlMask, xK_f), sendMessage $ XMonad.Layout.MultiToggle.Toggle FLOATED)                                        --Push layout into float
	, ((modMask .|. shiftMask, xK_z), sendMessage $ XMonad.Layout.MultiToggle.Toggle MIRROR)                                           --Push layout into mirror
	, ((modMask .|. shiftMask, xK_x), sendMessage $ XMonad.Layout.MultiToggle.Toggle REFLECTX)                                         --Reflect layout by X
	, ((modMask .|. shiftMask, xK_y), sendMessage $ XMonad.Layout.MultiToggle.Toggle REFLECTY)                                         --Reflect layout by Y
	--Gaps management bindings
	, ((modMask .|. controlMask, xK_t), sendMessage $ ToggleStruts ) --toogle the all struts
	, ((modMask .|. controlMask, xK_u), sendMessage $ ToggleStrut U) --toogle the top strut
	, ((modMask .|. controlMask, xK_d), sendMessage $ ToggleStrut D) --toogle the bottom strut
	--Scripts management bindings
	, ((modMask, xK_x), spawn "/usr/bin/xcalib -invert -alter")                                                           --Invert colors in X
	, ((modMask, xK_d), spawn "/usr/bin/killall dzen2 haskell-cpu-usage.out")                                             --Kill dzen2
	, ((0, 0x1008ffa9), spawn "/home/nnoell/bin/touchpadtoggle.sh")                                                       --Toggle touchpad (xmodmap -pk | grep -i toggle)
	, ((0, xF86XK_AudioMute), spawn "/home/nnoell/bin/voldzen.sh t -d")                                                   --Mute/unmute volume
	, ((0, xF86XK_AudioRaiseVolume), spawn "/home/nnoell/bin/voldzen.sh + -d")                                            --Raise volume
	, ((mod1Mask, xK_Up), spawn "/home/nnoell/bin/voldzen.sh + -d")
	, ((0, xF86XK_AudioLowerVolume), spawn "/home/nnoell/bin/voldzen.sh - -d")                                            --Lower volume
	, ((mod1Mask, xK_Down), spawn "/home/nnoell/bin/voldzen.sh - -d")
	, ((0, xF86XK_AudioNext),  flashText myTextConfig 1 " Next Song " >> spawn "/usr/bin/ncmpcpp next")                   --Next song
	, ((mod1Mask, xK_Right), flashText myTextConfig 1 " Next Song " >> spawn "/usr/bin/ncmpcpp next")
	, ((0, xF86XK_AudioPrev), flashText myTextConfig 1 " Previous Song " >> spawn "/usr/bin/ncmpcpp prev")                --Prev song
	, ((mod1Mask, xK_Left), flashText myTextConfig 1 " Previous Song " >> spawn "/usr/bin/ncmpcpp prev")
	, ((0, xF86XK_AudioPlay), flashText myTextConfig 1 " Song Toggled " >> spawn "/usr/bin/ncmpcpp toggle")               --Toggle song
	, ((mod1Mask .|. controlMask, xK_Down), flashText myTextConfig 1 " Song Toggled " >> spawn "/usr/bin/ncmpcpp toggle")
	, ((0, xF86XK_AudioStop), flashText myTextConfig 1 " Song Stopped " >> spawn "/usr/bin/ncmpcpp stop")                 --Stop song
	, ((mod1Mask .|. controlMask, xK_Up), flashText myTextConfig 1 " Song Stopped " >> spawn "ncmpcpp stop")
	, ((0, xF86XK_MonBrightnessUp), spawn "/home/nnoell/bin/bridzen.sh")                                                  --Raise brightness
	, ((0, xF86XK_MonBrightnessDown), spawn "/home/nnoell/bin/bridzen.sh")                                                --Lower brightness
	, ((0, xF86XK_ScreenSaver), spawn "/home/nnoell/bin/turnoffscreen.sh")                                                --Lock screen
	, ((0, xK_Print), spawn "/usr/bin/scrot '%Y-%m-%d_$wx$h.png'")                                                        --Take a screenshot
	, ((modMask , xK_s), spawn "/home/nnoell/bin/turnoffscreen.sh")                                                       --Turn off screen
	--Workspaces management bindings
	, ((mod1Mask, xK_comma), flashText myTextConfig 1 " Toggled to Previous Workspace " >> toggleWS)                          --Toggle to the workspace displayed previously
	, ((mod1Mask, xK_masculine), flashText myTextConfig 1 " Switching with Workspace 1 " >> toggleOrView (myWorkspaces !! 0)) --If ws != 0 then move to workspace 0, else move to latest ws I was
	, ((mod1Mask .|. controlMask, xK_Left), flashText myTextConfig 1 " Moved to Previous Workspace " >> prevWS)               --Move to previous Workspace
	, ((mod1Mask .|. controlMask, xK_Right), flashText myTextConfig 1 " Moved to Next Workspace " >> nextWS)                  --Move to next Workspace
	, ((modMask .|. shiftMask, xK_n), flashText myTextConfig 1 " Shifted to Next Workspace " >> shiftToNext)                  --Send client to next workspace
	, ((modMask .|. shiftMask, xK_p), flashText myTextConfig 1 " Shifted to Previous Workspace " >> shiftToPrev)              --Send client to previous workspace
	] ++
	[ ((m .|. modMask, k), windows $ f i)                                                        --Switch to n workspaces and send client to n workspaces
	  | (i, k) <- zip (XMonad.workspaces conf) ([xK_1 .. xK_9] ++ [xK_0])
	  , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]
	] ++
	[ ((m .|. modMask, key), screenWorkspace sc >>= flip whenJust (windows . f))                 --Switch to n screens and send client to n screens
	  | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
	  , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]
	] where
		fullFloatFocused = withFocused $ \f -> windows =<< appEndo `fmap` runQuery doFullFloat f
		rectFloatFocused = withFocused $ \f -> windows =<< appEndo `fmap` runQuery (doRectFloat $ RationalRect 0.05 0.05 0.9 0.9) f

-- Mouse bindings
myMouseBindings :: XConfig Layout -> M.Map (KeyMask, Button) (Window -> X ())
myMouseBindings (XConfig {XMonad.modMask = modMask}) = M.fromList $
	[ ((modMask, button1), (\w -> focus w >> mouseMoveWindow w >> windows W.shiftMaster)) --Set the window to floating mode and move by dragging
	, ((modMask, button2), (\w -> focus w >> windows W.shiftMaster))                      --Raise the window to the top of the stack
	, ((modMask, button3), (\w -> focus w >> Flex.mouseResizeWindow w))                   --Set the window to floating mode and resize by dragging
	, ((modMask, button4), (\_ -> prevWS))                                                --Switch to previous workspace
	, ((modMask, button5), (\_ -> nextWS))                                                --Switch to next workspace
	, (((modMask .|. shiftMask), button4), (\_ -> shiftToPrev))                           --Send client  to previous workspace
	, (((modMask .|. shiftMask), button5), (\_ -> shiftToNext))                           --Send client  to next workspace
	]
