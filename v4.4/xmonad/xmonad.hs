--------------------------------------------------------------------------------------------
-- File   : ~/.xmonad/xmonad.hs                                                           --
-- Author : Nnoell <nnoell3[at]gmail.com>                                                 --
-- Desc   : My XMonad config                                                              --
-- Build  : You might need to compile using -fcontext-stack=32:                           --
--          $ ghc -fcontext-stack=32 xmonad.hs -o xmonad-x86_64-linux                     --
--------------------------------------------------------------------------------------------

-- Options
{-# LANGUAGE DeriveDataTypeable, NoMonomorphismRestriction, MultiParamTypeClasses, ImplicitParams #-}

-- Modules
import XMonad
import XMonad.Layout
import XMonad.Layout.IM
import XMonad.Layout.Gaps
import XMonad.Layout.Named
import XMonad.Layout.Tabbed
import XMonad.Layout.OneBig
import XMonad.Layout.Reflect
import XMonad.Layout.MosaicAlt
import XMonad.Layout.NoFrillsDecoration
import XMonad.Layout.SimplestFloat
import XMonad.Layout.NoBorders
import XMonad.Layout.ResizableTile
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Minimize
import XMonad.Layout.Maximize
import XMonad.Layout.ToggleLayouts
import XMonad.Layout.MagicFocus
import XMonad.Layout.WindowNavigation
import XMonad.Layout.WindowSwitcherDecoration
import XMonad.Layout.DraggingVisualizer
import XMonad.Layout.LayoutBuilder
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
import XMonad.Util.Run
import XMonad.Util.Scratchpad
import XMonad.Util.NamedScratchpad
import XMonad.Actions.CycleWS
import XMonad.Actions.ShowText
import XMonad.Actions.GridSelect
import XMonad.Actions.MouseResize
import XMonad.Actions.FloatKeys
import Data.Monoid
import Data.List
import System.Exit
import System.IO
import Control.Concurrent
import Graphics.X11.ExtraTypes.XF86
import Graphics.X11.Xinerama
import Control.Applicative
import Control.Exception as E
import qualified XMonad.StackSet as W
import qualified Data.Map as M
import qualified XMonad.Actions.FlexibleResize as Flex
import qualified XMonad.Util.ExtensibleState as XS


--------------------------------------------------------------------------------------------
-- MAIN                                                                                   --
--------------------------------------------------------------------------------------------

main :: IO ()
main = do
	r <- getScreenRes ":0" 0  --display ":0", screen 0
	topLeftBar  <- dzenSpawnPipe $ dzenTopLeftFlags r
	topRightBar <- dzenSpawnPipe $ dzenTopRightFlags r
	botLeftBar  <- dzenSpawnPipe $ dzenBotLeftFlags r
	botRightBar <- dzenSpawnPipe $ dzenBotRightFlags r
	xmonad $ myUrgencyHook defaultConfig
		{ terminal           = "/usr/bin/urxvtc" --default terminal
		, modMask            = mod4Mask          --default modMask
		, focusFollowsMouse  = True              --focus follow config
		, clickJustFocuses   = True              --focus click config
		, borderWidth        = 1                 --border width
		, normalBorderColor  = colorBlackAlt     --border color
		, focusedBorderColor = colorWhiteAlt2    --focused border color
		, workspaces         = myWorkspaces      --workspace names
		, startupHook        = myStartupHook     --autostart config
		, handleEventHook    = myHandleEventHook --event config
		, layoutHook         = myLayoutHook      --layout config
		, manageHook         = myManageHook      --xprop config
		, logHook            =                   --status bar config
			myTopLeftLogHook topLeftBar   <+>      --top left dzen
			myTopRightLogHook topRightBar <+>      --top right dzen
			myBotLeftLogHook botLeftBar   <+>      --bottom left dzen
			myBotRightLogHook botRightBar <+>      --bottom right dzen
			ewmhDesktopsLogHook           >>
			setWMName "LG3D"
		, keys               = myKeys            --key bindings config
		, mouseBindings      = myMouseBindings   --mouse bindings config
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
boxLeftIcon    = "/home/nnoell/.icons/xbm_icons/subtle/boxleft.xbm"  --left icon of dzen boxes
boxLeftIcon2   = "/home/nnoell/.icons/xbm_icons/subtle/boxleft2.xbm" --left icon2 of dzen boxes
boxRightIcon   = "/home/nnoell/.icons/xbm_icons/subtle/boxright.xbm" --right icon of dzen boxes
xDefRes        = 1366 --no longer used
yDefRes        = 768  --no longer used
panelHeight    = 16   --height of top and bottom panels
boxHeight      = 14   --height of dzen logger box
topPanelSepPos = 950  --left-right alignment pos of top panel
botPanelSepPos = 450  --left-right alignment pos of bottom panel

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

-- Workspace index (do not change)
myWorkspaces :: [WorkspaceId]
myWorkspaces = map show $ [1..9] ++ [0]

-- Workspace names
workspaceNames :: [WorkspaceId]
workspaceNames =
	[ "Terminal"
	, "Network"
	, "Development"
	, "Graphics"
	, "Chatting"
	, "Video"
	, "Alternate1"
	, "Alternate2"
	, "Alternate3"
	, "Alternate4"
	]

-- Layout names (must be one word /= to: Mirror, ReflectX, ReflectY, Switcher, Normal and Unique)
myTileName = "Tiled"
myMirrName = "MirrTld"
myMosAName = "Mosaic"
myOneBName = "OneBig"
myCst1Name = "Default"
myCst2Name = "MstrTab"
myCst3Name = "Web"
myChatName = "Chat"
myFTabName = "Full"
myFloaName = "Float"


--------------------------------------------------------------------------------------------
-- STARTUP HOOK CONFIG                                                                    --
--------------------------------------------------------------------------------------------

-- Startup Hook
myStartupHook =
	(setDefaultCursor xC_left_ptr) <+>
	(spawn "/usr/bin/feh --bg-scale ~/Pictures/wallpapers/xmonad/xmonad_def_black.png") <+>
	(spawn "/usr/bin/killall haskell-cpu-usage.out") <+>
	(liftIO $ threadDelay 1000000) <+> --needed so that xmonad can be launched on the fly without crashing
	(spawn "/home/nnoell/.xmonad/apps/haskell-cpu-usage.out 5") <+>
	(startTimer 1 >>= XS.put . TID)


--------------------------------------------------------------------------------------------
-- HANDLE EVENT HOOK CONFIG                                                               --
--------------------------------------------------------------------------------------------

-- Wrapper for the Timer id, so it can be stored as custom mutable state
data TidState = TID TimerId deriving Typeable

instance ExtensionClass TidState where
	initialValue = TID 0

-- Handle event hook
myHandleEventHook =
	fullscreenEventHook <+> docksEventHook <+>
	clockEventHook <+> handleTimerEvent <+>
	notFocusFloat where
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
-- LAYOUT CONFIG                                                                          --
--------------------------------------------------------------------------------------------

-- Tabbed transformer (W+f)
data TABBED = TABBED deriving (Read, Show, Eq, Typeable)
instance Transformer TABBED Window where
	transform TABBED x k = k myFTabU (\_ -> x)

-- Floated transformer (W+ctl+f)
data FLOATED = FLOATED deriving (Read, Show, Eq, Typeable)
instance Transformer FLOATED Window where
	transform FLOATED x k = k myFloaU (\_ -> x)

-- Unique Layouts
myFTabU = smartBorders $ named ("Unique " ++ myFTabName) $ tabbedAlways shrinkText myTitleTheme
myFloaU = named ("Unique " ++ myFloaName) $ mouseResize $ noFrillsDeco shrinkText myTitleTheme simplestFloat

-- Layout hook
myLayoutHook =
	gaps [(U,panelHeight), (D,panelHeight)] $
	configurableNavigation noNavigateBorders $
	minimize $
	maximize $
	mkToggle (single TABBED) $
	mkToggle (single FLOATED) $
	mkToggle (single MIRROR) $
	mkToggle (single REFLECTX) $
	mkToggle (single REFLECTY) $
	onWorkspace (myWorkspaces !! 1) webLayouts $
	onWorkspace (myWorkspaces !! 2) codeLayouts $
	onWorkspace (myWorkspaces !! 4) chatLayouts $
	allLayouts where
		--per workspace layouts
		webLayouts  = (myToggleL myCst3 myCst3Name) ||| (myToggleL myCst1 myCst1Name)                                   --workspace 2 layouts
		codeLayouts = (myToggleL myCst2 myCst2Name) ||| (myToggleL myOneB myOneBName) ||| (myToggleL myTile myTileName) --workspace 3 layouts
		chatLayouts = myToggleL (withIM (0.2) (Title "Buddy List") myMosA) myChatName                                   --workspace 5 layouts
		allLayouts  =                                                                                                   -- rest of workspaces layouts
			(myToggleL myCst1 myCst1Name) |||
			(myToggleL myCst2 myCst2Name) |||
			(myToggleL myTile myTileName) |||
			(myToggleL myOneB myOneBName) |||
			(myToggleL myMirr myMirrName) |||
			(myToggleL myMosA myMosAName) |||
			(myToggleL myCst3 myCst3Name)
		--layouts
		myTile = ResizableTall 1 0.03 0.5 []                                                                               --default xmonad layout
		myMirr = Mirror myTile                                                                                             --mirror default xmonad layout
		myMosA = MosaicAlt M.empty                                                                                         --default mosaicAlt layout
		myOneB = OneBig 0.75 0.65                                                                                          --default OneBig layout
		myCst1 = (layoutN 2 (relBox 0 0 1 0.6) (Just $ relBox 0 0 1 1) $ myTile) $ (layoutAll (relBox 0 0.6 1 1) $ myTabb) --custom1 layout
		myCst2 = (layoutN 1 (relBox 0 0 0.4 1) (Just $ relBox 0 0 1 1) $ myTile) $ (layoutAll (relBox 0.4 0 1 1) $ myTabb) --custom2 layout
		myCst3 = (layoutN 1 (relBox 0 0 1 0.7) (Just $ relBox 0 0 1 1) $ myTabb) $ (layoutAll (relBox 0 0.7 1 1) $ myTabb) --custom3 layout
		myChat = withIM (0.20) (Title "Buddy List") myMosA                                                                 --custom chat layout
		myTabb = tabbed shrinkText myTitleTheme                                                                            --default tabbed layout
		--costom draggingVisualizer toggle
		myToggleL l n = smartBorders $ toggleLayouts (named ("Switcher " ++ n) $ switcher l) (named ("Normal " ++ n) l) where
			switcher l = windowSwitcherDecoration shrinkText myTitleTheme $ draggingVisualizer l


--------------------------------------------------------------------------------------------
-- MANAGE HOOK CONFIG                                                                     --
--------------------------------------------------------------------------------------------

-- Manage Hook
myManageHook :: ManageHook
myManageHook =
	manageDocks <+>
	(scratchpadManageHook $ W.RationalRect 0 0 1 (3/4)) <+>
	dynamicMasterHook <+>
	manageWindows

-- Manage Windows
manageWindows :: ManageHook
manageWindows = composeAll . concat $
	[ [ resource  =? r --> doIgnore                    | r <- myIgnores ]
	, [ className =? c --> doShift (myWorkspaces !! 1) | c <- myWebS    ]
	, [ className =? c --> doShift (myWorkspaces !! 2) | c <- myCodeS   ]
	, [ className =? c --> doShift (myWorkspaces !! 3) | c <- myGfxS    ]
	, [ className =? c --> doShift (myWorkspaces !! 4) | c <- myChatS   ]
	, [ className =? c --> doShift (myWorkspaces !! 9) | c <- myAlt3S   ]
	, [ className =? c --> doCenterFloat               | c <- myFloatCC ]
	, [ name      =? n --> doCenterFloat               | n <- myFloatCN ]
	, [ name      =? n --> doSideFloat NW              | n <- myFloatSN ]
	, [ className =? c --> doF W.focusDown             | c <- myFocusDC ]
	, [ currentWs =? (myWorkspaces !! 1) --> keepMaster "Chromium"      ]
	, [ isFullscreen   --> doFullFloat ]
	] where
		name      = stringProperty "WM_NAME"
		myIgnores = ["desktop", "desktop_window"]
		myWebS    = ["Chromium", "Firefox", "Opera"]
		myCodeS   = ["NetBeans IDE 7.3"]
		myChatS   = ["Pidgin", "Xchat"]
		myGfxS    = ["Gimp", "gimp", "GIMP"]
		myAlt3S   = ["Amule", "Transmission-gtk"]
		myFloatCC = ["MPlayer", "mplayer2", "File-roller", "zsnes", "Gcalctool", "Exo-helper-1"
		            , "Gksu", "Galculator", "Nvidia-settings", "XFontSel", "XCalc", "XClock"
		            , "Ossxmix", "Xvidcap", "Main", "Wicd-client.py"]
		myFloatCN = ["Choose a file", "Open Image", "File Operation Progress", "Firefox Preferences"
		            , "Preferences", "Search Engines", "Set up sync", "Passwords and Exceptions"
		            , "Autofill Options", "Rename File", "Copying files", "Moving files"
		            , "File Properties", "Replace", "Quit GIMP", "Change Foreground Color"
		            , "Change Background Color", ""]
		myFloatSN = ["Event Tester"]
		myFocusDC = ["Event Tester", "Notify-osd"]
		keepMaster c = assertSlave <+> assertMaster where
			assertSlave = fmap (/= c) className --> doF W.swapDown
			assertMaster = className =? c --> doF W.swapMaster

--------------------------------------------------------------------------------------------
-- DZEN STATUS BARS CONFIG                                                                --
--------------------------------------------------------------------------------------------

-- urgencyHook
myUrgencyHook :: LayoutClass l Window => XConfig l -> XConfig l
myUrgencyHook = withUrgencyHook dzenUrgencyHook
	{ duration = 2000000
	, args =
		[ "-x", "0"
		, "-y", "0"
		, "-h", show panelHeight
		, "-w", show topPanelSepPos
		, "-fn", dzenFont
		, "-bg", colorBlack
		, "-fg", colorGreen
		]
	}

-- Dzen top left bar flags
dzenTopLeftFlags :: Res -> DF
dzenTopLeftFlags _ = DF
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
myTopLeftLogHook h = dynamicLogWithPP defaultPP
	{ ppOutput = hPutStrLn h
	, ppOrder  = \(_:_:_:x) -> x
	, ppSep    = " "
	, ppExtras = [ myLayoutL, myWorkspaceL, myFocusL ]
	}

-- Dzen top right bar flags
dzenTopRightFlags :: Res -> DF
dzenTopRightFlags r = DF
	{ xPosDF       = topPanelSepPos
	, yPosDF       = 0
	, widthDF      = (xRes r) - topPanelSepPos
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
myTopRightLogHook h = dynamicLogWithPP defaultPP
	{ ppOutput  = hPutStrLn h
	, ppOrder   = \(_:_:_:x) -> x
	, ppSep     = " "
	, ppExtras  = [ myUptimeL, myDateL ]
	}

-- Dzen bottom left bar flags
dzenBotLeftFlags :: Res -> DF
dzenBotLeftFlags r = DF
	{ xPosDF       = 0
	, yPosDF       = (yRes r) - panelHeight
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
	, ppOrder           = \(ws:_:_:x) -> [ws] ++ x
	, ppSep             = " "
	, ppWsSep           = ""
	, ppCurrent         = dzenBoxStyle blue2BBoxPP
	, ppUrgent          = dzenBoxStyle green2BBoxPP . dzenClickWorkspace
	, ppVisible         = dzenBoxStyle blackBoxPP . dzenClickWorkspace
	, ppHiddenNoWindows = dzenBoxStyle blackBoxPP . dzenClickWorkspace
	, ppHidden          = dzenBoxStyle whiteBoxPP . dzenClickWorkspace
	, ppExtras          = [ myResL, myBrightL ]
	} where
		dzenClickWorkspace ws = "^ca(1," ++ xdo "w;" ++ xdo index ++ ")" ++ "^ca(3," ++ xdo "w;" ++ xdo index ++ ")" ++ ws ++ "^ca()^ca()" where
			wsIdxToString Nothing = "1"
			wsIdxToString (Just n) = show $ mod (n+1) $ length myWorkspaces
			index = wsIdxToString (elemIndex ws myWorkspaces)
			xdo key = "/usr/bin/xdotool key super+" ++ key

-- Dzen bottom right bar flags
dzenBotRightFlags :: Res -> DF
dzenBotRightFlags r = DF
	{ xPosDF       = botPanelSepPos
	, yPosDF       = (yRes r) - panelHeight
	, widthDF      = (xRes r) - botPanelSepPos
	, heightDF     = panelHeight
	, alignementDF = "r"
	, fgColorDF    = colorBlue
	, bgColorDF    = colorBlack
	, fontDF       = dzenFont
	, eventDF      = "onstart=lower"
	, extrasDF     = "-p"
	}

-- Bottom right bar logHook
myBotRightLogHook :: Handle -> X ()
myBotRightLogHook h = dynamicLogWithPP defaultPP
	{ ppOutput = hPutStrLn h
	, ppOrder  = \(_:_:_:x) -> x
	, ppSep    = " "
	, ppExtras = [ myCpuL, myMemL, myTempL, myWifiL, myBatL ]
	}


--------------------------------------------------------------------------------------------
-- LOGGERS CONFIG                                                                         --
--------------------------------------------------------------------------------------------

-- BotRight Loggers
myBatL =
	(dzenBoxStyleL gray2BoxPP $ labelL "BATTERY") ++!
	(dzenBoxStyleL blueBoxPP  $ batPercent 30 colorRed) ++!
	(dzenBoxStyleL whiteBoxPP batStatus)
myWifiL =
	(dzenBoxStyleL gray2BoxPP $ labelL "WIFI") ++!
	(dzenBoxStyleL blueBoxPP wifiSignal)
myTempL =
	(dzenBoxStyleL gray2BoxPP $ labelL "TEMP") ++!
	(dzenBoxStyleL blueBoxPP  $ cpuTemp 2 70 colorRed) --2 because I have 2 thermal zones
myMemL =
	(dzenBoxStyleL gray2BoxPP $ labelL "MEM") ++!
	(dzenBoxStyleL blueBoxPP  $ memUsage [percMemUsage, totMBMemUsage])
myCpuL =
	(dzenBoxStyleL gray2BoxPP $ labelL "CPU") ++!
	(dzenBoxStyleL blueBoxPP  $ cpuUsage "/tmp/haskell-cpu-usage.txt" 70 colorRed)

-- BotLeft Loggers
myResL =
	(dzenBoxStyleL blue2BoxPP $ labelL "RES") ++!
	(dzenBoxStyleL whiteBoxPP $ screenRes ":0" 0)
myBrightL =
	(dzenBoxStyleL blue2BoxPP $ labelL "BRIGHT") ++!
	(dzenBoxStyleL whiteBoxPP $ brightPerc 15) --15 because brightness go from 0 to 15 in my case, usually must be 10

-- TopRight Loggers
myDateL =
	(dzenBoxStyleL white2BBoxPP $ date "%A") ++!
	(dzenBoxStyleL whiteBoxPP   $ date $ "%Y^fg(" ++ colorGray ++ ").^fg()%m^fg(" ++ colorGray ++ ").^fg()^fg(" ++ colorBlue ++ ")%d^fg()") ++!
	(dzenBoxStyleL whiteBoxPP   $ date $ "%H^fg(" ++ colorGray ++ "):^fg()%M^fg(" ++ colorGray ++ "):^fg()^fg(" ++ colorGreen ++ ")%S^fg()") ++!
	(dzenClickStyleL calendarCA $ dzenBoxStyleL blueBoxPP $ labelL "CALENDAR")
myUptimeL =
	(dzenBoxStyleL blue2BoxPP   $ labelL "UPTIME") ++!
	(dzenBoxStyleL whiteBoxPP uptime)

-- TopLeft Loggers
myFocusL  =
	(dzenClickStyleL focusCA  $ dzenBoxStyleL white2BBoxPP $ labelL "FOCUS") ++!
	(dzenBoxStyleL whiteBoxPP $ shortenL 100 logTitle)
myLayoutL =
	(dzenClickStyleL layoutCA $ dzenBoxStyleL blue2BoxPP $ labelL "LAYOUT") ++!
	(dzenBoxStyleL whiteBoxPP $ onLogger (layoutText . removeWord . removeWord) logLayout) where
		removeWord xs = tail $ dropWhile (/= ' ') xs
		layoutText xs
			| isPrefixOf "Mirror" xs   = layoutText $ removeWord xs ++ " ^fg(" ++ colorBlue  ++ ")M^fg(" ++ colorGray ++ ")|^fg(" ++ colorWhiteAlt ++ ")"
			| isPrefixOf "ReflectY" xs = layoutText $ removeWord xs ++ " ^fg(" ++ colorBlue  ++ ")Y^fg(" ++ colorGray ++ ")|^fg(" ++ colorWhiteAlt ++ ")"
			| isPrefixOf "ReflectX" xs = layoutText $ removeWord xs ++ " ^fg(" ++ colorBlue  ++ ")X^fg(" ++ colorGray ++ ")|^fg(" ++ colorWhiteAlt ++ ")"
			| isPrefixOf "Switcher" xs = layoutText $ removeWord xs ++ " ^fg(" ++ colorRed   ++ ")S^fg(" ++ colorGray ++ ")|^fg(" ++ colorWhiteAlt ++ ")"
			| isPrefixOf "Normal" xs   = layoutText $ removeWord xs ++ " ^fg(" ++ colorGreen ++ ")N^fg(" ++ colorGray ++ ")|^fg(" ++ colorWhiteAlt ++ ")"
			| isPrefixOf "Unique" xs   = layoutText $ removeWord xs ++ " ^fg(" ++ colorGreen ++ ")U^fg(" ++ colorGray ++ ")|^fg(" ++ colorWhiteAlt ++ ")"
			| otherwise                = concat $ reverse $ words xs
myWorkspaceL =
	(dzenClickStyleL workspaceCA $ dzenBoxStyleL blue2BoxPP $ labelL "WORKSPACE") ++!
	(dzenBoxStyleL whiteBoxPP $ onLogger namedWorkspaces logCurrent) where
		namedWorkspaces w
			| (elem w $ map show [0..9]) = "^fg(" ++ colorGreen ++ ")" ++ w ++ "^fg(" ++ colorGray ++ ")|^fg()" ++ workspaceNames !! (mod ((read w::Int) - 1) 10)
			| otherwise                  = "^fg(" ++ colorRed   ++ ")x^fg(" ++ colorGray ++ ")|^fg()" ++ w


--------------------------------------------------------------------------------------------
-- BINDINGS CONFIG                                                                        --
--------------------------------------------------------------------------------------------

-- Key bindings
myKeys :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
myKeys conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $
	--Xmonad bindings
	[((modMask .|. shiftMask, xK_q), killAndExit)                        --Quit xmonad
	, ((modMask, xK_q), killAndRestart)                                  --Restart xmonad
	, ((0, xK_Pause), killAndRestart)
	, ((mod1Mask, xK_F2), shellPrompt myXPConfig)                        --Launch Xmonad shell prompt
	, ((modMask, xK_F2), xmonadPrompt myXPConfig)                        --Launch Xmonad prompt
	, ((mod1Mask, xK_F3), manPrompt myXPConfig)                          --Launch man prompt
	, ((modMask, xK_g), goToSelected $ myGSConfig myColorizer)           --Launch GridSelect
	, ((modMask, xK_masculine), scratchPad)                              --Scratchpad (0x0060 = grave key)
	, ((modMask, 0x0060), scratchPad)
	, ((modMask .|. shiftMask, xK_Return), spawn $ XMonad.terminal conf) --Launch default terminal
	--Window management bindings
	, ((modMask, xK_c), kill)                                                 --Close focused window
	, ((mod1Mask, xK_F4), kill)
	, ((modMask, xK_n), refresh)                                              --Resize viewed windows to the correct size
	, ((modMask, xK_Tab), windows W.focusDown)                                --Move focus to the next window
	, ((modMask, xK_j), windows W.focusDown)
	, ((mod1Mask, xK_Tab), windows W.focusDown)
	, ((modMask, xK_k), windows W.focusUp)                                    --Move focus to the previous window
	, ((modMask, xK_a), windows W.focusMaster)                                --Move focus to the master window
	, ((modMask .|. shiftMask, xK_a), windows W.swapMaster)                   --Swap the focused window and the master window
	, ((modMask .|. shiftMask, xK_j), windows W.swapDown)                     --Swap the focused window with the next window
	, ((modMask .|. shiftMask, xK_k), windows W.swapUp)                       --Swap the focused window with the previous window
	, ((modMask, xK_h), sendMessage Shrink)                                   --Shrink the master area
	, ((modMask .|. shiftMask, xK_Left), sendMessage Shrink)
	, ((modMask, xK_l), sendMessage Expand)                                   --Expand the master area
	, ((modMask .|. shiftMask, xK_Right), sendMessage Expand)
	, ((modMask .|. shiftMask, xK_h), sendMessage MirrorShrink)               --MirrorShrink the master area
	, ((modMask .|. shiftMask, xK_Down), sendMessage MirrorShrink)
	, ((modMask .|. shiftMask, xK_l), sendMessage MirrorExpand)               --MirrorExpand the master area
	, ((modMask .|. shiftMask, xK_Up), sendMessage MirrorExpand)
	, ((modMask, xK_t), withFocused $ windows . W.sink)                       --Push window back into tiling
	, ((modMask .|. shiftMask, xK_t), rectFloatFocused)                       --Push window into float
	, ((modMask, xK_m), withFocused minimizeWindow)                           --Minimize window
	, ((modMask, xK_b), withFocused (sendMessage . maximizeRestore))          --Maximize window
	, ((modMask .|. shiftMask, xK_m), sendMessage RestoreNextMinimizedWin)    --Restore window
	, ((modMask .|. shiftMask, xK_f), fullFloatFocused)                       --Push window into full screen
	, ((modMask, xK_comma), sendMessage (IncMasterN 1))                       --Increment the number of windows in the master area
	, ((modMask, xK_period), sendMessage (IncMasterN (-1)))                   --Deincrement the number of windows in the master area
	, ((modMask, xK_Right), sendMessage $ Go R)                               --Change focus to right
	, ((modMask, xK_Left ), sendMessage $ Go L)                               --Change focus to left
	, ((modMask, xK_Up   ), sendMessage $ Go U)                               --Change focus to up
	, ((modMask, xK_Down ), sendMessage $ Go D)                               --Change focus to down
	, ((modMask .|. controlMask, xK_Right), sendMessage $ Swap R)             --Swap focused window to right
	, ((modMask .|. controlMask, xK_Left ), sendMessage $ Swap L)             --Swap focused window to left
	, ((modMask .|. controlMask, xK_Up   ), sendMessage $ Swap U)             --Swap focused window to up
	, ((modMask .|. controlMask, xK_Down ), sendMessage $ Swap D)             --Swap focused window to down
	, ((modMask .|. mod1Mask, xK_Left), withFocused (keysMoveWindow (-30,0))) -- move floated window 10 pixels left
	, ((modMask .|. mod1Mask, xK_Right), withFocused (keysMoveWindow (30,0))) -- move floated window 10 pixels right
	, ((modMask .|. mod1Mask, xK_Up), withFocused (keysMoveWindow (0,-30)))   -- move floated window 10 pixels up
	, ((modMask .|. mod1Mask, xK_Down), withFocused (keysMoveWindow (0,30)))  -- move floated window 10 pixels down
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
	, ((modMask .|. controlMask, xK_t), sendMessage $ ToggleGaps ) --toogle the all gaps
	, ((0, xF86XK_Calculator), sendMessage $ ToggleGaps)
	, ((modMask .|. controlMask, xK_u), sendMessage $ ToggleGap U) --toogle the top gaps
	, ((modMask .|. controlMask, xK_d), sendMessage $ ToggleGap D) --toogle the bottom gaps
	--Scripts management bindings
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
	, ((0, 0xff14), spawn "/home/nnoell/bin/turnoffscreen.sh")
	, ((0, xK_Print), spawn "/usr/bin/scrot '%Y-%m-%d_$wx$h.png'" >> flashText myTextConfig 1 " Screenshot Saved ")       --Take a screenshot
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
		scratchPad = scratchpadSpawnActionCustom "/usr/bin/urxvtc -name scratchpad"
		fullFloatFocused = withFocused $ \f -> windows =<< appEndo `fmap` runQuery doFullFloat f
		rectFloatFocused = withFocused $ \f -> windows =<< appEndo `fmap` runQuery (doRectFloat $ W.RationalRect 0.05 0.05 0.9 0.9) f
		killAndExit =
			(spawn "/usr/bin/killall dzen2 haskell-cpu-usage.out") <+>
			io (exitWith ExitSuccess)
		killAndRestart =
			(spawn "/usr/bin/killall dzen2 haskell-cpu-usage.out") <+>
			(liftIO $ threadDelay 1000000) <+>
			(restart "xmonad" True)

-- Mouse bindings
myMouseBindings :: XConfig Layout -> M.Map (KeyMask, Button) (Window -> X ())
myMouseBindings (XConfig {XMonad.modMask = modMask}) = M.fromList $
	[ ((modMask, button1), (\w -> focus w >> mouseMoveWindow w >> windows W.shiftMaster)) --Set the window to floating mode and move by dragging
	, ((modMask, button2), (\w -> focus w >> windows W.shiftMaster))                      --Raise the window to the top of the stack
	, ((modMask, button3), (\w -> focus w >> Flex.mouseResizeWindow w))                   --Set the window to floating mode and resize by dragging
	, ((modMask, button4), (\_ -> prevWS))                                                --Switch to previous workspace
	, ((modMask, button5), (\_ -> nextWS))                                                --Switch to next workspace
	, (((modMask .|. shiftMask), button4), (\_ -> shiftToPrev))                           --Send client to previous workspace
	, (((modMask .|. shiftMask), button5), (\_ -> shiftToNext))                           --Send client to next workspace
	]


--------------------------------------------------------------------------------------------
-- DZEN UTILS                                                                             --
--------------------------------------------------------------------------------------------

-- Dzen flags
data DF = DF
	{ xPosDF       :: Int
	, yPosDF       :: Int
	, widthDF      :: Int
	, heightDF     :: Int
	, alignementDF :: String
	, fgColorDF    :: String
	, bgColorDF    :: String
	, fontDF       :: String
	, eventDF      :: String
	, extrasDF     :: String
	}

-- Dzen box pretty config
data BoxPP = BoxPP
	{ bgColorBPP   :: String
	, fgColorBPP   :: String
	, boxColorBPP  :: String
	, leftIconBPP  :: String
	, rightIconBPP :: String
	, boxHeightBPP :: Int
	}

-- Dzen clickable area config
data CA = CA
	{ leftClickCA   :: String
	, middleClickCA :: String
	, rightClickCA  :: String
	, wheelUpCA     :: String
	, wheelDownCA   :: String
	}

-- Create a dzen string with its flags
dzenFlagsToStr :: DF -> String
dzenFlagsToStr df =
	" -x '" ++ (show $ xPosDF df) ++
	"' -y '" ++ (show $ yPosDF df) ++
	"' -w '" ++ (show $ widthDF df) ++
	"' -h '" ++ (show $ heightDF df) ++
	"' -ta '" ++ alignementDF df ++
	"' -fg '" ++ fgColorDF df ++
	"' -bg '" ++ bgColorDF df ++
	"' -fn '" ++ fontDF df ++
	"' -e '" ++ eventDF df ++
	"' " ++ extrasDF df

-- Uses dzen format to draw a "box" arround a given text
dzenBoxStyle :: BoxPP -> String -> String
dzenBoxStyle bpp t =
	"^fg(" ++ (boxColorBPP bpp) ++
	")^i(" ++ (leftIconBPP bpp)  ++
	")^ib(1)^r(1920x" ++ (show $ boxHeightBPP bpp) ++
	")^p(-1920)^fg(" ++ (fgColorBPP bpp) ++
	")" ++ t ++
	"^fg(" ++ (boxColorBPP bpp) ++
	")^i(" ++ (rightIconBPP bpp) ++
	")^fg(" ++ (bgColorBPP bpp) ++
	")^r(1920x" ++ (show $ boxHeightBPP bpp) ++
	")^p(-1920)^fg()^ib(0)"

-- Uses dzen format to make dzen text clickable
dzenClickStyle :: CA -> String -> String
dzenClickStyle ca t = "^ca(1," ++ leftClickCA ca ++
	")^ca(2," ++ middleClickCA ca ++
	")^ca(3," ++ rightClickCA ca ++
	")^ca(4," ++ wheelUpCA ca ++
	")^ca(5," ++ wheelDownCA ca ++
	")" ++ t ++
	"^ca()^ca()^ca()^ca()^ca()"

-- Launch dzen through the system shell and return a Handle to its standard input
dzenSpawnPipe df = spawnPipe $ "dzen2" ++ dzenFlagsToStr df

-- Logger version of dzenBoxStyle
dzenBoxStyleL :: BoxPP -> Logger -> Logger
dzenBoxStyleL bpp l = (fmap . fmap) (dzenBoxStyle bpp) l

-- Logger version of dzenClickStyle
dzenClickStyleL :: CA -> Logger -> Logger
dzenClickStyleL ca l = (fmap . fmap) (dzenClickStyle ca) l


--------------------------------------------------------------------------------------------
-- HARDCODED LOGGERS (you may need to amend them so that they work on your computer)      --
--------------------------------------------------------------------------------------------

-- Concat two Loggers
(++!) :: Logger -> Logger -> Logger
l1 ++! l2 = (liftA2 . liftA2) (++) l1 l2

-- Label
labelL :: String -> Logger
labelL = return . return

-- Init version for Logger
initL :: Logger -> Logger
initL = (fmap . fmap) initNotNull

-- Concat a list of loggers
concatL :: [Logger] -> Logger
concatL [] = return $ return ""
concatL (x:xs) = x ++! concatL xs

-- Concat a list of loggers with spaces between them
concatWithSpaceL :: [Logger] -> Logger
concatWithSpaceL [] = return $ return ""
concatWithSpaceL (x:xs) = x ++! (labelL " ") ++! concatWithSpaceL xs

initNotNull :: String -> String
initNotNull [] = "0\n"
initNotNull xs = init xs

tailNotNull :: [String] -> [String]
tailNotNull [] = ["0\n"]
tailNotNull xs = tail xs

-- Convert the content of a file into a Logger
fileToLogger :: (String -> String) -> String -> FilePath -> Logger
fileToLogger f e p = do
	let readWithE f1 e1 p1 = E.catch (do
		contents <- readFile p1
		return $ f1 (initNotNull contents) ) ((\_ -> return e1) :: E.SomeException -> IO String)
	str <- liftIO $ readWithE f e p
	return $ return str

-- Battery percent
batPercent :: Int -> String -> Logger
batPercent v c = fileToLogger format "N/A" "/sys/class/power_supply/BAT0/capacity" where
	format x = if ((read x::Int) <= v) then "^fg(" ++ c ++ ")" ++ x ++ "%^fg()" else (x ++ "%")

-- Battery status
batStatus :: Logger
batStatus = fileToLogger (\x -> x) "AC Conection" "/sys/class/power_supply/BAT0/status"

-- Brightness percenn
brightPerc :: Int -> Logger
brightPerc p = fileToLogger format "0" "/sys/class/backlight/acpi_video0/actual_brightness" where
	format x = (show $ div ((read x::Int) * 100) p) ++ "%"

-- wifi signal
wifiSignal :: Logger
wifiSignal = fileToLogger format "N/A" "/proc/net/wireless" where
	format x = if (length $ lines x) >= 3 then (initNotNull ((words ((lines x) !! 2)) !! 2) ++ "%") else "Off"

-- CPU temperature
cpuTemp :: Int -> Int -> String -> Logger
cpuTemp n v c = initL $ concatWithSpaceL $ map (fileToLogger divc "0") pathtemps where
	pathtemps = map (++"/thermal_zone/temp") $ map ("/sys/bus/acpi/devices/LNXTHERM:0"++) $ take n $ map show [0..]
	divc x = crit $ div (read x::Int) 1000
	crit x = if (x >= v) then "^fg(" ++ c ++ ")" ++ show x ++ "°^fg()" else (show x ++ "°")

-- Memory usage
memUsage :: [(String -> String)] -> Logger
memUsage xs = initL $ concatWithSpaceL $ map funct xs where
	funct x = fileToLogger x "N/A" "/proc/meminfo"

--_memUsed x = (_memValues x !! 0) - ((_memValues x !! 2) + (_memValues x !! 3) + (_memValues x !! 1)) --old format
_memUsed x = (_memValues x !! 0) - (_memValues x !! 2)  --new format
_memPerc x = div (_memUsed x * 100) (_memValues x !! 0)
_memValues x = map (getValues x) $ take 4 [0..] where
	getValues x n = read (words (lines x !! n) !! 1)::Int

freeBMemUsage x = (show $ _memValues x !! 1) ++ "B"
freeMBMemUsage x = (show $ div (_memValues x !! 1) 1024) ++ "MB"
totBMemUsage = (++"B") . show . _memUsed
totMBMemUsage = (++"MB") . show . (`div` 1024) . _memUsed
percMemUsage = (++"%") . show . _memPerc

-- CPU Usage: this is an ugly hack that depends on "haskell-cpu-usage" app (See my github repo to get the app)
cpuUsage :: String -> Int -> String -> Logger
cpuUsage path v c = fileToLogger format "0" path where
	format x = if (null x) then "N/A" else initNotNull $ concat $ map (++" ") $ map crit $ tailNotNull $ words $ x
	crit x = if ((read x::Int) >= v) then "^fg(" ++ c ++ ")" ++ x ++ "%^fg()" else (x ++ "%")

-- Uptime
uptime :: Logger
uptime = fileToLogger format "0" "/proc/uptime" where
	u x  = read (takeWhile (/='.') x)::Integer
	h x  = div (u x) 3600
	hr x = mod (u x) 3600
	m x  = div (hr x) 60
	s x  = mod (hr x) 60
	format x = (show $ h x) ++ "h " ++ (show $ m x) ++ "m " ++ (show $ s x) ++ "s"

-- Gets the current resolution given a display and a screen
getScreenRes :: String -> Int -> IO Res
getScreenRes d n = do
	dpy <- openDisplay d
	r <- liftIO $ getScreenInfo dpy
	closeDisplay dpy
	return $ Res
		{ xRes = fromIntegral $ rect_width $ r !! n
		, yRes = fromIntegral $ rect_height $ r !! n
		}

-- Screen Resolution
data Res = Res
	{ xRes :: Int
	, yRes :: Int
	}

-- Resolution logger
screenRes :: String -> Int -> Logger
screenRes d n = do
	res <- liftIO $ getScreenRes d n
	return $ return $ (show $ xRes res) ++ "x" ++ (show $ yRes res)
