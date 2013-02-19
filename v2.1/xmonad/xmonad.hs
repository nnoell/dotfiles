--------------------------------------------------------------------------------------------
-- ~/.xmonad/xmonad.hs                                                                    --
-- validate syntax: xmonad --recompile                                                    --
--------------------------------------------------------------------------------------------

-- Language
{-# LANGUAGE DeriveDataTypeable, NoMonomorphismRestriction, TypeSynonymInstances, MultiParamTypeClasses,  ImplicitParams, PatternGuards #-}

-- Imported libraries
import XMonad
import XMonad.Core
import XMonad.Layout
import XMonad.Layout.IM
import XMonad.Layout.Gaps
import XMonad.Layout.Named
import XMonad.Layout.Tabbed
import XMonad.Layout.OneBig
import XMonad.Layout.Master
import XMonad.Layout.Reflect
import XMonad.Layout.MosaicAlt
import XMonad.Layout.NoFrillsDecoration
import XMonad.Layout.SimplestFloat
import XMonad.Layout.NoBorders (noBorders,smartBorders,withBorder)
import XMonad.Layout.ResizableTile
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.PerWorkspace (onWorkspace)
import XMonad.Layout.Minimize
import XMonad.Layout.Maximize
import XMonad.Layout.WindowNavigation
import XMonad.StackSet (RationalRect (..), currentTag)
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
import XMonad.Util.Cursor
import XMonad.Util.EZConfig
import XMonad.Util.Run (spawnPipe)
import XMonad.Util.Scratchpad
import XMonad.Util.NamedScratchpad
import XMonad.Actions.CycleWS
import XMonad.Actions.GridSelect
import XMonad.Actions.MouseResize
import Data.IORef
import Data.Monoid
import Data.List
import Graphics.X11.ExtraTypes.XF86
import System.Exit
import System.IO (Handle, hPutStrLn)
import qualified XMonad.StackSet as W
import qualified Data.Map as M
import qualified XMonad.Actions.FlexibleResize as Flex

-- Main
main :: IO ()
main = do
	workspaceBar            <- spawnPipe myWorkspaceBar
	bottomStatusBar         <- spawnPipe myBottomStatusBar
	topStatusBar            <- spawnPipe myTopStatusBar
	focusFollow             <- newIORef True; let ?focusFollow = focusFollow
	xmonad $ myUrgencyHook $ defaultConfig
		{ terminal           = "urxvtc"
		, modMask            = mod4Mask
		, focusFollowsMouse  = False
		, borderWidth        = 1
		, normalBorderColor  = myNormalBorderColor
		, focusedBorderColor = myFocusedBorderColor
		, layoutHook         = myLayoutHook
		, workspaces         = myWorkspaces
		, manageHook         = myManageHook <+> manageScratchPad <+> manageDocks <+> dynamicMasterHook
		, logHook            = myLogHook workspaceBar <+> ewmhDesktopsLogHook >> setWMName "LG3D"
		, handleEventHook    = myHandleEventHook
		, keys               = myKeys
		, mouseBindings      = myMouseBindings
		, startupHook        = setDefaultCursor xC_left_ptr >> setWMName "LG3D"
		}
		`additionalKeysP`
		[ ("<XF86TouchpadToggle>", spawn "sh /home/nnoell/bin/touchpadtoggle.sh") --because xF86XK_TouchpadToggle doesnt exist :(
		, ("M-v", io $ modifyIORef ?focusFollow not)                              --Toggle focus follow moouse
		]


--------------------------------------------------------------------------------------------
-- APPEARANCE CONFIG                                                                      --
--------------------------------------------------------------------------------------------

-- Colors, fonts and paths
dzenFont             = "-*-montecarlo-medium-r-normal-*-11-*-*-*-*-*-*-*"
colorBlack           = "#020202" --Background (Dzen_BG)
colorBlackAlt        = "#1c1c1c" --Black Xdefaults
colorGray            = "#444444" --Gray       (Dzen_FG2)
colorGrayAlt         = "#101010" --Gray dark
colorWhite           = "#a9a6af" --Foreground (Shell_FG)
colorWhiteAlt        = "#9d9d9d" --White dark (Dzen_FG)
colorMagenta         = "#8e82a2"
colorBlue            = "#44aacc"
colorBlueAlt         = "#3955c4"
colorRed             = "#e0105f"
colorGreen           = "#66ff66"
colorGreenAlt        = "#558965"
myNormalBorderColor  = colorBlackAlt
myFocusedBorderColor = colorGray
myIconPath           = "/home/nnoell/.icons/xbm_icons/subtle/"
xRes                 = 1366
yRes                 = 768
panelHeight          = 16
panelBoxHeight       = 12

-- Title theme
myTitleTheme :: Theme
myTitleTheme = defaultTheme
	{ fontName            = dzenFont
	, inactiveBorderColor = colorBlackAlt
	, inactiveColor       = colorBlack
	, inactiveTextColor   = colorGray
	, activeBorderColor   = colorGray
	, activeColor         = colorBlackAlt
	, activeTextColor     = colorWhiteAlt
	, urgentBorderColor   = colorGray
	, urgentTextColor     = colorGreen
	, decoHeight          = 14
	}

-- Prompt theme
myXPConfig :: XPConfig
myXPConfig = defaultXPConfig
	{ font                = dzenFont
	, bgColor             = colorBlack
	, fgColor             = colorWhite
	, bgHLight            = colorBlue
	, fgHLight            = colorBlack
	, borderColor         = colorGrayAlt
	, promptBorderWidth   = 1
	, height              = 16
	, position            = Top
	, historySize         = 100
	, historyFilter       = deleteConsecutive
	, autoComplete        = Nothing
	}

-- GridSelect color scheme
myColorizer :: Window -> Bool -> X (String, String)
myColorizer = colorRangeFromClassName
	(0x00,0x00,0x00) -- lowest inactive bg
	(0x1C,0x1C,0x1C) -- highest inactive bg
	(0x44,0xAA,0xCC) -- active bg
	(0xBB,0xBB,0xBB) -- inactive fg
	(0x00,0x00,0x00) -- active fg

-- GridSelect theme
myGSConfig :: t -> GSConfig Window
myGSConfig colorizer = (buildDefaultGSConfig myColorizer)
	{ gs_cellheight  = 50
	, gs_cellwidth   = 200
	, gs_cellpadding = 10
	, gs_font        = dzenFont
	}

-- Workspaces
myWorkspaces :: [WorkspaceId]
myWorkspaces = ["TERM", "WEBS", "CODE", "GRFX", "CHAT", "ALT1", "ALT2", "ALT3"]


--------------------------------------------------------------------------------------------
-- LAYOUT CONFIG                                                                          --
--------------------------------------------------------------------------------------------

-- Layouts (name must be diferent of Minimize, Maximize and Mirror)
myTile = named "ReTall" $ smartBorders $ ResizableTall 1 0.03 0.5 []
myMirr = named "MiTall" $ smartBorders $ Mirror myTile
myMosA = named "Mosaic" $ smartBorders $ MosaicAlt M.empty
myObig = named "OneBig" $ smartBorders $ OneBig 0.75 0.65
myTabs = named "Tabbed" $ smartBorders $ tabbed shrinkText myTitleTheme
myFull = named "Full"   $ smartBorders $ tabbedAlways shrinkText myTitleTheme
myTabM = named "Master" $ smartBorders $ mastered 0.01 0.4 $ tabbed shrinkText myTitleTheme
myFlat = named "Float"  $ mouseResize $ noFrillsDeco shrinkText myTitleTheme simplestFloat
myGimp = named "Gimp"   $ withIM (0.15) (Role "gimp-toolbox") $ reflectHoriz $ withIM (0.20) (Role "gimp-dock") myMosA
myChat = named "Pidgin" $ withIM (0.20) (Title "Buddy List") $ Mirror $ ResizableTall 1 0.03 0.5 []

-- Tabbed transformer (W+f)
data TABBED = TABBED deriving (Read, Show, Eq, Typeable)
instance Transformer TABBED Window where
	transform TABBED x k = k myFull (\_ -> x)

-- Floated transformer (W+ctl+f)
data FLOATED = FLOATED deriving (Read, Show, Eq, Typeable)
instance Transformer FLOATED Window where
	transform FLOATED x k = k myFlat (\_ -> x)

-- Layout hook
myLayoutHook = gaps [(U,16), (D,16), (L,0), (R,0)]
	$ avoidStruts
	$ windowNavigation
	$ minimize
	$ maximize
	$ mkToggle (single TABBED)
	$ mkToggle (single FLOATED)
	$ mkToggle (single MIRROR)
	$ mkToggle (single REFLECTX)
	$ mkToggle (single REFLECTY)
	$ onWorkspace (myWorkspaces !! 1) webLayouts  --Workspace 1 layouts
	$ onWorkspace (myWorkspaces !! 2) codeLayouts --Workspace 2 layouts
	$ onWorkspace (myWorkspaces !! 3) gimpLayouts --Workspace 3 layouts
	$ onWorkspace (myWorkspaces !! 4) chatLayouts --Workspace 4 layouts
	$ allLayouts
	where
		allLayouts  = myTile ||| myObig ||| myMirr ||| myMosA ||| myTabM
		webLayouts  = myTabs ||| myTabM
		codeLayouts = myTabM ||| myTile
		gimpLayouts = myGimp
		chatLayouts = myChat


--------------------------------------------------------------------------------------------
-- HANDLE EVENT HOOK CONFIG                                                               --
--------------------------------------------------------------------------------------------

-- Handle event hook
myHandleEventHook :: (?focusFollow::IORef Bool) => Event -> X All
myHandleEventHook = fullscreenEventHook <+> docksEventHook <+> toggleFocus --thanks to vogt
	where
		toggleFocus e = case e of
				CrossingEvent {ev_window=w, ev_event_type=t}
					| t == enterNotify, ev_mode e == notifyNormal -> do
						whenX (io $ readIORef ?focusFollow) (focus w)
						return $ All True
				_ -> return $ All True


--------------------------------------------------------------------------------------------
-- MANAGE HOOK CONFIG                                                                     --
--------------------------------------------------------------------------------------------

-- Scratchpad (W+º)
manageScratchPad :: ManageHook
manageScratchPad = scratchpadManageHook (W.RationalRect (0) (1/50) (1) (3/4))
scratchPad = scratchpadSpawnActionCustom "urxvtc -name scratchpad"

-- Manage hook
myManageHook :: ManageHook
myManageHook = composeAll . concat $
	[ [resource     =? r     --> doIgnore                             | r <- myIgnores] --ignore desktop
	, [className    =? c     --> doShift (myWorkspaces !! 1)          | c <- myWebS   ] --move myWebS windows to workspace 1 by classname
	, [className    =? c     --> doShift (myWorkspaces !! 2)          | c <- myCodeS  ] --move myCodeS windows to workspace 2 by classname
	, [className    =? c     --> doShift (myWorkspaces !! 4)          | c <- myChatS  ] --move myChatS windows to workspace 4 by classname
	, [className    =? c     --> doShift (myWorkspaces !! 3)          | c <- myGfxS   ] --move myGfxS windows to workspace 4 by classname
	, [className    =? c     --> doShiftAndGo (myWorkspaces !! 5)     | c <- myAlt1S  ] --move myGameS windows to workspace 5 by classname and shift
	, [className    =? c     --> doShift (myWorkspaces !! 7)          | c <- myAlt3S  ] --move myOtherS windows to workspace 5 by classname and shift
	, [className    =? c     --> doCenterFloat                        | c <- myFloatCC] --float center geometry by classname
	, [name         =? n     --> doCenterFloat                        | n <- myFloatCN] --float center geometry by name
	, [name         =? n     --> doSideFloat NW                       | n <- myFloatSN] --float side NW geometry by name
	, [className    =? c     --> doF W.focusDown                      | c <- myFocusDC] --dont focus on launching by classname
	, [isFullscreen          --> doFullFloat]
	]
	where
		doShiftAndGo ws = doF (W.greedyView ws) <+> doShift ws
		role            = stringProperty "WM_WINDOW_ROLE"
		name            = stringProperty "WM_NAME"
		myIgnores       = ["desktop","desktop_window"]
		myWebS          = ["Chromium","Firefox", "Opera"]
		myCodeS         = ["NetBeans IDE 7.2"]
		myGfxS          = ["Gimp", "gimp", "GIMP"]
		myChatS         = ["Pidgin", "Xchat"]
		myAlt1S         = ["zsnes"]
		myAlt3S         = ["Amule", "Transmission-gtk"]
		myFloatCC       = ["MPlayer", "File-roller", "zsnes", "Gcalctool", "Exo-helper-1", "Gksu", "PSX", "Galculator", "Nvidia-settings", "XFontSel"
						  , "XCalc", "XClock", "Desmume", "Ossxmix", "Xvidcap", "Main", "Wicd-client.py", "com-mathworks-util-PostVMInit", "MATLAB"]
		myFloatCN       = ["ePSXe - Enhanced PSX emulator", "Seleccione Archivo", "Config Video", "Testing plugin", "Config Sound", "Config Cdrom", "Config Bios"
						  , "Config Netplay", "Config Memcards", "About ePSXe", "Config Controller", "Config Gamepads", "Select one or more files to open"
						  , "Add media", "Choose a file", "Open Image", "File Operation Progress", "Firefox Preferences", "Preferences", "Search Engines"
						  , "Set up sync", "Passwords and Exceptions", "Autofill Options", "Rename File", "Copying files", "Moving files", "File Properties", "Replace", ""]
		myFloatSN       = ["Event Tester"]
		myFocusDC       = ["Event Tester", "Notify-osd"]


--------------------------------------------------------------------------------------------
-- STATUS BARS CONFIG                                                                     --
--------------------------------------------------------------------------------------------

-- UrgencyHook
myUrgencyHook = withUrgencyHook dzenUrgencyHook
	{ args = ["-fn", dzenFont, "-bg", colorBlack, "-fg", colorGreen] }

-- StatusBars
myWorkspaceBar, myBottomStatusBar, myTopStatusBar :: String
myWorkspaceBar    = "dzen2 -x '0' -y " ++ show (yRes - panelHeight) ++ " -h " ++ show panelHeight ++ " -w '946' -ta 'l' -fg '" ++ colorWhiteAlt ++ "' -bg '" ++
                    colorBlack ++ "' -fn '" ++ dzenFont ++ "' -p -e 'onstart=lower'"
myBottomStatusBar = "/home/nnoell/bin/bottomstatusbar.sh " ++ show xRes ++ " " ++ show yRes ++ " " ++ show panelBoxHeight ++ " " ++ show panelHeight ++ " 946"
myTopStatusBar    = "/home/nnoell/bin/topstatusbar.sh " ++ show xRes ++ " " ++ show yRes ++ " " ++ show panelBoxHeight ++ " " ++ show panelHeight ++ " 910"

-- myWorkspaceBar config
myLogHook :: Handle -> X ()
myLogHook h = dynamicLogWithPP $ defaultPP
	{ ppOutput          = hPutStrLn h
	, ppSort            = fmap (namedScratchpadFilterOutWorkspace .) (ppSort defaultPP) --hide "NSP" from workspace list
	, ppOrder           = orderText
	, ppExtras          = []
	, ppSep             = ""
	, ppWsSep           = ""
	, ppCurrent         = wrapTextBox colorBlack    colorBlue    colorBlack
	, ppUrgent          = wrapTextBox colorBlack    colorGreen   colorBlack . wrapClickWorkspace
	, ppVisible         = wrapTextBox colorBlack    colorGrayAlt colorBlack . wrapClickWorkspace
	, ppHiddenNoWindows = wrapTextBox colorBlack    colorGrayAlt colorBlack . wrapClickWorkspace
	, ppHidden          = wrapTextBox colorWhiteAlt colorGrayAlt colorBlack . wrapClickWorkspace
	, ppLayout          = wrapTextBox colorGray     colorGrayAlt colorBlack . wrapClickLayout . (++) "LAYOUT " . layoutText . removeWord . removeWord
	, ppTitle           = wrapTextBox colorGray     colorGrayAlt colorBlack . wrapClickTitle  . (++) "TITLE "  . titleText
	}
	where
		--display config
		orderText (ws:l:t:_) = [ws,l,t]
		titleText [] = "^fg(" ++ colorWhiteAlt ++ ")Desktop"
		titleText xs = "^fg(" ++ colorWhiteAlt ++ ")" ++ (shorten 85 xs)
		removeWord = tail . dropWhile (/= ' ')
		layoutText xs
			| isPrefixOf "Mirror" xs   = layoutText $ removeWord xs ++ " [M]"
			| isPrefixOf "ReflectY" xs = layoutText $ removeWord xs ++ " [Y]"
			| isPrefixOf "ReflectX" xs = layoutText $ removeWord xs ++ " [X]"
			| isPrefixOf "Float" xs    = "^fg(" ++ colorRed ++ ")" ++ xs
			| isPrefixOf "Full" xs     = "^fg(" ++ colorGreen ++ ")" ++ xs
			| otherwise                = "^fg(" ++ colorBlue ++ ")" ++ xs
		--box config
		wrapTextBox fg bg1 bg2 t = "^fg(" ++ bg1 ++ ")^i(" ++ myIconPath  ++ "boxleft.xbm)^ib(1)^r(" ++ show xRes ++ "x" ++ show panelBoxHeight ++ ")^p(-" ++ show xRes ++ ")^fg(" ++ fg ++ ")" ++ t
                                   ++ "^fg(" ++ bg1 ++ ")^i(" ++ myIconPath ++ "boxright.xbm)^fg(" ++ bg2  ++ ")^r(" ++ show xRes ++ "x" ++ show panelBoxHeight ++ ")^p(-" ++ show xRes ++ ")^fg()^ib(0)"
		--clickable config
		wrapClickLayout l = "^ca(1,xdotool key super+space)^ca(3,xdotool key super+shift+space)" ++ l ++ "^ca()^ca()"                       --clickable layout
		wrapClickTitle t = "^ca(1,xdotool key super+m)^ca(2,xdotool key super+c)^ca(3,xdotool key super+shift+m)" ++ t ++ "^ca()^ca()^ca()" --clickable title
		wrapClickWorkspace ws = "^ca(1," ++ xdo "w;" ++ xdo index ++ ")" ++ "^ca(3," ++ xdo "w;" ++ xdo index ++ ")" ++ ws ++ "^ca()^ca()"  --clickable workspaces
			where
				wsIdxToString Nothing = "1"
				wsIdxToString (Just n) = show (n+1)
				index = wsIdxToString (elemIndex ws myWorkspaces)
				xdo key = "xdotool key super+" ++ key


--------------------------------------------------------------------------------------------
-- BINDINGS CONFIG                                                                        --
--------------------------------------------------------------------------------------------

-- Key bindings
myKeys :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
myKeys conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $
	--Xmonad bindings
	[((modMask .|. shiftMask, xK_q), io (exitWith ExitSuccess))                                 --Quit xmonad
	, ((modMask, xK_q), restart "xmonad" True)                                                  --Restart xmonad
	, ((mod1Mask, xK_F2), shellPrompt myXPConfig)                                               --Launch Xmonad shell prompt
	, ((modMask, xK_F2), xmonadPrompt myXPConfig)                                               --Launch Xmonad prompt
	, ((mod1Mask, xK_F3), manPrompt myXPConfig)                                                 --Launch man prompt
	, ((modMask, xK_g), goToSelected $ myGSConfig myColorizer)                                  --Launch GridSelect
	, ((modMask, xK_masculine), scratchPad)                                                     --Scratchpad
	, ((modMask .|. shiftMask, xK_Return), spawn $ XMonad.terminal conf)                        --Launch default terminal
	--Window management bindings
	, ((modMask, xK_c), kill)                                                                   --Close focused window
	, ((mod1Mask, xK_F4), kill)
	, ((modMask, xK_n), refresh)                                                                --Resize viewed windows to the correct size
	, ((modMask, xK_Tab), windows W.focusDown)                                                  --Move focus to the next window
	, ((modMask, xK_j), windows W.focusDown)
	, ((mod1Mask, xK_Tab), windows W.focusDown)
	, ((modMask, xK_k), windows W.focusUp)                                                      --Move focus to the previous window
	, ((modMask, xK_a), windows W.focusMaster)                                                  --Move focus to the master window
	, ((modMask .|. shiftMask, xK_a), windows W.swapMaster)                                     --Swap the focused window and the master window
	, ((modMask .|. shiftMask, xK_j), windows W.swapDown)                                       --Swap the focused window with the next window
	, ((modMask .|. shiftMask, xK_k), windows W.swapUp)                                         --Swap the focused window with the previous window
	, ((modMask, xK_h), sendMessage Shrink)                                                     --Shrink the master area
	, ((modMask .|. shiftMask, xK_Left), sendMessage Shrink)
	, ((modMask, xK_l), sendMessage Expand)                                                     --Expand the master area
	, ((modMask .|. shiftMask, xK_Right), sendMessage Expand)
	, ((modMask .|. shiftMask, xK_h), sendMessage MirrorShrink)                                 --MirrorShrink the master area
	, ((modMask .|. shiftMask, xK_Down), sendMessage MirrorShrink)
	, ((modMask .|. shiftMask, xK_l), sendMessage MirrorExpand)                                 --MirrorExpand the master area
	, ((modMask .|. shiftMask, xK_Up), sendMessage MirrorExpand)
	, ((modMask, xK_t), withFocused $ windows . W.sink)                                         --Push window back into tiling
	, ((modMask .|. shiftMask, xK_t), rectFloatFocused)                                         --Push window into float
	, ((modMask, xK_m), withFocused minimizeWindow)                                             --Minimize window
	, ((modMask, xK_b), withFocused (sendMessage . maximizeRestore))                            --Maximize window
	, ((modMask .|. shiftMask, xK_m), sendMessage RestoreNextMinimizedWin)                      --Restore window
	, ((modMask .|. shiftMask, xK_f), fullFloatFocused)                                         --Push window into full screen
	, ((modMask, xK_comma), sendMessage (IncMasterN 1))                                         --Increment the number of windows in the master area
	, ((modMask, xK_period), sendMessage (IncMasterN (-1)))                                     --Deincrement the number of windows in the master area
	, ((modMask, xK_Right), sendMessage $ Go R)                                                 --Change focus to right
	, ((modMask, xK_Left ), sendMessage $ Go L)                                                 --Change focus to left
	, ((modMask, xK_Up   ), sendMessage $ Go U)                                                 --Change focus to up
	, ((modMask, xK_Down ), sendMessage $ Go D)                                                 --Change focus to down
	, ((modMask .|. controlMask, xK_Right), sendMessage $ Swap R)                               --Swap focused window to right
	, ((modMask .|. controlMask, xK_Left ), sendMessage $ Swap L)                               --Swap focused window to left
	, ((modMask .|. controlMask, xK_Up   ), sendMessage $ Swap U)                               --Swap focused window to up
	, ((modMask .|. controlMask, xK_Down ), sendMessage $ Swap D)                               --Swap focused window to down
	--Layout management bindings
	, ((modMask, xK_space), sendMessage NextLayout)                                             --Rotate through the available layout algorithms
	, ((modMask .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf)                  --Reset the layout on the current workspace to default
	, ((modMask, xK_f), sendMessage $ XMonad.Layout.MultiToggle.Toggle TABBED)                  --Push layout into tabbed
	, ((modMask .|. controlMask, xK_f), sendMessage $ XMonad.Layout.MultiToggle.Toggle FLOATED) --Push layout into float
	, ((modMask .|. shiftMask, xK_z), sendMessage $ Toggle MIRROR)                              --Push layout into mirror
	, ((modMask .|. shiftMask, xK_x), sendMessage $ XMonad.Layout.MultiToggle.Toggle REFLECTX)  --Reflect layout by X
	, ((modMask .|. shiftMask, xK_y), sendMessage $ XMonad.Layout.MultiToggle.Toggle REFLECTY)  --Reflect layout by Y
	--Gaps management bindings
	, ((modMask .|. controlMask, xK_t), sendMessage $ ToggleGaps)                               --toogle all gaps
	, ((modMask .|. controlMask, xK_u), sendMessage $ ToggleGap U)                              --toogle the top gap
	, ((modMask .|. controlMask, xK_d), sendMessage $ ToggleGap D)                              --toogle the bottom gap
	--Scripts management bindings
	, ((modMask , xK_x), spawn "xcalib -invert -alter")											--Invert colors in X
	, ((modMask , xK_d), spawn "killall dzen2")                                                 --Kill dzen2
	, ((0, xF86XK_AudioMute), spawn "sh /home/nnoell/bin/voldzen.sh t -d")                      --Mute/unmute volume
	, ((0, xF86XK_AudioRaiseVolume), spawn "sh /home/nnoell/bin/voldzen.sh + -d")               --Raise volume
	, ((mod1Mask, xK_Up), spawn "sh /home/nnoell/bin/voldzen.sh + -d")
	, ((0, xF86XK_AudioLowerVolume), spawn "sh /home/nnoell/bin/voldzen.sh - -d")               --Lower volume
	, ((mod1Mask, xK_Down), spawn "sh /home/nnoell/bin/voldzen.sh - -d")
	, ((0, xF86XK_AudioNext), spawn "ncmpcpp next")                                             --Next song
	, ((mod1Mask, xK_Right), spawn "ncmpcpp next")
	, ((0, xF86XK_AudioPrev), spawn "ncmpcpp prev")                                             --Prev song
	, ((mod1Mask, xK_Left), spawn "ncmpcpp prev")
	, ((0, xF86XK_AudioPlay), spawn "ncmpcpp toggle")                                           --Toggle song
	, ((mod1Mask .|. controlMask, xK_Down), spawn "ncmpcpp toggle")
	, ((0, xF86XK_AudioStop), spawn "ncmpcpp stop")                                             --Stop song
	, ((mod1Mask .|. controlMask, xK_Up), spawn "ncmpcpp stop")
	, ((0, xF86XK_MonBrightnessUp), spawn "sh /home/nnoell/bin/bridzen.sh")                     --Raise brightness
	, ((0, xF86XK_MonBrightnessDown), spawn "sh /home/nnoell/bin/bridzen.sh")                   --Lower brightness
	, ((0, xF86XK_ScreenSaver), spawn "sh /home/nnoell/bin/turnoffscreen.sh")                   --Lock screen
	, ((0, xK_Print), spawn "scrot '%Y-%m-%d_$wx$h.png'")                                       --Take a screenshot
	, ((modMask , xK_s), spawn "sh /home/nnoell/bin/turnoffscreen.sh")                          --Turn off screen
	--Workspaces management bindings
	, ((mod1Mask, xK_comma), toggleWS)                                                          --Toggle to the workspace displayed previously
	, ((mod1Mask, xK_masculine), toggleOrView (myWorkspaces !! 0))                              --If ws != 0 then move to workspace 0, else move to latest ws I was
	, ((mod1Mask .|. controlMask, xK_Left),  prevWS)                                            --Move to previous Workspace
	, ((mod1Mask .|. controlMask, xK_Right), nextWS)                                            --Move to next Workspace
	, ((modMask .|. shiftMask, xK_n), shiftToNext)                                              --Send client to next workspace
	, ((modMask .|. shiftMask, xK_p), shiftToPrev)                                              --Send client to previous workspace
	]
	++
	[((m .|. modMask, k), windows $ f i)                                                        --Switch to n workspaces and send client to n workspaces
		| (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
		, (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
	++
	[((m .|. modMask, key), screenWorkspace sc >>= flip whenJust (windows . f))                 --Switch to n screens and send client to n screens
		| (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
		, (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]
	where
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
