--------------------------------------------------------------------------------------------
-- File        : ~/.xmonad/DzenLoggers.hs                                                 --
-- Author      : Nnoell <nnoell3[at]gmail.com>                                            --
-- Module      : DzenLoggers                                                              --
-- Stability   : Unstable                                                                 --
-- Portability : Unportable                                                               --
-- Desc        : Non-official module with custom loggers for xmonad                       --
-- TODO        : Need more abstraction                                                    --
--------------------------------------------------------------------------------------------

module DzenLoggers
	( DF(..), BoxPP(..), CA(..), Res(..)
	, dzenFlagsToStr, dzenBoxStyle, dzenClickStyle, dzenBoxStyleL, dzenClickStyleL
	, (++!)
	, labelL
	, initL
	, concatL
	, concatWithSpaceL
	, fileToLogger
	, batPercent
	, batStatus
	, brightPerc
	, wifiSignal
	, cpuTemp
	, memUsage, freeBMemUsage, freeMBMemUsage, totBMemUsage, totMBMemUsage, percMemUsage
	, cpuUsage
	, uptime
	, fsPerc
	, getScreenRes, screenRes
	) where


import XMonad
import Control.Applicative
import StatFS
import Graphics.X11.Xinerama
import Control.Exception as E


-- Logger is just a convenient synonym for @X (Maybe String)@.
type Logger = X (Maybe String)

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

-- Screen Resolution
data Res = Res
	{ xRes :: Int
	, yRes :: Int
	}


-- Create a dzen string with its flags
dzenFlagsToStr :: DF -> String
dzenFlagsToStr df = " -x '" ++ (show $ xPosDF df) ++
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
dzenBoxStyle bpp t = "^fg(" ++ (boxColorBPP bpp) ++
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

-- Logger version of dzenBoxStyle
dzenBoxStyleL :: BoxPP -> Logger -> Logger
dzenBoxStyleL bpp l = (fmap . fmap) (dzenBoxStyle bpp) l

-- Logger version of dzenClickStyle
dzenClickStyleL :: CA -> Logger -> Logger
dzenClickStyleL ca l = (fmap . fmap) (dzenClickStyle ca) l

-- Concat two Loggers
(++!) :: Logger -> Logger -> Logger
l1 ++! l2 = (liftA2 . liftA2) (++) l1 l2

-- Label
labelL :: String -> Logger
labelL = return . return

-- Init version for Logger
initL :: Logger -> Logger
initL = (fmap . fmap) initNoNull

-- Concat a list of loggers
concatL :: [Logger] -> Logger
concatL [] = return $ return ""
concatL (x:xs) = x ++! concatL xs

-- Concat a list of loggers with spaces between them
concatWithSpaceL :: [Logger] -> Logger
concatWithSpaceL [] = return $ return ""
concatWithSpaceL (x:xs) = x ++! (labelL " ") ++! concatWithSpaceL xs

initNoNull :: [Char] -> [Char]
initNoNull [] = "\n"
initNoNull xs = init xs

-- Convert the content of a file into a Logger
fileToLogger :: (String -> String) -> String -> FilePath -> Logger
fileToLogger f e p = do
	let readWithE f1 e1 p1 = E.catch (do
		contents <- readFile p1
		return $ f1 (initNoNull contents) ) ((\_ -> return e1) :: E.SomeException -> IO String)
	str <- liftIO $ readWithE f e p
	return $ return str

batPercent :: Int -> String -> Logger
batPercent v c = fileToLogger format "N/A" "/sys/class/power_supply/BAT0/capacity" where
	format x = if ((read x::Int) <= v) then "^fg(" ++ c ++ ")" ++ x ++ "%^fg()" else (x ++ "%")

batStatus :: Logger
batStatus = fileToLogger (\x -> x) "AC Conection" "/sys/class/power_supply/BAT0/status"

brightPerc :: Int -> Logger
brightPerc p = fileToLogger format "0" "/sys/class/backlight/acpi_video0/actual_brightness" where
	format x = (show $ div ((read x::Int) * 100) p) ++ "%"

wifiSignal :: Logger
wifiSignal = fileToLogger format "N/A" "/proc/net/wireless" where
	format x = if (length $ lines x) >= 3 then (initNoNull ((words ((lines x) !! 2)) !! 2) ++ "%") else "Off"

cpuTemp :: Int -> Int -> String -> Logger
cpuTemp n v c = initL $ concatWithSpaceL $ map (fileToLogger divc "0") pathtemps where
	pathtemps = map (++"/thermal_zone/temp") $ map ("/sys/bus/acpi/devices/LNXTHERM:0"++) $ take n $ map show [0..]
	divc x = crit $ div (read x::Int) 1000
	crit x = if (x >= v) then "^fg(" ++ c ++ ")" ++ show x ++ "°^fg()" else (show x ++ "°")

memUsage :: [(String -> String)] -> Logger
memUsage xs = initL $ concatWithSpaceL $ map funct xs where
	funct x = fileToLogger x "N/A" "/proc/meminfo"

_memUsed x = (_memValues x !! 0) - ((_memValues x !! 2) + (_memValues x !! 3) + (_memValues x !! 1))
_memPerc x = div (_memUsed x * 100) (_memValues x !! 0)
_memValues x = map (getValues x) $ take 4 [0..] where
	getValues x n = read (words (lines x !! n) !! 1)::Int

freeBMemUsage x = (show $ _memValues x !! 1) ++ "B"
freeMBMemUsage x = (show $ div (_memValues x !! 1) 1024) ++ "MB"
totBMemUsage = (++"B") . show . _memUsed
totMBMemUsage = (++"MB") . show . (`div` 1024) . _memUsed
percMemUsage = (++"%") . show . _memPerc

-- CPU Usage Logger: this is an ugly hack that depends on "haskell-cpu-usage" app (See my github repo to get the app)
cpuUsage :: String -> Int -> String -> Logger
cpuUsage path v c = fileToLogger format "0" path where
	format x = if (null x) then "N/A" else initNoNull $ concat $ map (++" ") $ map crit $ tail $ words $ x
	crit x = if ((read x::Int) >= v) then "^fg(" ++ c ++ ")" ++ x ++ "%^fg()" else (x ++ "%")

uptime :: Logger
uptime = fileToLogger format "0" "/proc/uptime" where
	u x  = read (takeWhile (/='.') x)::Integer
	h x  = div (u x) 3600
	hr x = mod (u x) 3600
	m x  = div (hr x) 60
	s x  = mod (hr x) 60
	format x = (show $ h x) ++ "h " ++ (show $ m x) ++ "m " ++ (show $ s x) ++ "s"

fsPerc :: String -> Logger
fsPerc str = do
	fs <- liftIO $ getFileSystemStats str
	let text = do
		fss <- fs
		let strfs = show $ div ((fsStatBytesUsed fss) * 100) (fsStatByteCount fss)
		return $ strfs ++ "%"
	return text

-- Gets the current resolution of screen n
getScreenRes :: String -> Int -> IO Res
getScreenRes d n = do
	dpy <- openDisplay d
	r <- liftIO $ getScreenInfo dpy
	return $ Res
		{ xRes = fromIntegral $ rect_width $ r !! n
		, yRes = fromIntegral $ rect_height $ r !! n
		}

screenRes :: String -> Int -> Logger
screenRes d n = do
	res <- liftIO $ getScreenRes d n
	return $ return $ (show $ xRes res) ++ "x" ++ (show $ yRes res)
