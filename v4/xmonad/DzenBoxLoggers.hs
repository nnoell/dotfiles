--------------------------------------------------------------------------------------------
-- File        : ~/.xmonad/DzenBoxLoggers.hs                                              --
-- Author      : Nnoell <nnoell3[at]gmail.com>                                            --
-- Module      : DzenBoxLoggers                                                           --
-- Stability   : Unstable                                                                 --
-- Portability : Unportable                                                               --
-- Desc        : Non-official module with custom loggers for xmonad                       --
-- TODO        : Need more abstraction                                                    --
--------------------------------------------------------------------------------------------

module DzenBoxLoggers
	( DF(..), BoxPP(..), CA(..)
	, dzenFlagsToStr, dzenBoxStyle, dzenClickStyle, dzenBoxStyleL, dzenClickStyleL
	, (++!)
	, labelL
	, readWithE
	, batPercent
	, batStatus
	, brightPerc
	, wifiSignal
	, cpuTemp
	, fsPerc
	, memUsage
	, cpuUsage
	, uptime
	) where

import XMonad (liftIO)
import XMonad.Util.Loggers
import StatFS
import Control.Exception as E

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
dzenFlagsToStr df = "dzen2 -x '" ++ (show $ xPosDF df) ++
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
dzenBoxStyleL bpp l = do
	log <- l
	let text = do
		t <- log
		return $ dzenBoxStyle bpp t
	return text

-- Logger version of dzenClickStyle
dzenClickStyleL :: CA -> Logger -> Logger
dzenClickStyleL ca l = do
	log <- l
	let text = do
		t <- log
		return $ dzenClickStyle ca t
	return text

-- Concat two Loggers
(++!) :: Logger -> Logger -> Logger
l1 ++! l2 = do
	log1 <- l1
	log2 <- l2
	let text = do
		str1 <- log1
		str2 <- log2
		return $ str1 ++ str2
	return text

-- Label
labelL :: String -> Logger
labelL t = return $ return t

-- Returns the file content in a String. If it Doesnt exist, it will returns what you want
readWithE :: FilePath -> String -> String -> IO String
readWithE p e c = E.catch (do
	contents <- readFile p
	let check x = if (null x) then (e ++ "\n") else x --if file is empty return "\n"
	return $ (init $ check contents) ++ c ) ((\_ -> return e) :: E.SomeException -> IO String)

-- Battery Percent Logger
batPercent :: Logger
batPercent = do
	percent <- liftIO $ readWithE "/sys/class/power_supply/BAT0/capacity" "N/A" "%"
	return $ return percent

-- Battery Status Logger
batStatus :: Logger
batStatus = do
	status <- liftIO $ readWithE "/sys/class/power_supply/BAT0/status" "AC Conection" ""
	return $ return status

-- Brightness Logger
brightPerc :: Logger
brightPerc = do
	value <- liftIO $ readWithE "/sys/class/backlight/acpi_video0/actual_brightness" "0" ""
	return $ return $ (show $ div ((read value::Int) * 100) 15) ++ "%"

-- Wifi signal Logger
wifiSignal :: Logger
wifiSignal = do
	signalContent <- liftIO $ readWithE "/proc/net/wireless" "N/A" ""
	let signalLines = lines signalContent
	    signal = if (length signalLines) >= 3 then (init ((words (signalLines !! 2)) !! 2) ++ "%") else "Off"
	return $ return signal

-- CPU Temp Logger
cpuTemp :: Logger
cpuTemp = do
	temp1 <- liftIO $ readWithE "/sys/bus/platform/devices/coretemp.0/temp1_input" "0" ""
	temp2 <- liftIO $ readWithE "/sys/bus/platform/devices/coretemp.0/temp2_input" "0" ""
	temp3 <- liftIO $ readWithE "/sys/bus/platform/devices/coretemp.0/temp3_input" "0" ""
	let divc x = show $ div (read x::Int) 1000
	return $ return $ (divc temp1) ++ "° " ++ (divc temp2) ++ "° " ++ (divc temp3) ++ "°"

-- Filesystem percent Logger
fsPerc :: String -> Logger
fsPerc str = do
	fs <- liftIO $ getFileSystemStats str
	let text = do
		fss <- fs
		let strfs = show $ div ((fsStatBytesUsed fss) * 100) (fsStatByteCount fss)
		return $ strfs ++ "%"
	return text

-- Memory Usage Logger
memUsage :: Logger
memUsage = do
	memInfo <- liftIO $ readWithE "/proc/meminfo" "N/A" ""
	let memInfoLines = lines memInfo
	    memTotal = read (words (memInfoLines !! 0) !! 1)::Int
	    memFree  = read (words (memInfoLines !! 1) !! 1)::Int
	    buffers  = read (words (memInfoLines !! 2) !! 1)::Int
	    cached   = read (words (memInfoLines !! 3) !! 1)::Int
	    used     = memTotal - (buffers + cached + memFree)
	    perc     = div ((memTotal - (buffers + cached + memFree)) * 100) memTotal
	return $ return $ (show perc) ++ "% " ++ (show $ div used 1024) ++ "MB"


-- CPU Usage Logger: this is an ugly hack that depends on "haskell-cpu-usage" app (See my github repo to get the app)
cpuUsage :: String -> Logger
cpuUsage path = do
	cpus <- liftIO $ readWithE path "N/A" ""
	let str = concat $ map (++"% ") $ tail $ words cpus
	return $ return $ if (null str) then "" else init str

-- Uptime Logger
uptime :: Logger
uptime = do
	uptime <- liftIO $ readWithE "/proc/uptime" "0" ""
	let u  = read (takeWhile (/='.') uptime )::Integer
	    h  = div u 3600
	    hr = mod u 3600
	    m  = div hr 60
	    s  = mod hr 60
	return $ return $ (show h) ++ "h " ++ (show m) ++ "m " ++ (show s) ++ "s"
