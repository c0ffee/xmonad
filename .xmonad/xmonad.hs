
-- xmonad configuration file

import System.IO
import System.Exit

import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)
import XMonad.Util.EZConfig(additionalMouseBindings)
import Graphics.X11.ExtraTypes.XF86

import XMonad.Prompt
import XMonad.Prompt.Layout
import XMonad.Prompt.Shell
import XMonad.Prompt.Window
import XMonad.Prompt.XMonad

import XMonad.Actions.CycleWS

-- import XMonad.Layout.NoBorders
import XMonad.Layout.ThreeColumns

import XMonad.Hooks.SetWMName

import XMonad.Layout.Spacing
import XMonad.Layout.NoBorders (smartBorders, noBorders)
import XMonad.Layout.PerWorkspace (onWorkspace, onWorkspaces)
import XMonad.Layout.Reflect (reflectHoriz)
import XMonad.Layout.IM
import XMonad.Layout.SimpleFloat
import XMonad.Layout.Spacing
import XMonad.Layout.ResizableTile
import XMonad.Layout.LayoutHints
import XMonad.Layout.LayoutModifier
import XMonad.Layout.Grid
import XMonad.Layout.ThreeColumns

import qualified XMonad.StackSet as W
import qualified Data.Map        as M

button8 = 8 :: Button
button9 = 9 :: Button
button10 = 10 :: Button
button11 = 11 :: Button

-- Width of the window border in pixels.
--
myBorderWidth   = 0

-- Border colors for unfocused and focused windows, respectively.
--
myNormalBorderColor  = "#dddddd"
myFocusedBorderColor = "#ff0000"

myManageHook = composeAll [
		-- use xprop to find property, XMonad-ManageHook.html to find correspondic resource name
		className =? "Thunderbird"	--> doF (W.shift "3:mail"),
		className =? "Chromium-browser"	--> doF (W.shift "2:web"),
		className =? "Skype"	-->  doF (W.shift "4:chat"),
		className =? "Spotify"	--> doF (W.shift "6:media"),
		className =? "Googleearth-bin"	--> doFloat,
		className =? "Gimp"		--> doFloat,
		title =? "Set Zoom"		--> doFloat,
		title =? "Playlist"		--> doFloat,
		className =? "desktop_window"	--> doIgnore,
		className =? "kdesktop"		--> doIgnore
	]

myLayout = tiled ||| Mirror tiled ||| threetiled ||| Mirror threetiled ||| noBorders Full
  where
    -- The default number of windows in the master pane
    nmaster    = 1
    -- Percent of screen to increment by when resizing panes
    delta      = 1/100
    -- Default proportion of screen occupied by master pane
    ratio2     = 2/3
    ratio3     = 1/3

    tiled      = Tall nmaster delta ratio2
    threetiled = ThreeCol nmaster delta ratio3


main = do
	xmproc <- spawnPipe "/usr/bin/xmobar /home/m/.xmonad/xmobar-primary"
	xmonad $ defaultConfig {

            -- Define the names of all workspaces
            workspaces = ["1:main","2:web","3:mail","4:chat","5:dev","6:media", "7:misc", "8:misc", "9:misc", "10:misc"],
			borderWidth = 2,
			normalBorderColor  = "#dddddd",
			focusedBorderColor = "#ff0000",
			modMask = mod4Mask,		-- use WIN key as MOD
			terminal = "/usr/bin/terminator",

			startupHook = setWMName "LG3D",

			--manageHook = manageDocks <+> manageHook defaultConfig,
			manageHook = manageDocks <+> myManageHook,
			--layoutHook = avoidStruts $ layoutHook defaultConfig,
			--layoutHook = avoidStruts $ tiled ||| Mirror tiled ||| Full ||| threetiled,
			layoutHook = avoidStruts $ myLayout,
			logHook = dynamicLogWithPP $ xmobarPP {
					ppOutput = hPutStrLn xmproc,
					ppTitle = xmobarColor "green" "" . shorten 68
			}
	     } `additionalKeys` [
	     		-- focus
			((mod4Mask, xK_n),			windows W.focusDown),	-- focus next
			((mod4Mask, xK_p),			windows W.focusUp),	-- focus prev
			((mod4Mask .|. shiftMask, xK_n),	windows W.swapDown),	-- swap focus and next
			((mod4Mask .|. shiftMask, xK_p),	windows W.swapUp),	-- swap focus and prev
			((mod4Mask, xK_Return),			windows W.swapMaster),	-- swap master and focus

 			((mod4Mask, xK_KP_Subtract),		windows W.focusDown),	-- focus next
 			((mod4Mask, xK_KP_Add),			windows W.focusUp),	-- focus prev
 			((mod4Mask .|. shiftMask, xK_KP_Subtract), windows W.swapDown),	-- swap focus and next
 			((mod4Mask .|. shiftMask, xK_KP_Add),	windows W.swapUp),	-- swap focus and prev
 			((mod4Mask, xK_KP_Enter),		windows W.swapMaster),	-- swap master and focus

			((mod4Mask .|. controlMask, xK_Return),	spawn "/usr/bin/xterm -e tmux attach"),	-- open terminal session

			-- close/open window
			((mod4Mask , xK_q),			kill),			-- close focus

			-- execution, WM stuff
	     		((mod4Mask, xK_Escape),			spawn "xscreensaver-command -lock"),
			((mod4Mask, xK_z),			xmonadPrompt defaultXPConfig ),
			((mod4Mask, xK_s),			spawn "sudo pm-suspend"),
			((mod4Mask .|. shiftMask, xK_s),	spawn "sudo pm-hibernate"),
			((mod4Mask, xK_x), spawn "exe=`dmenu_path | dmenu -b -nb \"#220000\" -nf gray -sb red -sf white -fn \"-adobe-helvetica-medium-r-normal--14-100-*-*-*-*-*-*\"` && eval \"exec $exe\""),
			((mod4Mask .|. shiftMask, xK_BackSpace), spawn "exe=`dmenu_path | dmenu -b -nb \"#220000\" -nf gray -sb red -sf white -fn \"-adobe-helvetica-medium-r-normal--14-100-*-*-*-*-*-*\"` && eval \"exec $exe\""),
			((mod4Mask, xK_BackSpace),		shellPrompt defaultXPConfig),
			((mod4Mask .|. shiftMask, xK_m),	xmonadPrompt defaultXPConfig),
			((mod4Mask .|. shiftMask, xK_space),	layoutPrompt defaultXPConfig),
			((mod4Mask, xK_g),			windowPromptGoto  defaultXPConfig),
			((mod4Mask .|. shiftMask, xK_g),	windowPromptBring  defaultXPConfig),
			((mod4Mask .|. shiftMask, xK_q),	restart "xmonad" True),	-- reload config
			((mod4Mask .|. shiftMask .|. controlMask , xK_q),
								io (exitWith ExitSuccess)),
			((0 , xF86XK_AudioLowerVolume), spawn "amixer set Master on && amixer set Headphone on && amixer set Master 2-"), 
			((0 , xF86XK_AudioRaiseVolume), spawn "amixer set Master on && amixer set Headphone on && amixer set Master 2+"),
			((0 , xF86XK_AudioMute), spawn "amixer -D pulse set Master toggle && amixer -D pulse set Headphone toggle"),
			-- CycleWS bindings
			((mod4Mask, xK_Right),			moveTo Next HiddenNonEmptyWS),
			((mod4Mask, xK_Left),			moveTo Prev HiddenNonEmptyWS),
			((mod4Mask, xK_c),			moveTo Next EmptyWS),
			((mod4Mask .|. shiftMask, xK_Right),	shiftTo Next AnyWS),
			((mod4Mask .|. shiftMask, xK_Left),	shiftTo Prev AnyWS)

			-- special keys
			--
			--
	     ] `additionalMouseBindings` [
	     		((0, button8),			( \w ->	(spawn "/home/lostrace/bin/special_keys MOUSE8" ) )),
	     		((0, button9),			( \w ->	(spawn "/home/lostrace/bin/special_keys MOUSE9" ) )),
	     		((0, button10),			( \w ->	(spawn "/home/lostrace/bin/special_keys MOUSE10" ) )),
	     		((0, button11),			( \w ->	(spawn "/home/lostrace/bin/special_keys MOUSE11" ) ))
	     ]

