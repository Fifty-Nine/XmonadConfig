import Data.Ratio ((%))
import Graphics.X11.Xinerama(getScreenInfo)
import System.IO
import XMonad
import XMonad.Actions.CycleWS
import XMonad.Actions.FindEmptyWorkspace
import XMonad.Actions.Plane
import XMonad.Config.Gnome
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.DynamicLog
import XMonad.Layout.DwmStyle
import XMonad.Layout.Grid
import XMonad.Layout.IM
import XMonad.Layout.IndependentScreens
import XMonad.Layout.MagicFocus
import XMonad.Layout.Master
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Spiral
import XMonad.Layout.Tabbed
import qualified XMonad.StackSet as W
import XMonad.Util.EZConfig(additionalKeys)
import XMonad.Util.EZConfig(additionalKeysP)
import XMonad.Util.Loggers
import XMonad.Util.Run
import XMonad.Util.Themes
import XMonad.Prompt
import XMonad.Prompt.Shell
import qualified Data.Map as M
import XMonad.Layout.DecorationMadness

xrandrCmd = "/home/tprince/.screenlayout/xmonad.sh"

desktopAppsFolder = "/home/tprince/dev/Desktop"

windowEditCommand = "gvim"
browserCommand    = "/usr/bin/x-www-browser --proxy-pac-url=\"file:///home/tprince/.pac\""
vcsCommand        = "/opt/remote/bin/p4v"
chatCommand       = "/usr/bin/pidgin"
mailCommand       = "/usr/bin/thunderbird"
rdesktopCommand   = ("rdesktop -K -g 1920x1200 engdap-009 -r sound:remote "
                 ++ "-r clipboard:PRIMARYCLIPBOARD")
networkManagerCommand = "/usr/bin/nm-applet"

gpsSimCommand     = (desktopAppsFolder ++ "/GpsSimulator/GpsSim")
canSimCommand     = (desktopAppsFolder 
                 ++ "/CANSimulator/builds/linux/CANSimulator")
iprCommand        = "/usr/bin/totem http://128.255.60.59:8000/listen.pls"
sshCommand        = "ssh -N -D8080 princet-nas"
objViewerCommand  = "cd /home/tprince/dev/Venom/ObjectViewer && ./ObjectViewer"
totemCommand = "/usr/bin/totem"
totemPlayPauseCommand = "/usr/bin/totem --play-pause"
vmCommand = "/usr/bin/VBoxManage startvm 'Ubuntu x86' --type headless"
relayCommand = "/home/tprince/dev/Desktop/CanRelay/CanRelay 2>&1 > /home/tprince/.relay_log"
synergysCommand = "/usr/bin/synergys"

framebufferCommand width height depth = "qvfb -width "  ++ (show width) 
                                         ++ " -height " ++ (show height)
                                         ++ " -depth "  ++ (show depth)

takeScreenshot = spawn "/usr/bin/gnome-screenshot"

myKeysP = 
    [ ("C-1",         spawn windowEditCommand)
    , ("C-2",         spawn browserCommand)
    , ("C-3",         spawn vcsCommand)
    , ("C-4",         spawn chatCommand)
    , ("C-5",         spawn mailCommand)
    , ("C-6",         spawn rdesktopCommand)
    , ("C-7",         spawn gpsSimCommand)
    , ("C-8",         spawn canSimCommand)
    , ("C-9",         spawn objViewerCommand)
    , ("C-0",         spawn (framebufferCommand 1024 768 16))
    , ("M-`",         viewEmptyWorkspace)
    , ("M-<F4>",      kill)
    , ("M-S-P",       takeScreenshot)
    , ("M1-<Tab>",    nextScreen)
    , ("M-s",         swapNextScreen)
    , ("M1-S-<Tab>",  shiftNextScreen)
    , ("M-r",         shellPrompt defaultXPConfig { promptKeymap = M.fromList [((controlMask,xK_c), quit)] `M.union` promptKeymap defaultXPConfig } )
    , ("M-m",         spawn totemCommand)
    , ("M-<Space>",   spawn totemPlayPauseCommand)
    ]

myKeys bar1 bar2 =
    [((m .|. (modMask (myConfig bar1 bar2)), k), windows $ f i)
        | (i, k) <- zip myWorkspaces ([xK_1 .. xK_9] ++ [xK_0])
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]
    ]


myManageHook = composeAll . concat $
    [ [ className =? name --> doF (W.shift "0:junk") 
                            | name <- myClassJunkShifts ]
    , [ className =? name --> doF (W.shift "2:web") 
                            | name <- myClassWebShifts ]
    , [ className =? name --> doF (W.shift "3:perforce") 
                            | name <- myClassPerforceShifts ]
    , [ className =? name --> doF (W.shift "4:chat") 
                            | name <- myClassChatShifts ]
    , [ className =? name --> doF (W.shift "5:mail") 
                            | name <- myClassMailShifts ]
    , [ className =? name --> doF (W.shift "6:misc") 
                            | name <- myClassRemoteShifts ]
    , [ className =? name --> doF (W.shift "8:docs") 
                            | name <- myClassDocsShifts ]
    , [ className =? name --> doF (W.shift "9:misc")
                            | name <- myClassMiscShifts ]
    , [ className =? name --> doFloat 
                            | name <- myClassFloats ]
    , [ title     =? name --> doFloat 
                            | name <- myTitleFloats ] 
    , [ className =? name --> doIgnore 
                            | name <- myClassIgnores ]
    , [ resource  =? name --> doIgnore
                            | name <- myResIgnores ]
    , [ className =? name --> unfloat
                            | name <- myClassUnfloats ]
    ]
    where 
        unfloat = ask >>= doF . W.sink
        myClassJunkShifts = [ "Totem", "Qvfb" ]
        myClassWebShifts = ["Firefox", "Chromium-browser", "X-www-browser"]
        myClassPerforceShifts = ["P4v.bin"]
        myClassChatShifts = [ "Pidgin"
                            , "Empathy"
                            ]
        myClassMailShifts = [ "Thunderbird" ]
        myClassRemoteShifts = [ "VirtualBox"
                              , "rdesktop" 
                              ]
        myClassDocsShifts = [ "OpenOffice.org 3.2"
                            , "Evince"
                            , "ObjectViewer"
                            , "Spotify"
                            ]
        myClassMiscShifts = [ "CANSimulator", "GpsSim" ]
        myClassFloats = [ "Gimp"
                        , "Gcalctool"
                        , "XTerm"
                        , "Update-manager"
                        , "Gvimdiff"
                        , "Gvim"
                        , "pooledit-Main"
                        ]
        myTitleFloats = [ "PrintChanges"
                        , "LinesChanged"
                        , "PrintChangesWithLint"
                        , "Pdf Print Changes"
                        , "Browse for Files/Folder" 
                        , "Hardware Configuration"
                        , "CAN Hardware Error"
                        ]
        myClassIgnores = [ "Gnome-Screenshot" ]
        myResIgnores = [ "Do" ]
        myClassUnfloats = [ "rdesktop", "CANSimulator" ]

myTheme = theme smallClean

-- A layout for IM windows
imLayout = avoidStruts
           $ smartBorders
           $ withIM (1%7) (Or (And (ClassName "Pidgin") (Role "buddy_list")) (And (ClassName "Empathy") (Role "contact_list")))
             (tallDwmStyle shrinkText myTheme)
             --(dwmStyle shrinkText myTheme (Tall 0))

-- A layout for console windows
consoleLayout = avoidStruts
                $ smartBorders
                $ dwmStyle shrinkText myTheme
                $ mastered (1/100) (1/2) Grid

-- A layout for the rest
defaultLayout = avoidStruts
                $ smartBorders
                $ dwmStyle shrinkText myTheme (layoutHook defaultConfig)

tabbedLayout = avoidStruts
             $ smartBorders
             $ simpleTabbed

-- A layout for console windows
myLayout = onWorkspace "4:chat" imLayout
                       $ onWorkspace "1:console" consoleLayout
                       $ onWorkspace "0:junk" tabbedLayout
                       $ onWorkspace "6:misc" tabbedLayout
                       $ onWorkspace "8:docs" tabbedLayout
                       $ onWorkspace "9:misc" tabbedLayout
                       $ defaultLayout


myWorkspaces = [ "1:console"
               , "2:web"
               , "3:perforce"
               , "4:chat"
               , "5:mail"
               , "6:misc"
               , "7:misc"
               , "8:docs"
               , "9:misc"
               , "0:junk"
               ]

myLogOutput :: Handle -> Handle -> String -> IO ()
myLogOutput h1 h2 s = do hPutStrLn h1 s
                         hPutStrLn h2 s 

myPP bar1 bar2 = defaultPP { ppCurrent  = dzenColor "white" "#2b4f98" . pad
                           , ppVisible  = dzenColor "black" "#999999" . pad
                           , ppHidden   = dzenColor "black" "#cccccc" . pad
                           , ppHiddenNoWindows = const ""
                           , ppUrgent   = dzenColor "red" "yellow" . dzenStrip
                           , ppWsSep    = ""
                           , ppSep      = ""
                           , ppLayout   = const ""
                           , ppTitle    = shorten 50 . ("^bg(#324c80) " ++) . dzenEscape
                           , ppOutput   = myLogOutput bar1 bar2
                           , ppExtras   = [ date (dzenColor "black" "#cccccc" " %a %b %d, %Y @ %I:%M %p")
                                          , loadAvg 
                                          ]
                           }
myBarToggleKey XConfig {XMonad.modMask = modMask} = (modMask, xK_b)

keymapCmd = "/usr/bin/xmodmap /home/tprince/.xmodmaprc"

trayerCmd = "/usr/bin/trayer --edge top --align right "
          ++ "--width 100 --widthtype pixel --height 32 --transparent true "
          ++ "--alpha 0 --tint 0x000000"

leftBgImage = "/home/tprince/.xmonad/bg2.jpg"
rightBgImage = "/home/tprince/.xmonad/bg1.jpg"

setBgImages left right = spawn ( "xloadimage -onroot -border black -at 0,0 " 
                               ++ left 
                               ++ " -at 1920,0 "
                               ++ right
                               )


main = do
    spawn xrandrCmd
    bar1 <-spawnPipe "/usr/bin/dzen2 -xs 1 -w 1920 -h 32 -ta l -fn '-unregistered-latin modern sans-bold-r-expanded-*-17-120-100-100-p-0-iso8859-15'"
    bar2 <-spawnPipe "/usr/bin/dzen2 -xs 2 -w 1920 -h 32 -ta l -fn '-unregistered-latin modern sans-bold-r-expanded-*-17-120-100-100-p-0-iso8859-15'"
    spawn keymapCmd
    spawn trayerCmd
    setBgImages leftBgImage rightBgImage
    spawn synergysCommand
    spawn networkManagerCommand
    spawn sshCommand
    spawn browserCommand
    spawn vcsCommand
    spawn mailCommand
    spawn chatCommand
    spawn rdesktopCommand
    spawn (framebufferCommand 1024 768 16)
    spawn gpsSimCommand
    spawn canSimCommand
    spawn vmCommand
    spawn relayCommand
    --spawn iprCommand
    xmonad $ (myConfig bar1 bar2)

myConfig bar1 bar2 = defaultConfig
                     { manageHook = manageDocks <+> myManageHook
                                    <+> manageHook defaultConfig
                     , layoutHook = myLayout
                     , logHook = dynamicLogWithPP (myPP bar1 bar2) >> logHook defaultConfig
                     , modMask = mod4Mask
                     , workspaces = myWorkspaces
                     , focusFollowsMouse = True
                     , terminal = "gnome-terminal"
                     }
                     `additionalKeysP` myKeysP
                     `additionalKeys`  (myKeys bar1 bar2)


                    
