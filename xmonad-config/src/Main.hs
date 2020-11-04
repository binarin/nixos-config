{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

import           Control.Lens

import           Control.Monad (when, join, void)
import           Data.Default
import           Data.List (sortBy, find, isInfixOf)
import           Data.Maybe (maybeToList, fromMaybe)
import           Data.Monoid (All(..), (<>))
import           Data.Ratio ((%))
import           Data.Set (Set)
import           Debug.Trace (traceShow)
import           System.Exit
import qualified Data.Map as M
import qualified Data.Set as Set

import XMonad.Actions.RotSlaves
import           Graphics.X11.ExtraTypes.XF86
import           System.Taffybar.Support.PagerHints (pagerHints)
import           XMonad
import           XMonad.Actions.CycleWS
import           XMonad.Actions.CycleWindows
import           XMonad.Actions.GridSelect
import           XMonad.Actions.Navigation2D
import           XMonad.Actions.OnScreen
import           XMonad.Core (withWindowSet, fromMessage)
import           XMonad.Hooks.CurrentWorkspaceOnTop (currentWorkspaceOnTop)
import           XMonad.Hooks.EwmhDesktops (ewmhDesktopsStartup, ewmhDesktopsLogHook, fullscreenEventHook)
import           XMonad.Hooks.ManageDebug (debugManageHookOn)
import           XMonad.Hooks.ManageDocks (docks, avoidStruts)
import           XMonad.Hooks.ManageHelpers (doFullFloat, isFullscreen, doCenterFloat, Side(..), doSideFloat, doFloatAt, composeOne)
import           XMonad.Hooks.Place (smart, fixed, withGaps, inBounds, placeHook, simpleSmart)
import           XMonad.Hooks.UrgencyHook (withUrgencyHookC, NoUrgencyHook(NoUrgencyHook), focusUrgent, urgencyConfig)
import qualified XMonad.Hooks.UrgencyHook as Urgency
import           XMonad.Layout.Accordion

-- NOTE: Not compatible with NoFullscreenBorders defined below
-- NOTE: The only reason to use this was that it also produces layout messages, which I tried to use for screen-saver inhibitor
-- import           XMonad.Layout.Fullscreen (FullscreenMessage(..), fullscreenSupport, fullscreenFull, fullscreenFloat)


import           XMonad.Layout.Grid
import           XMonad.Layout.IM
import           XMonad.Layout.LayoutModifier (LayoutModifier, handleMess, ModifiedLayout(..))
import           XMonad.Layout.NoBorders
import           XMonad.Layout.PerWorkspace
import           XMonad.Layout.SimpleFloat (simpleFloat)
import           XMonad.Layout.Tabbed
import           XMonad.Prompt
import           XMonad.Prompt.Pass (passPrompt)
import qualified XMonad.StackSet as W
import           XMonad.Util.EZConfig
import qualified XMonad.Util.ExtensibleState as ES
import           XMonad.Util.WorkspaceCompare (getSortByIndex)
import           XMonad.Util.XUtils (fi)

-- from local lib/
import           Xkb
import           FullscreenScreensaverInhibitor (disableScreensaverWhenFullscreen)
import           XMonad.Layout.Ultrawide (Ultrawide(..))

import System.IO

debugStuff :: X ()
debugStuff = withWindowSet (\ws -> do
    liftIO $ print ws
    liftIO $ logToTmpFile $ show ws
  )

myAppendFile :: FilePath -> String -> IO ()
myAppendFile f s = do
  withFile f AppendMode $ \h -> do
    hPutStrLn h s

logToTmpFile :: String -> IO ()
logToTmpFile = myAppendFile "/tmp/xmonad.log" . (++ "\n")

primaryWorkspaces :: [(String, String)]
primaryWorkspaces =
  [ ("term", "1")
  , ("emacs", "2")
  , ("web", "3")
  , ("msg", "4")
  , ("jabber", "5")
  , ("misc", "`")
  ]

secondaryWorkspaces :: [(String, String)]
secondaryWorkspaces =
  [ ("secondary", "6")
  , ("coins", "7")
  , ("passwd", "8")
  , ("secondary2", "9")
  , ("secondary3", "0")
  , ("secondary4", "-")
  ]

scratchpadWorkspace :: (String, String)
scratchpadWorkspace = ("scratch", "<Esc>")

myManageHook :: ManageHook
myManageHook = composeAll
    [ className =? "Sshmenu"        --> doFloat
    -- , isFullscreen                  --> doFullFloat
    -- , resource  =? "desktop_window" --> doIgnore
    -- , resource  =? "kdesktop"       --> doIgnore
    -- , className =? "Xfce4-notifyd"  --> doIgnore
    -- , className =? "Wine"           --> doFloat
    -- , className =? "Skype"          --> doF (W.shift "msg")
    -- , className =? "Viber"          --> doF (W.shift "msg")
    -- , className =? "Workrave"       --> doFloat
    -- , className =? "looking-glass-client" --> doFloat
    , title     =? "FAST_CHOICE"    --> doCenterFloat
    , className =? "looking-glass-client" --> hasBorder False
    -- , className =? "kitty" --> do
    --     w <- ask
    --     liftX $ killWindow w
    --     doF (W.delete w)
    ]

myBordersMod = lessBorders (NoFullscreenBorders Never)

data NoFullscreenBorders = NoFullscreenBorders Ambiguity deriving (Read, Show)

instance SetsAmbiguous NoFullscreenBorders where
  hiddens (NoFullscreenBorders amb) ws parentRect maybeStack wrs =
    (fst <$> fullFloats) ++ hiddens amb ws parentRect maybeStack wrs
    where
      floats = M.toList $ W.floating ws
      fullRect = W.RationalRect (0 % 1) (0 % 1) (1 % 1) (1 % 1)
      fullFloats = filter (\(_, r) -> r == fullRect) floats

myLayout = myBordersMod perWorkspace

  where
    defaultLayout = ultrawide ||| Full ||| Mirror tiled ||| tiled
    perWorkspace = onWorkspace "secondary" simpleFloat defaultLayout
    tiled = Tall nmaster delta ratio
    ultrawide = Ultrawide 1 (5/100) (1/3)
    nmaster = 1
    ratio = 1/2
    delta = 5/100

myLayoutHook =
  -- fullscreenFloat $
  xkbLayout $
  avoidStruts $
  myLayout

myNavigation :: TwoD a (Maybe a)
myNavigation = makeXEventhandler $ shadowWithKeymap navKeyMap navDefaultHandler
 where navKeyMap = M.fromList [
          ((0,xK_Escape), cancel)
         ,((0,xK_Return), select)
         ,((0,xK_slash) , substringSearch myNavigation)
         ,((0,xK_Left)  , move (-1,0)  >> myNavigation)
         ,((0,xK_d)     , move (-1,0)  >> myNavigation)
         ,((0,xK_Right) , move (1,0)   >> myNavigation)
         ,((0,xK_n)     , move (1,0)   >> myNavigation)
         ,((0,xK_Down)  , move (0,1)   >> myNavigation)
         ,((0,xK_h)     , move (0,1)   >> myNavigation)
         ,((0,xK_Up)    , move (0,-1)  >> myNavigation)
         ,((0,xK_t)     , move (0,-1)  >> myNavigation)
         ,((0,xK_f)     , move (-1,-1) >> myNavigation)
         ,((0,xK_c)     , move (1,-1)  >> myNavigation)
         ,((0,xK_b)     , move (-1,1)  >> myNavigation)
         ,((0,xK_m)     , move (1,-1)  >> myNavigation)
         ,((0,xK_space) , setPos (0,0) >> myNavigation)
         ]
       -- The navigation handler ignores unknown key symbols
       navDefaultHandler = const myNavigation

gsconfig1 :: HasColorizer a => GSConfig a
gsconfig1 = def
    { gs_navigate = myNavigation }

main :: IO ()
main = do
  xmonad $ debugManageHookOn "M-u" myConfig

myManageFloats :: ManageHook
myManageFloats = placeHook $ inBounds $ withGaps (16,0,16,0) (fixed (0.5,0.5))


configModifiers =
      withUrgencyHookC NoUrgencyHook urgencyConfig {Urgency.suppressWhen = Urgency.Never}
    . myEwmh
    . pagerHints
    . arrowNavigation
    . docks
  where
    arrowNavigation =
      navigation2DP def ("<Up>", "<Left>", "<Down>", "<Right>")
        [("M-",   windowGo  ),
         ("M-S-", windowSwap)]
        True

myEwmh :: XConfig l -> XConfig l
myEwmh xc = xc { startupHook = startupHook xc <> ewmhDesktopsStartup  <> addEWMHFullscreen
               , handleEventHook = handleEventHook xc <> myEwmhDesktopsEventHook <> fullscreenEventHook <> borderEventHook
               , logHook = logHook xc <> ewmhDesktopsLogHook
               }

myEwmhDesktopsEventHook :: Event -> X All
myEwmhDesktopsEventHook ClientMessageEvent
  { ev_window = w
  , ev_message_type = mt
  , ev_data = d
  } = withWindowSet $ \s -> do
    sort' <- getSortByIndex
    let ws = sort' $ W.workspaces s

    a_cd <- getAtom "_NET_CURRENT_DESKTOP"
    a_d <- getAtom "_NET_WM_DESKTOP"
    a_aw <- getAtom "_NET_ACTIVE_WINDOW"
    a_cw <- getAtom "_NET_CLOSE_WINDOW"
    if  mt == a_cd then do
            let n = head d
            if 0 <= n && fi n < length ws then
                    showWSOnProperScreen (W.tag (ws !! fi n))
              else  trace $ "Bad _NET_CURRENT_DESKTOP with data[0]="++show n
     else if mt == a_d then do
            let n = head d
            if 0 <= n && fi n < length ws then
                    windows $ W.shiftWin (W.tag (ws !! fi n)) w
              else  trace $ "Bad _NET_DESKTOP with data[0]="++show n
     else if mt == a_aw then do
            case W.findTag w s of
              Nothing -> pure ()
              Just tag -> do
                showWSOnProperScreen tag
            windows $ W.focusWindow w
     else if mt == a_cw then do
            killWindow w
     else do
       -- The Message is unknown to us, but that is ok, not all are meant
       -- to be handled by the window manager
        pure ()
    return (All True)
myEwmhDesktopsEventHook _ = return (All True)

showWSOnProperScreen :: String -> X ()
showWSOnProperScreen ws = case classifyWorkspace ws of
  Primary -> switchToPrimary ws
  Secondary -> switchToSecondary ws
  Tertiary -> switchToTertiary ws

myConfig =  configModifiers def
  { modMask = mod4Mask
  , workspaces = map fst (primaryWorkspaces ++ secondaryWorkspaces ++ [scratchpadWorkspace])
  , terminal           = "urxvt"
  , borderWidth        = 3
  , normalBorderColor  = "#cccccc"
  , focusedBorderColor = "#cd8b00"
  , manageHook = myManageFloats <+> myManageHook <+> manageHook def
  , handleEventHook = mconcat $
                      [ handleEventHook def
                      , onRescreen placeWorkplaces
                      ]
  , layoutHook = myLayoutHook
  , logHook = currentWorkspaceOnTop
  , startupHook = startupHook def >> placeWorkplaces
  }
        `additionalKeysP`
        ([ ("M-p", spawn "exe=$(yeganesh -x) && exec $exe")
         , ("C-\\", sendMessage (XkbToggle Nothing))
         , ("M-i", getPassword)
         , ("M-;", spawn "sshmenu")
         , ("M-<Backspace>", windows W.swapMaster)
         , ("M-<Tab>", rotSlavesUp)
         , ("M-S-<Tab>", rotSlavesDown)
         , ("M-S-l", debugStuff)

         -- , ("M-S-l", spawn "gmrun")
         , ("M-<Print>", spawn "escrotum -s -C")
         -- -- , ("M-q", spawn "xmonad --recompile && xmonad --restart")
         -- -- , ("M-S-q", io (exitWith ExitSuccess))
         -- , ("M-h", windows W.focusDown)
         -- , ("M-t", windows W.focusUp)
         -- , ("M-m", windows W.focusMaster)
         -- , ("M-S-h", windows W.swapDown)
         -- , ("M-S-t", windows W.swapUp)
         -- , ("M-d", sendMessage Shrink)
         -- , ("M-b", withFocused $ \w ->  sendMessage (HasBorder False w))
         -- , ("M-S-b", withFocused $ \w ->  sendMessage (ResetBorder w))
         -- , ("M-n", sendMessage Expand)
         -- , ("M-w", sendMessage (IncMasterN 1))
         -- , ("M-v", sendMessage (IncMasterN (-1)))
         , ("M-'", goToSelected gsconfig1)
         -- , ("M-s", withFocused $ windows . W.sink)
         -- , ("M-<Right>", nextScreen)
         -- , ("M-<Left>", prevScreen)
         -- , ("M-<Up>", gridselectWorkspace gsconfig1 (\ws -> W.shift ws))
         -- , ("M-S-,", screenWorkspace 0 >>= flip whenJust (windows . W.shift))
         -- , ("M-S-.", screenWorkspace 1 >>= flip whenJust (windows . W.shift))
         -- , ("M-S-p", screenWorkspace 2 >>= flip whenJust (windows . W.shift))
         , ("M-S-c", withFocused $ \w -> do
               withDisplay $ \dpy -> do
                 classHint <- io $ getClassHint dpy w
                 when (resClass classHint /= "Workrave") $ do
                   killWindow w
               )
         -- , ("M-<Backspace>", cycleRecentWindows [xK_Super_L, xK_Super_R] xK_BackSpace xK_Delete)
         -- , ("M-g", focusUrgent)
        ]
         ++ [ ("M-" ++ key, switchToPrimary name)
            | (name, key) <- primaryWorkspaces ]
         ++ [ ("M-" ++ key, switchToSecondary name)
            | (name, key) <- secondaryWorkspaces ]
         ++ [ ("M-" ++ key, switchToTertiary name)
            | (name, key) <- [scratchpadWorkspace] ]
        )
        `additionalKeys`
        [ ((0, xF86XK_Mail), return ())
        , ((0, xF86XK_MonBrightnessUp), spawn "light -A 5")
        , ((0, xF86XK_MonBrightnessDown), spawn "light -U 5")
        , ((0, xF86XK_TouchpadToggle), spawn "toggle-touchpad")
        , ((0, xF86XK_AudioLowerVolume), spawn "volume.sh down")
        , ((0, xF86XK_AudioRaiseVolume), spawn "volume.sh up")
        , ((0, xF86XK_AudioMute), spawn "volume.sh mute")
        , ((0, xF86XK_AudioPlay), spawn "playerctl play-pause")
        , ((0, xF86XK_AudioNext), spawn "playerctl next")
        , ((0, xF86XK_AudioPrev), spawn "playerctl previous")
        ]

-- primary workspaces, secondary workspaces, scratchpad(s?)
-- 1 screen: everything on it
-- 2 screens: primary on 1st, secondary and scratchpad ond 2nd
-- 3 screens: primary on 1st, secondary on 2nd, scratchpad always on 3rd

-- 3 screens at work: primary in the middle, secondary to the left, tertiary to the right (laptop)
-- 3 screen at home: secondary to the left of primary, and tertiary is below secondary


data MonitorConfig
  = SingleMonitor
  | DualMonitor
  | TripleMonitor ScreenId ScreenId

data WorkspaceChoice = WorkspaceChoice WorkspaceId WorkspaceId WorkspaceId deriving (Typeable, Read, Show, Eq)
instance ExtensionClass WorkspaceChoice where
  initialValue = WorkspaceChoice (fst $ head primaryWorkspaces) (fst $ head secondaryWorkspaces) (fst scratchpadWorkspace)
  extensionType = PersistentExtension

primaryChoiceL :: Lens' WorkspaceChoice WorkspaceId
primaryChoiceL k (WorkspaceChoice prim sec ter) = fmap (\newPrim -> WorkspaceChoice newPrim sec ter) (k prim)

secondaryChoiceL :: Lens' WorkspaceChoice WorkspaceId
secondaryChoiceL k (WorkspaceChoice prim sec ter) = fmap (\newSec -> WorkspaceChoice prim newSec ter) (k sec)

tertiaryChoiceL :: Lens' WorkspaceChoice WorkspaceId
tertiaryChoiceL k (WorkspaceChoice prim sec ter) = fmap (\newTer -> WorkspaceChoice prim sec newTer) (k ter)

switchToPrimary :: WorkspaceId -> X ()
switchToPrimary name = do
  windows $ viewPrimary name
  ES.modify $ primaryChoiceL .~ name
  pure ()

switchToSecondary :: WorkspaceId -> X ()
switchToSecondary name = do
  windows $ viewSecondary name
  ES.modify $ secondaryChoiceL .~ name
  pure ()

switchToTertiary :: WorkspaceId -> X ()
switchToTertiary name = do
  windows $ viewTertiary name
  ES.modify $ tertiaryChoiceL .~ name
  pure ()

detectMonitorConfig :: WindowSet -> MonitorConfig
detectMonitorConfig W.StackSet {W.visible = []} = SingleMonitor
detectMonitorConfig W.StackSet {W.visible = [_]} = DualMonitor
detectMonitorConfig ss@(W.StackSet {W.visible = (_:_:[])}) = TripleMonitor (chooseSecondary ss) (chooseTertiary ss)
detectMonitorConfig _ = SingleMonitor -- No idea how to handle more than 3 monitors =)

scratchPadPosition :: WindowSet -> ScreenId
scratchPadPosition ss = go (detectMonitorConfig ss)
  where
    go SingleMonitor = 0
    go DualMonitor = 1
    go (TripleMonitor _ ter) = ter


-- very dumb logic - top-leftmost screen is always secondary
chooseSecondary :: WindowSet -> ScreenId
chooseSecondary W.StackSet { W.visible = visible, W.current = current } =
  case sorted of
    _primary:a:b:_ ->
      case screenRect (W.screenDetail a) of
        Rectangle { rect_x = 0, rect_y = 0 } -> W.screen a
        _ -> W.screen b
    _ -> W.screen current
  where
    allScreens = current : visible
    sorted = sortBy (\x y -> compare (W.screen x) (W.screen y)) allScreens

chooseTertiary :: WindowSet -> ScreenId
chooseTertiary ss = case chooseSecondary ss of
                      1 -> 2
                      2 -> 1
                      _ -> 1

viewPrimary, viewSecondary, viewTertiary :: WorkspaceId -> WindowSet -> WindowSet

viewPrimary i ss = go (detectMonitorConfig ss)
  where
    go SingleMonitor = W.view i ss
    go _ = greedyViewOnScreen 0 i ss

viewSecondary i ss = go (detectMonitorConfig ss)
  where
    go SingleMonitor = W.view i ss
    go DualMonitor = greedyViewOnScreen 1 i ss
    go (TripleMonitor sec _) = greedyViewOnScreen sec i ss

viewTertiary i ss = go (detectMonitorConfig ss)
  where
    go SingleMonitor = W.view i ss
    go DualMonitor = greedyViewOnScreen 1 i ss
    go (TripleMonitor _ ter) = greedyViewOnScreen ter i ss

getActualWorkspaceChoice :: X WorkspaceChoice
getActualWorkspaceChoice = do
  cfg <- detectMonitorConfig <$> gets windowset
  WorkspaceChoice _ secChoice terChoice <- ES.get
  priReal <- workspaceOnScreen 0
  case cfg of
    SingleMonitor ->
      pure $ WorkspaceChoice priReal secChoice terChoice
    DualMonitor -> do
      secReal <- workspaceOnScreen 1
      pure $ WorkspaceChoice priReal secReal terChoice
    TripleMonitor secSid terSid -> do
      secReal <- workspaceOnScreen secSid
      terReal <- workspaceOnScreen terSid
      pure $ WorkspaceChoice priReal secReal terReal

fixWorkspaceChoice :: WorkspaceChoice -> WorkspaceChoice -> WorkspaceChoice
fixWorkspaceChoice (WorkspaceChoice priChoice secChoice terChoice) (WorkspaceChoice priReal secReal terReal) =
    WorkspaceChoice priNew secNew terNew
  where
    mkOverride ws typ = if classifyWorkspace ws /= typ then [ws] else []
    everything = mkOverride priReal Primary
              ++ mkOverride secReal Secondary
              ++ mkOverride terReal Tertiary
              ++ [priChoice, secChoice, terChoice]
    chooseWorkspace typ dflt = fromMaybe dflt $ find (\x -> classifyWorkspace x == typ) everything
    priNew = chooseWorkspace Primary priChoice
    secNew = chooseWorkspace Secondary secChoice
    terNew = chooseWorkspace Tertiary terChoice

workspaceOnScreen :: ScreenId -> X WorkspaceId
workspaceOnScreen sid = do
  W.StackSet { W.current = currentScreen, W.visible = visibleScreens} <- gets windowset
  let allScreens = currentScreen:visibleScreens
  pure $ W.tag $ W.workspace $ head $ filter (\x -> W.screen x == sid) allScreens

data WorkspaceType = Primary | Secondary | Tertiary deriving (Eq)
classifyWorkspace :: WorkspaceId -> WorkspaceType

classifyWorkspace ws
  | ws `elem` (fst <$> primaryWorkspaces) = Primary
  | ws `elem` (fst <$> secondaryWorkspaces) = Secondary
  | otherwise = Tertiary

addNETSupported :: Atom -> X ()
addNETSupported x   = withDisplay $ \dpy -> do
    r               <- asks theRoot
    a_NET_SUPPORTED <- getAtom "_NET_SUPPORTED"
    a               <- getAtom "ATOM"
    liftIO $ do
        sup <- (join . maybeToList) <$> getWindowProperty32 dpy a_NET_SUPPORTED r
        when (fromIntegral x `notElem` sup) $
          changeProperty32 dpy r a_NET_SUPPORTED a propModeAppend [fromIntegral x]

addEWMHFullscreen :: X ()
addEWMHFullscreen   = do
    wms <- getAtom "_NET_WM_STATE"
    wfs <- getAtom "_NET_WM_STATE_FULLSCREEN"
    -- liftIO $ appendFile "/tmp/xm.log" $ "enabling fullscreen"
    mapM_ addNETSupported [wms, wfs]

placeWorkplaces :: X ()
placeWorkplaces = do
  monConf <- detectMonitorConfig <$> gets windowset
  WorkspaceChoice prim sec ter <- ES.get
  case monConf of
    SingleMonitor -> do
      switchToPrimary prim
    DualMonitor -> do
      switchToSecondary sec
      switchToPrimary prim
    TripleMonitor _ _ -> do
      switchToTertiary ter
      switchToSecondary sec
      switchToPrimary prim

onRescreen :: X () -> Event -> X All
onRescreen u (ConfigureEvent {ev_window = w}) = do
  rootPred <- isRoot w
  case rootPred of
    True -> do
      rescreen
      u
      return (All False)
    _ -> return (All True)
onRescreen _ _ = return (All True)

getPassword = passPrompt def { font = "xft:Arial:size=20"
                             , height = 40
                             , searchPredicate = \input variant -> input `isInfixOf` variant
                             , position = Top
                             }

