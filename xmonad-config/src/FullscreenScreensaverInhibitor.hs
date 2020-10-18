{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NumericUnderscores #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

-- XXX: Make it inhibit only fullscreen window is visible on some workspace

module FullscreenScreensaverInhibitor where

import System.Posix.Types (ProcessID)
import System.Posix.Signals (signalProcess, sigKILL)
import           Control.Monad (forever, forM, void)
import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Concurrent.STM.TVar
import           System.IO.Unsafe (unsafePerformIO)

import           Data.Set (Set)
import qualified Data.Set as S

import           XMonad
import qualified XMonad.Util.ExtensibleState as ES
import           XMonad.Layout.LayoutModifier (LayoutModifier, handleMess, ModifiedLayout(..))
import           XMonad.Layout.Fullscreen (FullscreenMessage(..))

xscreensaverKicker :: TVar (Maybe ProcessID)
{-# NOINLINE xscreensaverKicker #-}
xscreensaverKicker = unsafePerformIO $ newTVarIO $ Nothing

data FullscreenScreensaverInhibitor a = NoScreenSaverOnFullScreen (Set Window) deriving (Show, Read)

disableScreensaverWhenFullscreen = ModifiedLayout (NoScreenSaverOnFullScreen S.empty)

instance LayoutModifier FullscreenScreensaverInhibitor a where
  handleMess (NoScreenSaverOnFullScreen ws) m = do

    let mesToSetOp (Just (AddFullscreen win)) = S.insert win
        mesToSetOp (Just (RemoveFullscreen win)) = S.delete win
        mesToSetOp (_) = id

    io $ case fromMessage m of
      Just (AddFullscreen w) -> do
        -- trace $ "Add " ++ show w ++ " to " ++ show ws
        pure $ Just $ NoScreenSaverOnFullScreen (mesToSetOp (fromMessage m) ws)
      Just (RemoveFullscreen w) -> do
        -- trace $ "Remove " ++ show w ++ " from " ++ show ws
        pure $ Just $ NoScreenSaverOnFullScreen (mesToSetOp (fromMessage m) ws)
      Just FullscreenChanged -> do
        -- trace $ "Changed " ++ show ws
        case S.null ws of
          True -> io $ stopInhibitor
          False -> io $ startInhibitor
        pure Nothing
      _ ->
        pure Nothing


screensaverKickInterval = 60 * 3
maxInhibitonTime = 3600 * 2

startInhibitor = do
  stopInhibitor
  trace "Starting screensaver inhibitor"
  tid <- xfork $ void $ forM [1..(maxInhibitonTime `div` screensaverKickInterval)] $ \_ -> do
    trace "Kicking xscreensaver"
    spawn "xscreensaver-command -deactivate"
    threadDelay (screensaverKickInterval * 1_000_000)
  atomically $ writeTVar xscreensaverKicker $ Just tid
  pure Nothing

stopInhibitor = do
  readTVarIO xscreensaverKicker >>= \case
    Just tid -> do
      trace "Stopping screensaver inhibitor"
      signalProcess sigKILL tid
    _ -> pure ()
  atomically $ writeTVar xscreensaverKicker $ Nothing
  pure Nothing
