-- | Present 'PreferencesPane' to update `'IORef' 'PomodoroTimer'`
module Doromochi.View.PreferencesPane
  ( PreferencesPane
  , newPreferencesPane
  ) where

import Control.Monad.Doromochi ((.>.))
import Data.IORef (IORef, readIORef, modifyIORef)
import Doromochi.FilePath (configOfIntervals)
import Doromochi.Types
import Java
import JavaFX
import System.Environment (getEnv)

type PreferencesPane = FlowPane

--TODO: Use a `'SpinnerValueFactory' 'Seconds'` instead of `'SpinnerValueFactory' JInteger`
-- | Make a 'PreferencesPane' to update the 'PomodoroIntervals' in given `'IORef' 'PomodoroTimer'`
newPreferencesPane :: PomodoroTimer -> Java a FlowPane
newPreferencesPane (PomodoroTimer prefsRef _ _) = do
  prefs <- io $ readIORef prefsRef
  (timeOnTaskSpinner, timeOnTaskPane)             <- makePaneATime "仕事時間: " $ timeOnTask prefs
  (timeOnShortRestSpinner, timeOnShortRestPane)   <- makePaneATime "休憩: "     $ timeOnShortRest prefs
  (lengthToLongRestSpinner, lengthToLongRestPane) <- makePaneLengthToLongRest   $ lengthToLongRest prefs
  (timeOnLongRestSpinner, timeOnLongRestPane)     <- makePaneATime "超休憩: "   $ timeOnLongRest prefs
  applyButton <- makeApplyButton prefsRef ( timeOnTaskSpinner
                                          , timeOnShortRestSpinner
                                          , lengthToLongRestSpinner
                                          , timeOnLongRestSpinner
                                          )
  noticeLabel <- newLabel "注意: 「保存」ボタンを押すと、現在進行中のタイマーがリセットされます。ポモドーロ実施中の場合はご注意ください。"
  newFlowPane verticalOrient [ superCast timeOnTaskPane
                             , superCast timeOnShortRestPane
                             , superCast lengthToLongRestPane
                             , superCast timeOnLongRestPane
                             , superCast applyButton
                             , superCast noticeLabel
                             ]


makePaneATime :: String -> Seconds -> Java a (Spinner JInteger, FlowPane)
makePaneATime subject current = do
  label        <- newLabel subject
  spinner      <- newSpinner1ToFullDay current
  secondsLabel <- newLabel "分"
  whole <- newFlowPane horizontalOrient [ superCast label
                                        , superCast spinner
                                        , superCast secondsLabel
                                        ]
  return (spinner, whole)
  where
    -- Make new Spinner with default values
    newSpinner1ToFullDay :: Seconds -> Java a (Spinner JInteger)
    newSpinner1ToFullDay (Seconds x) = do
      let fullDay = unSeconds $ minutes 60 .* 24 - 1
      newSpinner 1 fullDay x


makePaneLengthToLongRest :: Int -> Java a (Spinner JInteger, FlowPane)
makePaneLengthToLongRest current = do
  label      <- newLabel "長休憩までの仕事時間〜休憩の繰り返し回数: "
  spinner    <- newSpinner 1 999 current
  countLabel <- newLabel "回"
  whole <- newFlowPane horizontalOrient [ superCast label
                                        , superCast spinner
                                        , superCast countLabel
                                        ]
  return (spinner, whole)


-- |
-- Make a button.
--
-- That button aggregates the final values of 'Spinner's,
-- Replace the value of given `'IORef' 'PomodoroTimer'` with the aggregated result,
-- and Reset 'pomodoroClock' to 1
makeApplyButton :: IORef PomodoroIntervals -> (Spinner JInteger, Spinner JInteger, Spinner JInteger, Spinner JInteger) -> Java a Button
makeApplyButton prefsRef spinners = do
  self <- newButton "保存"
  self <.> setOnButtonAction (hotloadPrefs prefsRef spinners .>. savePrefs spinners)
  return self
  where
    aggregatePrefs :: (Spinner JInteger, Spinner JInteger, Spinner JInteger, Spinner JInteger) -> Java a PomodoroIntervals
    aggregatePrefs (timeOnTaskSpinner, timeOnShortRestSpinner, lengthToLongRestSpinner, timeOnLongRestSpinner) = do
      timeOnTask'       <- Seconds . fromJava <$> timeOnTaskSpinner <.> getSpinnerValue
      timeOnShortRest'  <- Seconds . fromJava <$> timeOnShortRestSpinner <.> getSpinnerValue
      lengthToLongRest' <- fromJava <$> lengthToLongRestSpinner <.> getSpinnerValue
      timeOnLongRest'   <- Seconds . fromJava <$> timeOnLongRestSpinner <.> getSpinnerValue
      let x = PomodoroIntervals
                { timeOnTask       = timeOnTask'
                , timeOnShortRest  = timeOnShortRest'
                , lengthToLongRest = lengthToLongRest'
                , timeOnLongRest   = timeOnLongRest'
                }
      io $ print x
      return x

    hotloadPrefs :: IORef PomodoroIntervals -> (Spinner JInteger, Spinner JInteger, Spinner JInteger, Spinner JInteger) -> (ActionEvent -> Java (EventHandler ActionEvent) ())
    hotloadPrefs prefsRef spinners = \_ -> do
      newPrefs <- aggregatePrefs spinners
      io . modifyIORef prefsRef $ const newPrefs

    -- Save the input 'PomodoroIntervals' to the expected path
    savePrefs :: (Spinner JInteger, Spinner JInteger, Spinner JInteger, Spinner JInteger) -> (ActionEvent -> Java (EventHandler ActionEvent) ())
    savePrefs spinners = \_ -> do
      newPrefs <- aggregatePrefs spinners
      io $ do
        print newPrefs
        configPath <- (++ configOfIntervals) . (++ "/.config/doromochi/") <$> getEnv "HOME"
        writeFile configPath $ show newPrefs
