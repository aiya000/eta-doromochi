-- |
-- Export constant values of this app specific files.
--
-- These doesn't include a directory path, only the path of a file.
-- This means the constants requires to be combined with $HOME or somewhere.
module Doromochi.FilePath where

--TODO: Include the directory, e.g. zunkoOnTaskFirstHalf should be changed to "images/work_in_progress_head.png"

--TODO: Rename rest_time.png to on_long_rest.png
--TODO: Rename work_finishes.png to on_short_rest.png
--TODO: Rename work_in_progress_head.png to on_task_first_half.png
--TODO: Rename work_in_progress_tail.png to on_task_last_half.png

-- | A file path to a png image, please see 'correspondZunko'
zunkoOnTaskFirstHalf :: FilePath
zunkoOnTaskFirstHalf = "work_in_progress_head.png"

zunkoOnTaskLastHalf :: FilePath
zunkoOnTaskLastHalf = "work_in_progress_tail.png"

zunkoOnShortRest :: FilePath
zunkoOnShortRest = "work_finishes.png"

zunkoOnLongRest :: FilePath
zunkoOnLongRest = "rest_time.png"


-- | A file path to a sound, please see 'startClock'
seLongRestFinishes :: FilePath
seLongRestFinishes = "long_rest_is_finished.mp3"

seShortRestFinishes :: FilePath
seShortRestFinishes = "short_rest_is_finished.mp3"

seTaskFinishes :: FilePath
seTaskFinishes = "task_is_finished.mp3"


-- | A file path to an user config of 'PomodoroIntervals' ('intervalPrefs')
configOfIntervals :: FilePath
configOfIntervals = "prefs-interval"
