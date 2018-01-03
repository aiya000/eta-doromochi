-- |
-- Export constant values of this app specific files.
--
-- These doesn't include a directory path, only the path of a file.
-- This means the constants requires to be combined with $HOME or somewhere.
module Doromochi.FilePath where

--TODO: Rename work_finishes.png to on_long_rest.png
--TODO: Rename rest_time.png to on_short_rest.png
--TODO: Rename work_in_progress_head.png to on_task_first_half.png
--TODO: Rename work_in_progress_tail.png to on_task_last_half.png

-- | A file path to a png image, please see 'correspondZunko'
zunkoOnTaskFirstHalf :: FilePath
zunkoOnTaskFirstHalf = "work_in_progress_head.png"

zunkoOnTaskLastHalf :: FilePath
zunkoOnTaskLastHalf = "work_in_progress_tail.png"

zunkoOnShortRest :: FilePath
zunkoOnShortRest = "rest_time.png"

zunkoOnLongRest :: FilePath
zunkoOnLongRest = "work_finishes.png"
