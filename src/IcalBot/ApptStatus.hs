module IcalBot.ApptStatus(
    ApptStatus(..)
  ) where

-- |This type is just to categorize in which context a day "was" selected by daysGrouped
data ApptStatus = ApptStarts
                | ApptEnds
