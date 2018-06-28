module IcalBot.SubAppt(
    SubAppt(..)
  , appointmentDates
  ) where

import           Data.Functor           ((<$>))
import           Data.Maybe             (maybeToList)
import           IcalBot.AppointedTime  (AppointedTime, appTimeEnd,
                                         appTimeStart)
import           IcalBot.Appt           (Appt (apptTime))
import           IcalBot.ApptStatus     (ApptStatus (ApptEnds, ApptStarts))
import           IcalBot.DateOrDateTime (DateOrDateTime)

-- |An Appt possibly contains two "sub-appointments": For the
--  start and end. This class represents such a "sub-appointment".
data SubAppt = SubAppt {
    saAppt   :: Appt AppointedTime
  , saStatus :: ApptStatus
  , saTime   :: DateOrDateTime
  }

appointmentDates :: Appt AppointedTime -> [SubAppt]
appointmentDates appt =
  let time = apptTime appt
      start = SubAppt appt ApptStarts (appTimeStart time)
      end = SubAppt appt ApptEnds <$> appTimeEnd time
  in start : maybeToList end
