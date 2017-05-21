module Test.Main where

import Prelude (discard, Unit)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Aff.AVar (AVAR)
import Test.Unit.Console (TESTOUTPUT)
import Test.Data.HObject (main) as HObject
import Test.Data.HObject.Record (main) as HObjectRecord


main :: Eff ( console :: CONSOLE, testOutput :: TESTOUTPUT, avar :: AVAR ) Unit
main = do
  HObject.main
  HObjectRecord.main
