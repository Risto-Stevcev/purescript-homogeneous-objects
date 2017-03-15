module Data.HObject.Record where

import Prelude (($), bind, pure, show)
import Data.Either (Either(..), either)
import Data.HObject (HObject)
import Data.Foreign (F, Foreign, toForeign)
import Data.Foreign.Class (class IsForeign, readProp, read)
import Data.Argonaut.Core (Json)
import Control.Monad.Except (runExcept)
import Control.Monad.Eff.Exception (Error, error)


-- | Converts a Json to a Record
jsonToRecord :: forall a. IsForeign a => Json -> Either Error a
jsonToRecord json = either (\err -> Left (error $ show err)) Right $ runExcept (read (toForeign json))


-- | Converts an HObject to a Record
hObjToRecord :: forall a b. (IsForeign a, IsForeign b) => HObject a -> Either Error b
hObjToRecord hobj = either (\err -> Left (error $ show err)) Right $ runExcept (read (toForeign hobj))


-- | Gets the contructor name for the type
foreign import structName :: forall a . a -> String
