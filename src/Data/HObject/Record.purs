module Data.HObject.Record where

import Prelude (($), show)
import Data.Either (Either(..), either)
import Data.HObject (HObject)
import Data.Foreign (toForeign)
import Data.Foreign.Class (class Decode, decode)
import Data.Argonaut.Core (Json)
import Control.Monad.Except (runExcept)
import Control.Monad.Eff.Exception (Error, error)


-- | Converts a Json to a Record
jsonToRecord :: forall a. Decode a => Json -> Either Error a
jsonToRecord json = either (\err -> Left (error $ show err)) Right $ runExcept (decode (toForeign json))


-- | Converts an HObject to a Record
hObjToRecord :: forall a b. Decode b => HObject a -> Either Error b
hObjToRecord hobj = either (\err -> Left (error $ show err)) Right $ runExcept (decode (toForeign hobj))


-- | Gets the contructor name for the type
foreign import structName :: forall a . a -> String
