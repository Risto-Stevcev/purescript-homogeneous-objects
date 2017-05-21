module Data.HObject.Record where

import Prelude (($), show)
import Data.Either (Either(..), either)
import Data.HObject (HObject)
import Data.Foreign (F, Foreign, toForeign)
import Data.Argonaut.Core (Json)
import Control.Monad.Except (runExcept)
import Control.Monad.Eff.Exception (Error, error)


-- | Converts a Json to a Record
jsonToRecord :: forall a. (Foreign -> F a) -> Json -> Either Error a
jsonToRecord read json = either (\err -> Left (error $ show err)) Right $ runExcept (read (toForeign json))


-- | Converts an HObject to a Record
hObjToRecord :: forall a b. (Foreign -> F b) -> HObject a -> Either Error b
hObjToRecord read hobj = either (\err -> Left (error $ show err)) Right $ runExcept (read (toForeign hobj))


-- | Gets the contructor name for the type
foreign import structName :: forall a . a -> String
