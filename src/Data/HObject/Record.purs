module Data.HObject.Record where

import Prelude (($), bind, pure, show)
import Data.Either (Either(..), either)
import Data.HObject (HObject)
import Data.Foreign (ForeignError, Foreign, toForeign)
import Data.Foreign.Class (class IsForeign, readProp, read)
import Data.Argonaut.Core (Json)
import Control.Monad.Eff.Exception (Error, error)


-- | Converts a Json to a Record
jsonToRecord :: forall a. IsForeign a => Json -> Either Error a
jsonToRecord json = either (\err -> Left (error $ show err)) Right $ read $ toForeign json


-- | Converts an HObject to a Record
hObjToRecord :: forall a b. (IsForeign a, IsForeign b) => HObject a -> Either Error b
hObjToRecord hobj = either (\err -> Left (error $ show err)) Right $ read $ toForeign hobj


-- | Gets the contructor name for the type
foreign import structName :: forall a . a -> String


-- | Reads the Foreign value of a unary kinded type
readType :: forall a b. IsForeign a => (a -> b) -> Foreign -> Either ForeignError b
readType = readType1

readType1 :: forall a b. IsForeign a => (a -> b) -> Foreign -> Either ForeignError b
readType1 f value = either Left (\r -> Right (f r)) (readProp "value0" value)


-- | Reads the Foreign value of a binary kinded type
readType2 :: forall a b c. (IsForeign a, IsForeign b) => (a -> b -> c) -> Foreign -> Either ForeignError c
readType2 f value = do
  value0 <- readProp "value0" value
  value1 <- readProp "value1" value
  pure $ f value0 value1


-- | Reads the Foreign value of a 3-ary kinded type
readType3 :: forall a b c d
           . (IsForeign a, IsForeign b, IsForeign c)
          => (a -> b -> c -> d)
          -> Foreign
          -> Either ForeignError d
readType3 f value = do
  value0 <- readProp "value0" value
  value1 <- readProp "value1" value
  value2 <- readProp "value2" value
  pure $ f value0 value1 value2


-- | Reads the Foreign value of a 4-ary kinded type
readType4 :: forall a b c d e
           . (IsForeign a, IsForeign b, IsForeign c, IsForeign d)
          => (a -> b -> c -> d -> e)
          -> Foreign
          -> Either ForeignError e
readType4 f value = do
  value0 <- readProp "value0" value
  value1 <- readProp "value1" value
  value2 <- readProp "value2" value
  value3 <- readProp "value3" value
  pure $ f value0 value1 value2 value3


-- | Reads the Foreign value of a 5-ary kinded type
readType5 :: forall a b c d e f
           . (IsForeign a, IsForeign b, IsForeign c, IsForeign d, IsForeign e)
          => (a -> b -> c -> d -> e -> f)
          -> Foreign
          -> Either ForeignError f
readType5 f value = do
  value0 <- readProp "value0" value
  value1 <- readProp "value1" value
  value2 <- readProp "value2" value
  value3 <- readProp "value3" value
  value4 <- readProp "value4" value
  pure $ f value0 value1 value2 value3 value4


-- | Reads the Foreign value of a 6-ary kinded type
readType6 :: forall a b c d e f g
           . (IsForeign a, IsForeign b, IsForeign c, IsForeign d, IsForeign e, IsForeign f)
          => (a -> b -> c -> d -> e -> f -> g)
          -> Foreign
          -> Either ForeignError g
readType6 f value = do
  value0 <- readProp "value0" value
  value1 <- readProp "value1" value
  value2 <- readProp "value2" value
  value3 <- readProp "value3" value
  value4 <- readProp "value4" value
  value5 <- readProp "value5" value
  pure $ f value0 value1 value2 value3 value4 value5


-- | Reads the Foreign value of a 7-ary kinded type
readType7 :: forall a b c d e f g h
           . (IsForeign a, IsForeign b, IsForeign c, IsForeign d, IsForeign e, IsForeign f, IsForeign g)
          => (a -> b -> c -> d -> e -> f -> g -> h)
          -> Foreign
          -> Either ForeignError h 
readType7 f value = do
  value0 <- readProp "value0" value
  value1 <- readProp "value1" value
  value2 <- readProp "value2" value
  value3 <- readProp "value3" value
  value4 <- readProp "value4" value
  value5 <- readProp "value5" value
  value6 <- readProp "value6" value
  pure $ f value0 value1 value2 value3 value4 value5 value6
