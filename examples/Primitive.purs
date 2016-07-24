module Data.HObject.Primitive.Example where

import Data.Tuple (Tuple)
import Data.HObject.Primitive (class Primitive, (/^\))
import Data.HObject.Primitive.Helpers (BoolOrInt, Prim)

foo :: Array (Tuple String Prim)
foo = [ "foo"  /^\ 2
      , "bar"  /^\ 3.14
      , "baz"  /^\ "qux"
      , "norf" /^\ true
      ]


bar :: Array (Tuple String BoolOrInt)
bar = [ "foo" /^\ true
      , "bar" /^\ 3
      ]


data Baz = Qux Int | Norf String

instance intToBaz :: Primitive Int Baz where
  mkPrim = Qux 

instance strToBaz :: Primitive String Baz where
  mkPrim = Norf

baz :: Array (Tuple String Baz)
baz = [ "foo" /^\ 42
      , "bar" /^\ "baz"
      ]
