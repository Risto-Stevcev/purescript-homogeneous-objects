module Test.Data.HObject where

import Prelude (class Show, Unit, show, bind, ($), (==))
import Data.HObject (HObject, TupleTree, hObj, hJson, mkTree, hObjToJson, (-=), (-<))
import Data.Maybe (Maybe(..))
import Data.Argonaut.Core (Json, fromString)
import Data.Argonaut.Encode.Class (class EncodeJson)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Test.Unit (test, suite)
import Test.Unit.Main (runTest)
import Test.Unit.Assert (assert)
import Test.Unit.Console (TESTOUTPUT)


data SampleType = StrType | NumType | BoolType

-- | This show instance makes (HObject SampleType) serializable
instance showSampleType :: Show SampleType where
   show StrType  = "[Fn String]"
   show NumType  = "[Fn Number]"
   show BoolType = "[Fn Boolean]"

-- | This encode instance enables the convertion to Json
instance encodeSampleType :: EncodeJson SampleType where
   encodeJson StrType  = fromString "String"
   encodeJson NumType  = fromString "Number"
   encodeJson BoolType = fromString "Boolean"


sampleTree :: TupleTree Int
sampleTree  = mkTree [ "foo" -= 3
                     , "bar" -< [ "baz" -= 4 ]
                     ]

sampleJson1 :: Json
sampleJson1 = hJson [ "foo" -= 1
                    , "bar" -< [ "baz" -= 1
                               , "qux" -< [ "norf" -= 2 ]
                               ]
                    , "worble" -= 3
                    ]

sampleJson2 :: Json
sampleJson2 = hJson [ "foo" -= (Just 3)
                    , "bar" -= (Just 4)
                    , "qux" -= Nothing
                    ]

sampleJson3 :: Json
sampleJson3 = hJson [ "foo" -= StrType
                    , "bar" -< [ "baz" -= BoolType ]
                    , "qux" -= NumType
                    ]

sampleHObj :: HObject SampleType
sampleHObj = hObj [ "foo" -= StrType
                  , "bar" -< [ "baz" -= BoolType ]
                  , "qux" -= NumType
                  ]



main :: Eff ( console :: CONSOLE, testOutput :: TESTOUTPUT ) Unit
main = runTest do
  suite "Data.HObject" do
    test "mkTree" do
      assert "sampleTree" $ (show sampleTree) == "[(Tuple \"foo\" 3),(Tuple \"bar\" [(Tuple \"baz\" 4)])]"
    test "hJson" do
      assert "sampleJson1" $ (show sampleJson1) == "{\"foo\":1,\"bar\":{\"baz\":1,\"qux\":{\"norf\":2}},\"worble\":3}"
      assert "sampleJson2" $ (show sampleJson2) == "{\"foo\":3,\"bar\":4,\"qux\":null}"
      assert "sampleJson3" $ (show sampleJson3) == "{\"foo\":\"String\",\"bar\":{\"baz\":\"Boolean\"},\"qux\":\"Number\"}"
    test "hObj" $
      assert "sampleHObj" $ (show sampleHObj) == "{ foo: [Fn String], { bar: { baz: [Fn Boolean] } }, qux: [Fn Number] }"
    test "hObjToJson" $
      assert "hObjToJson" $ (show sampleJson3) == (show $ hObjToJson sampleHObj)
