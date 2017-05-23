module Test.Data.HObject.Record where

import Control.Monad.Aff (Aff)
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Exception (Error, message)
import Data.Argonaut.Core (Json)
import Data.Either (Either(..))
import Data.Foreign (F, Foreign, readInt)
import Data.Foreign.Index (readProp)
import Data.HObject (HObject, hObj, hJson, (-=), (-<))
import Data.HObject.Record (jsonToRecord)
import Prelude (class Show, Unit, bind, discard, pure, show, ($), (&&), (<>), (=<<), (==))
import Test.Unit (test, suite)
import Test.Unit.Assert (assert)
import Test.Unit.Console (TESTOUTPUT)
import Test.Unit.Main (runTest)



-- | Json to Record
-- | --------------
data Foo = Foo { foo :: Int, bar :: { baz :: Int, qux :: { norf :: Int } }, worble :: Int }

-- | Defines how to construct Foreign (internally a Json) into a Foo record
readFoo :: Foreign -> F Foo
readFoo obj = do
  foo <- readInt =<< readProp "foo" obj
  bar <- readProp "bar" obj
  baz <- readInt =<< readProp "baz" bar
  qux <- readProp "qux" bar
  norf <- readInt =<< readProp "norf" qux
  worble <- readInt =<< readProp "worble" obj
  pure $ Foo { foo: foo, bar: { baz: baz, qux: { norf: norf } }, worble: worble }



-- | Unary HObject to Record
-- | -----------------------
data UnaryType = I Int | S String | B Boolean
data UnaryHObj = UnaryHObj { foo :: UnaryType, bar :: { baz :: UnaryType }, qux :: UnaryType }

instance showUnaryType :: Show UnaryType where
  show (I a) = show a
  show (S a) = show a
  show (B a) = show a



-- | NAry HObject to Record
-- | ----------------------
data NAryType = IS Int String | ISB Int String Boolean
data NAryTypeHObj = NAryTypeHObj { foo :: NAryType, bar :: { baz :: NAryType }, qux :: NAryType }

instance showNAryType :: Show NAryType where
  show (IS a b) = show a <> show b
  show (ISB a b c) = show a <> show b <> show c





-- | Some sample types
-- | -----------------
sampleJson :: Json
sampleJson = hJson [ "foo" -= 1
                   , "bar" -< [ "baz" -= 1
                              , "qux" -< [ "norf" -= 2 ]
                              ]
                   , "worble" -= 3
                   ]

unaryHObj :: HObject UnaryType
unaryHObj = hObj [ "foo" -= I 2
                 , "bar" -< [ "baz" -= B true ]
                 , "qux" -= S "norf"
                 ]

nAryHObj :: HObject NAryType
nAryHObj = hObj [ "foo" -= IS 2 "hello"
                , "bar" -< [ "baz" -= ISB 3 "world" true ]
                , "qux" -= IS 4 "norf"
                ]



-- | Converting sample types to Records
-- | ----------------------------------
sampleJson' :: Either Error Foo
sampleJson' = jsonToRecord readFoo sampleJson


-- | Test helpers
-- | ------------
testFoo :: forall a. Either Error Foo -> Aff a Unit
testFoo (Left err) = assert (message err) false
testFoo (Right (Foo rec)) = do
  assert "Foo.foo"          $ (rec.foo == 1)
  assert "Foo.bar.baz"      $ (rec.bar.baz == 1)
  assert "Foo.bar.qux.norf" $ (rec.bar.qux.norf == 2)
  assert "Foo.worble"       $ (rec.worble == 3)


testUnaryType :: UnaryType -> UnaryType -> Boolean
testUnaryType (I a) (I b) = a == b
testUnaryType (S a) (S b) = a == b
testUnaryType (B a) (B b) = a == b
testUnaryType _     _     = false

testUnaryHObj :: forall a. Either Error UnaryHObj -> Aff a Unit
testUnaryHObj (Left err) = assert (message err) false
testUnaryHObj (Right (UnaryHObj rec)) = do
  assert "UnaryHObj.foo"     $ testUnaryType rec.foo     (I 2)
  assert "UnaryHObj.bar.baz" $ testUnaryType rec.bar.baz (B true)
  assert "UnaryHObj.qux"     $ testUnaryType rec.qux     (S "norf")


testNAryType :: NAryType -> NAryType -> Boolean
testNAryType (IS a b)    (IS a' b')     = a == a' && b == b'
testNAryType (ISB a b c) (ISB a' b' c') = a == a' && b == b' && c == c'
testNAryType _     _     = false

testNaryHObj :: forall a. Either Error NAryTypeHObj -> Aff a Unit
testNaryHObj (Left err) = assert (message err) false
testNaryHObj (Right (NAryTypeHObj rec)) = do
  assert "SampleHObj2.foo"     $ testNAryType rec.foo     (IS 2 "hello")
  assert "SampleHObj2.bar.baz" $ testNAryType rec.bar.baz (ISB 3 "world" true)
  assert "SampleHObj2.qux"     $ testNAryType rec.qux     (IS 4 "norf")



main :: Eff ( avar :: AVAR, console :: CONSOLE, testOutput :: TESTOUTPUT ) Unit
main = runTest do
  suite "Data.HObject.Record" do
    test "jsonToRecord" do
      assert "sampleJson" $ (show sampleJson) == "{\"foo\":1,\"bar\":{\"baz\":1,\"qux\":{\"norf\":2}},\"worble\":3}"
      testFoo sampleJson'
