# purescript-homogeneous-objects

[![Latest release](http://img.shields.io/bower/v/purescript-homogeneous-objects.svg)](https://github.com/Risto-Stevcev/purescript-homogeneous-objects/releases)

There's no way to get homogeneous objects in Purescript due to limitations in the type system. This library provides homogeneous JSON-like objects where the keys are all `String` types and the values all have the same type (homegeneous) that you specified. You can think of this as analagous to a typed array except for objects. It comes in three flavors:


## TupleTree

A TupleTree is just a tree representation of the `Tuple` type from [purescript-tuples][1] with a `String` as the first argument to represent the key in the key/value pair:

```haskell
data TupleTree a = Leaf a | Branch (Array (Tuple String (TupleTree a)))
``` 

You can construct one with the `mkTree` function:

```haskell
sampleTree :: TupleTree Int
sampleTree  = mkTree [ "foo" -= 3
                     , "bar" -< [ "baz" -= 4 ]
                     ]
```

The `-<` and `-=` combinators are syntactic sugar to make your tree representation easy to read. The `-<` combinator is used for `Branch` nodes, and `-=` is used for `Leaf` nodes.


## Json

You can construct a `Json` type from the [purescript-argonaut][2] package using the same `-<` and `-=` combinators but constructed with `hJson`. You must provide an instance of the `EncodeJson` typeclass for your homogeneous type if it doesn't already exist in order to be able to construct a `Json` type:

```haskell
sampleJson :: Json
sampleJson = hJson [ "foo" -= (Just 1)
                   , "bar" -< [ "baz" -= Nothing
                              , "qux" -< [ "norf" -= (Just 2) ]
                              ]
                   , "worble" -= (Just 3)
                   ]
```


## HObject

For situations where you don't want to use `Json` but you want to underlying representation to be a JSON object, you can use `HObject`. This is useful for situations where you have more complex types that strict `Json` doesn't support, but you still want a JSON representation internally for use in the FFI. It is constructed with `hObj`:

```haskell
data SampleType = StrType | NumType | BoolType

-- | This show instance makes (HObject SampleType) serializable
instance showSampleType :: Show SampleType where
   show StrType  = "[Fn String]"
   show NumType  = "[Fn Number]"
   show BoolType = "[Fn Boolean]"

sampleHObj :: HObject SampleType
sampleHObj = hObj [ "foo" -= StrType
                  , "bar" -< [ "baz" -= BoolType ]
                  , "qux" -= NumType
                  ]
```

[1]: https://github.com/purescript/purescript-tuples
[2]: https://github.com/purescript-contrib/purescript-argonaut-core
