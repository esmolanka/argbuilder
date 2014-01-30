argbuilder - command line arguments building DSL
==========

The library provides a monadic DSL to build command line arguments. It
is especially useful for building more specific DSLs.

For example, let's define something like Hadoop Streaming command line
arguments DSL:

```haskell

data Env = Env
    { envScriptPath :: FilePath
    , envResourcePath :: FilePath
    }

data HadoopArgState = HadoopArgState
    { stOutput :: Maybe FilePath
    }

type HadoopArgM w a = ArgBuilderM Env HadoopArgState w a

-- ... defining some combinators ...


-- Example function
exampleArgs :: Bool -> HadoopArgM Argument ()
exampleArgs noFooMode = do
  "-D" =:@ do arg ("stream.num.map.output.key.fields",    "4")
              arg ("mapred.text.key.partitioner.options", "-k2,4")
              arg ("stream.num.reduce.output.key.fields", "5")

  inputs ["s3://path/to/input"]
  output "s3://path/to/output"

  mapper "Map" $ do
    when noFooMode $ flag "--no-foo"
    (Equals, "--dictionary") =:: "file.txt"

  reducer "ReducerUsesFooBarBinary.sh" $ do
    flag "--enable-foo"
    (Sticky, "-a") =:@ do arg "Date"
                          arg "Time"
                          arg "Asset"

  cacheFile $ do
    script "FooBar"
    resource $ "directory/some-file.txt" `alias` "file.txt"

  cmdenv "BEGINDATE" "2013-01-01"
  cmdenv "ENDDATE"   "2013-01-01"
```
Outputs the following list of arguments

```
-D
stream.num.map.output.key.fields=4
-D
mapred.text.key.partitioner.options=-k2,4
-D
stream.num.reduce.output.key.fields=5
-input
s3://path/to/input
-output
s3://path/to/output
-mapper
/bin/Map --no-foo --dictionary=file.txt
-reducer
/bin/ReducerUsesFooBarBinary.sh --enable-foo -aDate -aTime -aAsset
-cacheFile
/bin/FooBar#FooBar
-cacheFile
/etc/directory/some-file.txt#file.txt
-cmdenv
BEGINDATE=2013-01-01
-cmdenv
ENDDATE=2013-01-01
```

