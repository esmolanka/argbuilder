module Hadoop.ArgBuilder where

import System.Console.ArgBuilder
import System.FilePath

import Control.Arrow
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Error

import Data.Char
import Data.Default

data Env = Env
    { envScriptPath :: FilePath
    , envResourcePath :: FilePath
    }

data HadoopArgState = HadoopArgState
    { stOutput :: Maybe FilePath
    } deriving (Show)

instance Default HadoopArgState where
    def = HadoopArgState Nothing

type HadoopArgM w a = ArgBuilderM Env HadoopArgState w a

alias :: FilePath -> FilePath -> FilePath
alias f a = let (path, fn) = splitFileName f
            in path </> (fn ++ "#" ++ a)

script :: (ArgWrite w) => FilePath -> HadoopArgM w ()
script f = do
  path <- asks envScriptPath
  arg (path </> f)

resource :: (ArgWrite w) => FilePath -> HadoopArgM w ()
resource f = do
  path <- asks envResourcePath
  arg (path </> f)

mapper :: FilePath -> HadoopArgM Argument () -> HadoopArgM Argument ()
mapper f argm =
    "-mapper" =:$ do script f
                     argm

reducer :: FilePath -> HadoopArgM Argument () -> HadoopArgM Argument ()
reducer f argm =
    "-reducer" =:$ do script f
                      argm

inputs :: [FilePath] -> HadoopArgM Argument ()
inputs paths = "-input" =:@ mapM_ arg paths

output :: String -> HadoopArgM Argument ()
output path = do
  mprevPath <- gets stOutput
  case mprevPath of
    Just prevPath -> throwError $ "Multiple outputs are not allowed: was " ++ prevPath ++ ", inserting " ++ path
    Nothing -> return ()
  "-output" =:: path
  modify (\st -> st {stOutput = Just path} )

cacheFile :: HadoopArgM Arg a -> HadoopArgM Argument a
cacheFile filesm =
    "-cacheFile" =:@ censor (map addAlias) filesm
        where
          addAlias (Arg fn) =
              let (path, file) = splitFileName fn
                  newfile | '#' `elem` file = file
                          | otherwise = file ++ "#" ++ file
              in Arg $ path </> newfile

cmdenv :: [Char] -> [Char] -> HadoopArgM Argument ()
cmdenv name value = do
  unless (all (\c -> isAlphaNum c || c == '_') name) $
         throwError $ "Environment variable name in -cmdenv is malformed: " ++ show name

  unless (all (\c -> not $ (isSpace c) || (isControl c)) value) $
         throwError $ "Environment variable value in -cmdenv is malformed: " ++ name ++ "=" ++ show value

  "-cmdenv" =:: name ++ "=" ++ value

runHadoopArgs :: Env -> HadoopArgM Argument a -> Either String (HadoopArgState, [String])
runHadoopArgs env m = right (second detokenize) $ runArgsM env (m >> get)

{-
testArgs :: Bool -> ArgBuilderM Env HadoopArgState Argument ()
testArgs noFooMode = do
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

testIt :: Either String (HadoopArgState, [String])
testIt = runHadoopArgs (Env "s3://aggregators/scripts/version-3.1" "s3://aggregators/dictionaries/version-1.0") (testArgs True)

testIt2 :: Either String ((), String)
testIt2 = right (second shellize) $ runArgsM (Env "/bin" "/etc") (testArgs True)

testIt3 :: IO ExitCode
testIt3 = do
  system' "./echo-args" (Env "/bin" "/etc") (testArgs True)
  rawSystem' "./echo-args" (Env "/bin" "/etc") (testArgs True)
-}
