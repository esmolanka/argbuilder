{-# LANGUAGE FlexibleContexts #-}

module Hadoop.ArgBuilder where

import System.Console.ArgBuilder
import System.FilePath

import Control.Applicative
import Control.Arrow

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Error

import Data.Char
import Data.Default
import Data.List
import Data.Maybe


data Env = Env { envScriptPath :: FilePath
               , envResourcePath :: FilePath
               }

data HadoopArgState = HadoopArgState { stOutput :: Maybe FilePath } deriving (Show)

instance Default HadoopArgState where
    def = HadoopArgState Nothing

type HadoopArgM a = ArgsM Env HadoopArgState a

alias :: FilePath -> FilePath -> FilePath
alias f a = let (path, fn) = splitFileName f
            in path </> (fn ++ "#" ++ a)

script :: (MonadReader Env m, MonadWriter w m, PushArg w) =>
          FilePath
       -> m ()
script f = do
  path <- asks envScriptPath
  arg (path </> f)

resource :: (MonadReader Env m, MonadWriter w m, PushArg w) =>
          FilePath
       -> m ()
resource f = do
  path <- asks envResourcePath
  arg (path </> f)


mapper :: FilePath -> HadoopArgM () -> HadoopArgM ()
mapper f argm = do
  "-mapper" =:$ do script f
                   argm

reducer :: FilePath -> HadoopArgM () -> HadoopArgM ()
reducer f argm = do
  path <- asks envScriptPath
  "-reducer" =:$ do script f
                    argm

inputs :: [FilePath] -> HadoopArgM ()
inputs paths = "-input" =:@ mapM_ arg paths

output path = do
  mprevPath <- gets stOutput
  case mprevPath of
    Just prevPath -> throwError $ "Multiple outputs are not allowed: was " ++ prevPath ++ ", inserting " ++ path
    Nothing -> return ()
  "-output" =:: path
  modify (\st -> st {stOutput = Just path} )

cacheFile filesm =
    "-cacheFile" =:@ censor (map addAlias) filesm
        where
          addAlias (Arg fn) =
              let (path, file) = splitFileName fn
                  newfile | '#' `elem` file = file
                          | otherwise = file ++ "#" ++ file
              in Arg $ path </> newfile

cmdenv name value = do
  unless (all (\c -> isAlphaNum c || c == '_') name) $
         throwError $ "Environment variable name in -cmdenv is malformed: " ++ show name

  unless (all (\c -> not $ (isSpace c) || (isControl c)) value) $
         throwError $ "Environment variable value in -cmdenv is malformed: " ++ name ++ "=" ++ show value

  "-cmdenv" =:: name ++ "=" ++ value

runHadoopArgs :: Env -> ArgsM Env HadoopArgState a -> Either String (HadoopArgState, [String])
runHadoopArgs env m = right (second detokenize) $ runArgsM env (m >> get)


testArgs x = do
  output "s3://somewhere/in/s3"
  mapper "AggregateMap" $ do
               when x $ flag "-xMPRelativeBuzz"
               flag "-B4"

  reducer "AggregateReduce" $ do
               flag "-B4"
               flag "Hello World"

  cacheFile $ do
               script "AggregateMap.sh"
               resource $ "directory/some.conf" `alias` "someothername.conf"

  cmdenv "BEGINDATE" "2013-01-01"

  (Sticky, "-W") =:@ do arg "foo"
                        arg "bar"
                        arg "baz"

testIt = runHadoopArgs (Env "/bin" "/etc") (testArgs True)

testIt' = right (second shellize) $ runArgsM (Env "/bin" "/etc") (testArgs True)
