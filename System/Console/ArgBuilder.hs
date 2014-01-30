{-# LANGUAGE FlexibleContexts, FlexibleInstances, GeneralizedNewtypeDeriving #-}

module System.Console.ArgBuilder
    ( Arg (..)
    , FlagType (..)
    , Token (..)
    , detokenize
    , shellize
    , ArgBuilderM
    , runArgBuilderM
    , ArgsM
    , runArgsM
    , ValueM
    , runValueM
    , PushArg (..)
    , (=:@)
    , (=:$)
    , Flag (..)
    )
    where

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Error

import Data.Default
import Data.List
import Data.Char

-- | Generalized argument builder monad
newtype ArgBuilderM env s w a = ArgBuilderM
    { unArgBuilderM :: ReaderT env (StateT s (WriterT w (Either String))) a }
    deriving (Monad, MonadReader env, MonadWriter w, MonadState s, MonadError String)

runArgBuilderM :: env -> s -> ArgBuilderM env s [w] a -> (Either String ((a, s), [w]))
runArgBuilderM env state m = runWriterT . flip runStateT state . flip runReaderT env $ unArgBuilderM m

-- | Wrapper for raw arguments
newtype Arg = Arg String

-- | Flag-argument binding type
data FlagType = Sticky         -- ^ i.e. -Wall
              | Equals         -- ^ i.e. --count=10
              | Separated      -- ^ i.e. --file foo.txt

-- | Argument builder token type, contains basic blocks
data Token = Flag String
           | Optional FlagType String (Maybe Arg)
           | Parameter FlagType String Arg
           | Argument Arg

-- | Flatten tokens to a list of strings, which is could be used to run process.
detokenize = concatMap expandToken
    where
      joinFlagArg :: FlagType -> String -> String -> [String]
      joinFlagArg Sticky flg arg = [flg ++ arg]
      joinFlagArg Equals flg arg = [flg ++ "=" ++ arg]
      joinFlagArg Separated flg arg = [flg, arg]

      expandToken :: Token -> [String]
      expandToken (Argument (Arg a)) = [a]
      expandToken (Flag f) = [f]
      expandToken (Optional ty f ma) = maybe [] (\(Arg a) -> joinFlagArg ty f a) ma
      expandToken (Parameter ty f (Arg a)) = joinFlagArg ty f a

-- | Flatten tokens to a string, which could be used in call "system" or in shell.
-- Outputs a string which does not allow any shell magic: wildcards, env variables, etc.
shellize = intercalate " " . map escape . detokenize
    where
      escape s | any (not . isNonEscapable) s = "'" ++ concatMap escSingleQuote s ++ "'"
               | otherwise = s
      escSingleQuote '\'' = "'\\''"
      escSingleQuote x = x:[]
      isNonEscapable c = any ($ c) [isAlphaNum, (`elem` "-_+=/:")]

type ArgsM env s a = ArgBuilderM env s [Token] a

runArgsM :: (Default s) =>
            env
         -> ArgBuilderM env s [Token] a
         -> Either String (a, [Token])
runArgsM env m = do
  ((result, _), tokens) <- runArgBuilderM env def m
  return (result, tokens)

type ValueM env s a = ArgBuilderM env s [Arg] a

runValueM :: env
          -> s
          -> ArgBuilderM env s [Arg] a
          -> Either String ((a, s), [Arg])
runValueM env st m = runArgBuilderM env st m

class PushArg w where
    arg :: (MonadWriter w m) => String -> m ()

instance PushArg [Arg] where
    arg s = tell [Arg s]

instance PushArg [Token] where
    arg s = tell [Argument $ Arg s]

infix 1 =:?
infix 1 =::

class Flag f where
    flag  :: f -> ArgsM env s ()
    (=:?) :: f -> Maybe String -> ArgsM env s ()
    (=::) :: f -> String -> ArgsM env s ()

instance Flag String where
    flag f       = tell [ Flag f ]
    (=:?) f marg = (Separated, f) =:? marg
    (=::) f arg  = (Separated, f) =:: arg

instance Flag (FlagType, String) where
    flag (ty, f)       = tell [ Flag f ]
    (=:?) (ty, f) marg = tell [Optional ty f (fmap Arg marg)]
    (=::) (ty, f) arg  = tell [Parameter ty f (Arg arg)]

liftError :: (MonadError e m) => Either e a -> m a
liftError (Left e) = throwError e
liftError (Right a) = return a

infix 1 =:@

(=:@) :: (Flag flag) => flag -> ValueM env s a -> ArgsM env s a
(=:@) f argm = do
  env <- ask
  st <- get
  ((r, st), args) <- liftError $ runValueM env st argm
  put st
  mapM_ (\(Arg a) -> f =:: a) args
  return r

infix 1 =:$

(=:$) :: (Flag flag) => flag -> ArgsM env s a -> ArgsM env s a
(=:$) f argm = do
  env <- ask
  st <- get
  ((r, _), args) <- liftError $ runArgBuilderM env st argm
  f =:: shellize args
  return r
