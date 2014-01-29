{-# LANGUAGE FlexibleContexts, FlexibleInstances, GeneralizedNewtypeDeriving #-}
module System.Console.ArgBuilder where

import Control.Applicative
import Control.Arrow

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Error

import Data.Default
import Data.List
import Data.Char

newtype ArgBuilderM env s w a = ArgBuilderM
    { unArgBuilderM :: ReaderT env (StateT s (WriterT w (Either String))) a }
    deriving (Monad, MonadReader env, MonadWriter w, MonadState s, MonadError String)

runArgBuilderM :: env -> s -> ArgBuilderM env s [w] a -> (Either String ((a, s), [w]))
runArgBuilderM env state m = runWriterT . flip runStateT state . flip runReaderT env $ unArgBuilderM m

newtype Arg = Arg String
data FlagType = Sticky
              | Equals
              | Separated

joinFlagArg :: FlagType -> String -> String -> [String]
joinFlagArg Sticky flg arg = [flg ++ arg]
joinFlagArg Equals flg arg = [flg ++ "=" ++ arg]
joinFlagArg Separated flg arg = [flg, arg]

data Token = Flag String
           | Optional FlagType String (Maybe Arg)
           | Parameter FlagType String Arg
           | Argument Arg

detokenize = concatMap go
    where go :: Token -> [String]
          go (Argument (Arg a)) = [a]
          go (Flag f) = [f]
          go (Optional ty f ma) = maybe [] (\(Arg a) -> joinFlagArg ty f a) ma
          go (Parameter ty f (Arg a)) = joinFlagArg ty f a

shellize = intercalate " " . map escape . detokenize
    where escape s | any (not . isNonEscapable) s = "'" ++ concatMap escSingleQuote s ++ "'"
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

argument :: String -> ArgsM env s ()
argument a = tell [ Argument (Arg a) ]

flag :: String -> ArgsM env s ()
flag f = tell [Flag f]

infix 1 =:?
infix 1 =::

class Flag f where
    (=:?) :: f -> Maybe String -> ArgsM env s ()
    (=::) :: f -> String -> ArgsM env s ()

instance Flag String where
    (=:?) f marg = (Separated, f) =:? marg
    (=::) f arg  = (Separated, f) =:: arg

instance Flag (FlagType, String) where
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

class PushArg w where
    arg :: (MonadWriter w m) => String -> m ()

instance PushArg [Arg] where
    arg s = tell [Arg s]

instance PushArg [Token] where
    arg s = tell [Argument $ Arg s]
