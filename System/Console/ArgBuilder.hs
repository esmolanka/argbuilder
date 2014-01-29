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

newtype ArgBuilderM env s w a = ArgBuilderM
    { unArgBuilderM :: ReaderT env (StateT s (WriterT w (Either String))) a }
    deriving (Monad, MonadReader env, MonadWriter w, MonadState s, MonadError String)

runArgBuilderM :: env -> s -> ArgBuilderM env s [w] a -> (Either String ((a, s), [w]))
runArgBuilderM env state m = runWriterT . flip runStateT state . flip runReaderT env $ unArgBuilderM m

newtype Arg = Arg String
data Token = Argument Arg | Flag String | Optional String (Maybe Arg) | Parameter String Arg

detokenize = concatMap go
    where go :: Token -> [String]
          go (Argument (Arg a)) = [a]
          go (Flag f) = [f]
          go (Optional f ma) = f : maybe [] (\(Arg a) -> [a]) ma
          go (Parameter f (Arg a)) = [f,a]

type ArgsM env s a = ArgBuilderM env s [Token] a

runArgsM :: (Default s) =>
            env
         -> ArgBuilderM env s [Token] a
         -> Either String (a, [String])
runArgsM env m = do
  ((result, _), tokens) <- runArgBuilderM env def m
  return (result, detokenize tokens)

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

(=:?) :: String -> Maybe String -> ArgsM env s ()
(=:?) f marg = tell [Optional f (fmap Arg marg)]

infix 1 =::

(=::) :: String -> String -> ArgsM env s ()
(=::) f arg = tell [Parameter f (Arg arg)]

liftError :: (MonadError e m) => Either e a -> m a
liftError (Left e) = throwError e
liftError (Right a) = return a

infix 1 =:@

(=:@) :: String -> ValueM env s a -> ArgsM env s a
(=:@) f argm = do
  env <- ask
  st <- get
  ((r, st), args) <- liftError $ runValueM env st argm
  put st
  mapM_ (\(Arg a) -> f =:: a) args
  return r

infix 1 =:$

(=:$) :: String -> ArgsM env s a -> ArgsM env s a
(=:$) f argm = do
  env <- ask
  st <- get
  ((r, _), args) <- liftError $ runArgBuilderM env st argm
  f =:: (intercalate " " . detokenize $ args)
  return r

class PushArg w where
    arg :: (MonadWriter w m) => String -> m ()

instance PushArg [Arg] where
    arg s = tell [Arg s]

instance PushArg [Token] where
    arg s = tell [Argument $ Arg s]

embed :: (MonadState s m, MonadWriter w m, Monoid w) => ((w -> w) -> s -> s) -> m a -> m a
embed acc m = do
  (a, tokens) <- censor (const mempty) . listen $ m
  modify (acc (mappend tokens))
  return a
