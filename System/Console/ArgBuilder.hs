{-# LANGUAGE ViewPatterns, OverlappingInstances, FlexibleContexts
  , FlexibleInstances, GeneralizedNewtypeDeriving #-}

module System.Console.ArgBuilder
    (
     -- * Types
     FlagType (..)
    , Argument (..)
    , Arg (..)
    , ArgBuilderM
    , runArgBuilderM
    , ArgsM
    , runArgsM
    , ValueM
    , runValueM
    -- * Argument classes
    , ArgWrite (..)
    , ArgLike
    -- * Combinators
    , Flag (..)
    , (=:@)
    , (=:$)
    -- * Converting utils
    , detokenize
    , shellize
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
    { unArgBuilderM :: ReaderT env (StateT s (WriterT [w] (Either String))) a }
    deriving (Monad, MonadReader env, MonadWriter [w], MonadState s, MonadError String)

-- | Runs building of arguments
runArgBuilderM :: env
               -> s
               -> ArgBuilderM env s w a
               -> (Either String ((a, s), [w]))
runArgBuilderM env state m = runWriterT . flip runStateT state . flip runReaderT env $ unArgBuilderM m

-- | Wrapper for raw arguments
newtype Arg = Arg { unArg :: String } deriving (Eq, Show)

-- | Flag-argument binding type
data FlagType = Sticky         -- ^ i.e. -Wall
              | Equals         -- ^ i.e. --count=10
              | Separated      -- ^ i.e. --file foo.txt
                deriving (Eq, Show)

-- | Argument builder token type
data Argument = Argument Arg
              | Flag String
              | Optional FlagType String (Maybe Arg)
              | Parameter FlagType String Arg
                deriving (Eq, Show)

-- | Flatten tokens to a list of arguments, which could be passed to 'System.Process.runProcess'.
detokenize = concatMap expandArgument
    where
      joinFlagArg :: FlagType -> String -> String -> [String]
      joinFlagArg Sticky flg arg = [flg ++ arg]
      joinFlagArg Equals flg arg = [flg ++ "=" ++ arg]
      joinFlagArg Separated flg arg = [flg, arg]

      expandArgument :: Argument -> [String]
      expandArgument (Argument (Arg a)) = [a]
      expandArgument (Flag f) = [f]
      expandArgument (Optional ty f ma) = maybe [] (\(Arg a) -> joinFlagArg ty f a) ma
      expandArgument (Parameter ty f (Arg a)) = joinFlagArg ty f a

-- | Flatten tokens to a string, which could be passed to
-- 'System.Process.system' or used in shell. Output string is escaped,
-- so no iterpolation is being made.
shellize = intercalate " " . map escape . detokenize
    where
      escape s | any (not . isNonEscapable) s = "'" ++ concatMap escSingleQuote s ++ "'"
               | otherwise = s
      escSingleQuote '\'' = "'\\''"
      escSingleQuote x = x:[]
      isNonEscapable c = any ($ c) [isAlphaNum, (`elem` "-_+=/:")]

-- | Shortcut type for argmument builder monad (high-level).
type ArgsM env s a = ArgBuilderM env s Argument a

runArgsM :: (Default s) =>
            env
         -> ArgBuilderM env s Argument a
         -> Either String (a, [Argument])
runArgsM env m = do
  ((result, _), tokens) <- runArgBuilderM env def m
  return (result, tokens)


-- | Shortcut to a monad which provides value binding for '=:@' combinator.
type ValueM env s a = ArgBuilderM env s Arg a

runValueM :: env
          -> s
          -> ArgBuilderM env s Arg a
          -> Either String ((a, s), [Arg])
runValueM env st m = runArgBuilderM env st m


-- | Helper class which allows to use 'arg' in both 'ArgsM' and 'ValueM' monads.
class ArgWrite w where
    arg :: (ArgLike a) => a -> ArgBuilderM e s w ()

instance ArgWrite Arg where
    arg a = tell [ printArg a ]

instance ArgWrite Argument where
    arg a = tell [ Argument $ printArg a ]


-- | Class of types which can be printed as a command line argument.
class ArgLike a where
    printArg :: a -> Arg

instance ArgLike String where
    printArg = Arg

instance ArgLike Int where
    printArg = Arg . show

instance ArgLike Integer where
    printArg = Arg . show

instance (ArgLike a, ArgLike b) => ArgLike (a, b) where
    printArg ( printArg -> Arg a
             , printArg -> Arg b
             ) = Arg $ a ++ "," ++ b

instance (ArgLike a, ArgLike b, ArgLike c) => ArgLike (a, b, c) where
    printArg ( printArg -> Arg a
             , printArg -> Arg b
             , printArg -> Arg c
             ) = Arg $ intercalate "," [a,b,c]

instance (ArgLike a) => ArgLike [a] where
    printArg as = Arg $ intercalate "," . map (unArg . printArg) $ as

infix 1 =:?
infix 1 =::

-- | Class of types which could be used as a flag. It is basically
-- strings or strings with annotation.
class Flag f where
    -- | Push a flag.
    flag  :: f -> ArgsM env s ()
    -- | Push an option with optional value. If value is missing, only the flag will be pushed.
    (=:?) :: f -> Maybe String -> ArgsM env s ()
    -- | Push an option with required value.
    (=::) :: f -> String -> ArgsM env s ()

instance Flag String where
    flag f       = tell [ Flag f ]
    (=:?) f marg = (Separated, f) =:? marg
    (=::) f arg  = (Separated, f) =:: arg

instance Flag (FlagType, String) where
    flag (ty, f)       = tell [ Flag f ]
    (=:?) (ty, f) marg = tell [ Optional ty f (fmap Arg marg) ]
    (=::) (ty, f) arg  = tell [ Parameter ty f (Arg arg) ]

liftError :: (MonadError e m) => Either e a -> m a
liftError (Left e) = throwError e
liftError (Right a) = return a

infix 1 =:@

-- | For each argument from value monad outputs a flag preceding that
-- argument.
--
-- For example:
-- > (Sticky, "-W") =:@ arg "all" >> arg "error"
-- gives us:
-- > -Wall -Werror
--
(=:@) :: (Flag flag) => flag -> ValueM env s a -> ArgsM env s a
(=:@) f argm = do
  env <- ask
  st <- get
  ((r, st), args) <- liftError $ runValueM env st argm
  put st
  mapM_ (\(Arg a) -> f =:: a) args
  return r

infix 1 =:$

-- | Embeds arguments and outputs this string escaped and with a flag
-- preceding it.
--
-- For example:
-- > (Equals, "--c-opts") =:$ do
-- >   flag "-O2"
-- >   "-march" =:: "i486"
-- gives us:
-- > --c-opts="-O2 -march i486"
--
(=:$) :: (Flag flag) => flag -> ArgsM env s a -> ArgsM env s a
(=:$) f argm = do
  env <- ask
  st <- get
  ((r, _), args) <- liftError $ runArgBuilderM env st argm
  f =:: shellize args
  return r
