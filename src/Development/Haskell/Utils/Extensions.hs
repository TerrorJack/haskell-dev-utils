{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE GADTs #-}

module Development.Haskell.Utils.Extensions where

import Control.Applicative
import Control.Concurrent.Async
import Control.Exception (throwIO)
import Data.Functor
import qualified Data.HashSet as HashSet
import Data.List
import qualified Data.Text.Lazy.IO as Text
import System.Directory.Extra
import System.FilePath
import Text.Megaparsec.Char
import Text.Megaparsec.Combinator
import Text.Megaparsec.Error
import Text.Megaparsec.Prim

type Extension = String

type Extensions = HashSet.HashSet Extension

symbol :: (MonadParsec e s m, Token s ~ Char) => String -> m ()
symbol s = do
  skipMany spaceChar
  void $ string s

extension :: (MonadParsec e s m, Token s ~ Char) => m Extension
extension = do
  symbol "{-"
  symbol "#"
  skipMany spaceChar
  void $ string' "LANGUAGE"
  skipSome spaceChar
  ext <- some letterChar
  symbol "#"
  symbol "-}"
  pure ext

extensions :: (MonadParsec e s m, Token s ~ Char) => m Extensions
extensions = try l <|> r
  where
    l = do
      ext <- extension
      exts <- extensions
      pure $ HashSet.insert ext exts
    r = pure HashSet.empty

fileExtensions :: FilePath -> IO Extensions
fileExtensions p = do
  src <- Text.readFile p
  case runParser extensions p src of
    Left err -> throwIO (err :: ParseError Char Dec)
    Right exts -> pure exts

hsFiles :: FilePath -> IO [FilePath]
hsFiles bp = do
  ps <- listFilesRecursive bp
  pure [p | p <- ps, takeExtension p == ".hs"]

allExtensions :: FilePath -> IO [Extension]
allExtensions p = do
  ps <- hsFiles p
  exts_list <- forConcurrently ps fileExtensions
  pure $ sort $ HashSet.toList $ HashSet.unions exts_list
