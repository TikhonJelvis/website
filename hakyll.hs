{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

import           Control.Arrow        (arr, second, (>>>))

import           System.Directory     (doesFileExist)
import qualified System.FilePath      as F

import           Hakyll

import           Hakyll.Core.Resource (Resource (..))

main = hakyll $ do
  match "templates/*" $ compile templateCompiler

  match (deep "misc/**") $ do
    route   upRoute
    compile copyFileCompiler

  match (deep "img/**") $ do
    route   idRoute
    compile copyFileCompiler

  match (deep "js/**") $ do
    route   idRoute
    compile copyFileCompiler

  match (deep "css/**") $ do
    route   idRoute
    compile compressCssCompiler

  match (deep "*.md") $ do
    route   $ setExtension "html"
    compile $ pageCompiler
      >>> headers
      >>> applyTemplateCompiler "templates/default.html"
      >>> relativizeUrlsCompiler

  match (deep "*.html") $ do
    route   idRoute
    compile copyFileCompiler

upRoute = customRoute $ up . identifierPath
  where up file = let path = F.splitPath file in F.joinPath $ init (init path) ++ [last path]

deep pat = predicate $ \ i -> (matches (parseGlob pat) i) || (matches (parseGlob $ "**/" ++ pat) i)

headers = split >>> setFieldA "includes" (unsafeCompiler readHead)
  where split = arr (\ x -> (x, x)) >>> second getIdentifier
        readHead (Identifier {identifierPath}) =
          let name = F.replaceFileName identifierPath "head" in
          do exists <- doesFileExist name
             if exists then readFile name else return ""