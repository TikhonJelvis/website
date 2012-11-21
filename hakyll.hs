{-# LANGUAGE OverloadedStrings, NamedFieldPuns #-}

import Control.Arrow ((>>>), arr, second)

import System.Directory (doesFileExist)
import System.FilePath (replaceExtension)

import Hakyll

import Hakyll.Core.Resource (Resource (..))

main = hakyll $ do
  match "templates/*" $ compile templateCompiler

  match (deep "img/*") $ do
    route   idRoute
    compile copyFileCompiler

  match (deep "js/*") $ do
    route   idRoute
    compile copyFileCompiler

  match (deep "css/*") $ do
    route   idRoute
    compile compressCssCompiler

  match (deep "*.md") $ do
    route   $ setExtension "html"
    compile $ pageCompiler
      >>> headers
      >>> applyTemplateCompiler "templates/default.html"
      >>> relativizeUrlsCompiler

deep pat = predicate $ \ i -> check pat i || check ("**/" ++ pat) i
  where check = matches . parseGlob 

headers = split >>> setFieldA "includes" (unsafeCompiler $ readHeader . includeFile)
  where split = arr (\ x -> (x, x)) >>> second getIdentifier
        readHeader path = do exists <- doesFileExist path
                             if exists then readFile path else return ""
        includeFile (Identifier {identifierPath}) = replaceExtension identifierPath "head"