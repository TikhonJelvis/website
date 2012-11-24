{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

import           Control.Arrow        (arr, second, (&&&), (>>>), (>>^))
import           Control.Monad        (filterM)

import           Data.Function        (on)
import           Data.Functor         ((<$>))
import           Data.List            (nubBy)

import           System.Directory     (doesFileExist, getDirectoryContents)
import           System.FilePath      ((</>))
import qualified System.FilePath      as F

import           Hakyll

import           Hakyll.Core.Resource (Resource (..))

main = hakyll $ do
  match "templates/*" $ do
    compile templateCompiler

  match (deep "misc/**") $ do
    route   upRoute
    compile copyFileCompiler

  match (alternates ["img/**", "js/**", "images/**", "*.html"]) $ do
    route   idRoute
    compile copyFileCompiler

  match (deep "css/**") $ do
    route   idRoute
    compile compressCssCompiler

  match (deep "*.md") $ do
    route   $ setExtension "html"
    compile $ readPageCompiler
      >>> addIncludes
      >>> arr applySelf
      >>> pageRenderPandoc
      >>> applyTemplateCompiler "templates/default.html"
      >>> relativizeUrlsCompiler

  match (deep "*.html") $ do
    route   idRoute
    compile copyFileCompiler

upRoute = customRoute $ up . identifierPath
  where up file = let path = F.splitPath file in
          F.joinPath $ init (init path) ++ [last path]

deep pat = predicate $ \ i -> (matches (parseGlob pat) i) ||
                              (matches (parseGlob $ "**/" ++ pat) i)

alternates pats = predicate . foldl go (const False) $ map deep pats
  where go prevs pat = \ inp -> prevs inp || matches pat inp

addIncludes = getIncludes &&& arr id >>^ setFields
  where getIncludes = getIdentifier >>> unsafeCompiler (readIncludes . includePath)
        includePath (Identifier {identifierPath}) =
          F.dropFileName identifierPath </> "include"
        readIncludes path =
          do allFiles <- map (path </>) <$> getDirectoryContents path
             files    <- filterM doesFileExist allFiles
             contents <- mapM readFile files
             return . nubBy ((==) `on` fst) $ zip (key <$> files) contents
        setFields (fields, page) = foldr (.) id (map (uncurry setField) fields) page
        key = F.dropExtension . F.takeFileName

include file page = setField key (pageBody file) page
  where key = F.dropExtension . F.takeFileName $ getField "path" file
