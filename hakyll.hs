{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

import           Prelude              hiding (id)

import           Control.Arrow        (arr, (&&&), (>>>), (>>^))
import           Control.Category     (id)
import           Control.Monad        (filterM)

import           Data.Char            (toLower)
import           Data.Function        (on)
import           Data.Functor         ((<$>))
import           Data.List            (isPrefixOf, nubBy)

import           System.Directory     (doesDirectoryExist, doesFileExist, getDirectoryContents)
import           System.FilePath      ((</>))
import qualified System.FilePath      as F

import qualified Text.HTML.TagSoup    as TS
import           Text.Pandoc.Shared   as P

import           Hakyll

main = hakyll $ do
  match "templates/*" $ do
    compile templateCompiler

  match (deep "misc/**") $ do
    route $ removeDir "misc"
    compile copyFileCompiler

  match (alternates ["img/**", "js/**", "images/**", "fonts/**", "*.html"]) $ do
    route   idRoute
    compile copyFileCompiler

  match (deep "css/**") $ do
    route   idRoute
    compile compressCssCompiler

  match (deep "*.md") $ do
    route   $ setExtension "html"
    compile $ readPageCompiler
      >>> addIncludes >>> addDefaultFields
      >>> arr (changeField "title" (++ " | jelv.is") . applySelf)
      >>> pageRenderPandocWith defaultHakyllParserState pandocOptions
      >>> applyTemplateCompiler "templates/default.html"
      >>> relativizeCompiler

pandocOptions = defaultHakyllWriterOptions {
  P.writerHTMLMathMethod = P.MathJax ""
}

removeDir dir = customRoute $ remove .  identifierPath
  where remove file = F.joinPath . filter (/= target) $ F.splitPath file
        target = F.addTrailingPathSeparator dir

deep pat = predicate $ \ i -> (matches (parseGlob pat) i) ||
                              (matches (parseGlob $ "**/" ++ pat) i)

alternates pats = predicate . foldl go (const False) $ map deep pats
  where go prevs pat = \ inp -> prevs inp || matches pat inp

addIncludes = getIncludes &&& id >>^ trySetField "imports" "" . setFields
  where getIncludes = getIdentifier >>> unsafeCompiler (readIncludes . includePath)
        includePath (Identifier {identifierPath}) =
          F.dropFileName identifierPath </> "include"
        readIncludes path =
          do exists   <- doesDirectoryExist path
             allFiles <- if exists
                         then map (path </>) <$> getDirectoryContents path
                         else return []
             files    <- filterM doesFileExist allFiles
             contents <- mapM readFile files
             return . nubBy ((==) `on` fst) $ zip (key <$> files) contents
        setFields (fields, page) = foldr (.) id (map (uncurry setField) fields) page
        key = F.dropExtension . F.takeFileName

include file page = setField key (pageBody file) page
  where key = F.dropExtension . F.takeFileName $ getField "path" file

        -- Fixed the weird self-closing tags issue:
relativizeCompiler = getRoute &&& id >>^ uncurry relativize
  where relativize Nothing  = id
        relativize (Just r) = fmap (fixUrls $ toSiteRoot r)
        fixUrls root = mapUrls rel
          where isRel x = "/" `isPrefixOf` x && not ("//" `isPrefixOf` x)
                rel x = if isRel x then root ++ x else x

mapUrls f = render . map tag . TS.parseTags
  where tag (TS.TagOpen s a) = TS.TagOpen s $ map attr a
        tag x                = x
        attr (k, v) = (k, if k `elem` ["src", "href"] then f v else v)
        render = TS.renderTagsOptions TS.renderOptions {
          TS.optRawTag = (`elem` ["script", "style"]) . map toLower,
          TS.optMinimize = (`elem` ["link", "meta", "img", "br"]) . map toLower
        }
