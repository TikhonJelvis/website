{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE NamedFieldPuns            #-}
{-# LANGUAGE OverloadedStrings         #-}

import           Prelude              hiding (id)

import           Control.Arrow        (arr, (&&&), (>>>), (>>^))
import           Control.Category     (id)
import           Control.Monad        (filterM)

import           Data.Char            (toLower)
import           Data.Function        (on)
import           Data.Functor         ((<$>))
import           Data.List            (isPrefixOf, nubBy)
import qualified Data.Map             as Map
import           Data.Monoid          ((<>))

import           System.Directory     (doesDirectoryExist, doesFileExist, getDirectoryContents)
import           System.FilePath      ((</>))
import qualified System.FilePath      as F

import qualified Text.HTML.TagSoup    as TS
import           Text.Pandoc.Shared   as P
import           Text.Printf          (printf)

import           Hakyll

(&)   = flip ($)
(<&>) = flip (<$>)

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
    compile $ do
      page     <- pandocCompiler
      template <- loadBody "templates/default.html"
      applyTemplate template (defaultContext <> title) page
      -- >>= pageRenderPandocWith defaultHakyllParserState pandocOptions
      -- >>= applyTemplateCompiler "templates/default.html"
      -- >>= relativizeCompiler

title = field "title" $ \ item -> do
  metadata <- getMetadata (itemIdentifier item)
  return $ case Map.lookup "title" metadata of
    Just title -> printf "%s | jelv.is" title
    Nothing    -> "jelv.is"

-- pandocOptions = defaultHakyllWriterOptions {
--   P.writerHTMLMathMethod = P.MathJax ""
-- }

removeDir = undefined
-- removeDir dir = customRoute $ remove .  identifierPath
--   where remove file = F.joinPath . filter (/= target) $ F.splitPath file
--         target = F.addTrailingPathSeparator dir

deep = undefined
-- deep pat = predicate $ \ i -> (matches (parseGlob pat) i) ||
--                               (matches (parseGlob $ "**/" ++ pat) i)

alternates = undefined
-- alternates pats = predicate . foldl go (const False) $ map deep pats
--   where go prevs pat = \ inp -> prevs inp || matches pat inp

-- addIncludes = getIncludes &&& id >>^ trySetField "imports" "" . setFields
--   where getIncludes = getIdentifier >>> unsafeCompiler (readIncludes . includePath)
--         includePath (Identifier {identifierPath}) =
--           F.dropFileName identifierPath </> "include"
--         readIncludes path =
--           do exists   <- doesDirectoryExist path
--              allFiles <- if exists
--                          then map (path </>) <$> getDirectoryContents path
--                          else return []
--              files    <- filterM doesFileExist allFiles
--              contents <- mapM readFile files
--              return . nubBy ((==) `on` fst) $ zip (key <$> files) contents
--         setFields (fields, page) = foldr (.) id (map (uncurry setField) fields) page
--         key = F.dropExtension . F.takeFileName

-- include file page = setField key (pageBody file) page
--   where key = F.dropExtension . F.takeFileName $ getField "path" file

        -- Fixed the weird self-closing tags issue:
-- relativizeCompiler = getRoute &&& id >>^ uncurry relativize
--   where relativize Nothing  = id
--         relativize (Just r) = fmap (fixUrls $ toSiteRoot r)
--         fixUrls root = mapUrls rel
--           where isRel x = "/" `isPrefixOf` x && not ("//" `isPrefixOf` x)
--                 rel x = if isRel x then root ++ x else x

-- mapUrls f = render . map tag . TS.parseTags
--   where tag (TS.TagOpen s a) = TS.TagOpen s $ map attr a
--         tag x                = x
--         attr (k, v) = (k, if k `elem` ["src", "href"] then f v else v)
--         render = TS.renderTagsOptions TS.renderOptions {
--           TS.optRawTag = (`elem` ["script", "style"]) . map toLower,
--           TS.optMinimize = (`elem` ["link", "meta", "img", "br"]) . map toLower
--         }
