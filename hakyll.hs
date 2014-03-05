{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE NamedFieldPuns            #-}
{-# LANGUAGE OverloadedStrings         #-}

import           Data.String          (fromString)
import qualified Data.Map             as Map
import           Data.Monoid          ((<>))

import qualified System.FilePath      as F

import qualified Text.Pandoc.Options  as P
import           Text.Printf          (printf)

import           Hakyll

main = hakyll $ do
  match "templates/*" $ do
    compile templateCompiler

  match (deep "misc") $ do
    route $ removeDir "misc"
    compile copyFileCompiler

  match (alternates ["img/**", "js/**", "images/**", "fonts/**", "*.html"]) $ do
    route   idRoute
    compile copyFileCompiler

  match (deep "css") $ do
    route   idRoute
    compile compressCssCompiler

  match ("*.md" .||. "**/*.md") $ do
    route   $ setExtension "html"
    compile $
          pandocCompilerWith defaultHakyllReaderOptions pandocOptions
      >>= loadAndApplyTemplate "templates/default.html" (defaultContext <> title)
      >>= relativizeUrls

title = field "title" $ \ item -> do
  metadata <- getMetadata (itemIdentifier item)
  return $ case Map.lookup "title" metadata of
    Just title -> printf "%s | jelv.is" title
    Nothing    -> "jelv.is"

pandocOptions = defaultHakyllWriterOptions {
  P.writerHTMLMathMethod = P.MathJax ""
}

removeDir dir = customRoute $ remove . toFilePath
  where remove file = F.joinPath . filter (/= target) $ F.splitPath file
        target      = F.addTrailingPathSeparator dir

deep :: String -> Pattern
deep name = pat "%s/*" .||. pat "**/%s/*"
  where pat spec = fromString $ printf spec name
-- deep pat = predicate $ \ i -> (matches (parseGlob pat) i) ||
--                               (matches (parseGlob $ "**/" ++ pat) i)

alternates = foldr1 (.||.)
