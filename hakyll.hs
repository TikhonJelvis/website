{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE NamedFieldPuns            #-}
{-# LANGUAGE OverloadedStrings         #-}

import           Data.String          (fromString)
import qualified Data.Map             as Map
import           Data.Monoid          ((<>))

import qualified System.Directory     as Dir
import qualified System.FilePath      as Path

import qualified Text.Pandoc.Options  as P
import           Text.Printf          (printf)

import           Hakyll

main = hakyll $ do
  match "templates/*" $ do
    compile templateCompiler

  match (deep "misc") $ do
    route $ removeDir "misc"
    compile copyFileCompiler

  let supportFiles = alternates $ map deep ["img", "js", "images", "fonts"]
  match (supportFiles .||. "*.html" .||. "**/*.html") $ do
    route   idRoute
    compile copyFileCompiler

  match (deep "css") $ do
    route   idRoute
    compile compressCssCompiler

  match ("*.md" .||. "**/*.md") $ do
    route   $ setExtension "html"
    compile $
          pandocCompilerWith defaultHakyllReaderOptions pandocOptions
      >>= loadAndApplyTemplate "templates/default.html" (defaultContext <> title <> includes)
      >>= relativizeUrls

title = field "title" $ \ item -> do
  metadata <- getMetadata (itemIdentifier item)
  return $ case Map.lookup "title" metadata of
    Just title -> printf "%s | jelv.is" title
    Nothing    -> "jelv.is"

includes = field "imports" . const $ unsafeCompiler loadIncludes
  where loadIncludes = do exists <- Dir.doesFileExist "include/imports.html"
                          if exists then readFile "include/imports.html"
                                    else return ""
  

pandocOptions = defaultHakyllWriterOptions {
  P.writerHTMLMathMethod = P.MathJax ""
}

removeDir dir = customRoute $ remove . toFilePath
  where remove file = Path.joinPath . filter (/= target) $ Path.splitPath file
        target      = Path.addTrailingPathSeparator dir

deep :: String -> Pattern
deep name = pat "%s/*" .||. pat "**/%s/*"
  where pat spec = fromString $ printf spec name

alternates = foldr1 (.||.)
