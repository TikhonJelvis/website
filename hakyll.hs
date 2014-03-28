#!/usr/bin/env runhaskell
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE NamedFieldPuns            #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE ViewPatterns              #-}

import           Data.Functor         ((<$>))
import qualified Data.List            as List
import           Data.Monoid          ((<>), mconcat)
import           Data.String          (fromString)

import qualified System.Directory     as Dir
import qualified System.FilePath      as Path
import           System.FilePath      ((</>))

import qualified Text.Pandoc.Options  as P
import           Text.Printf          (printf)

import           Hakyll

infixl 1 <&>
(<&>) :: Functor f => f a -> (a -> b) -> f b
(<&>) = flip (<$>)

main :: IO ()
main = hakyll $ do
  match "templates/*" $ do
    compile templateCompiler

  match ("blog/*/*.md" .||. "drafts/*/*.md") $ do
    route $ setExtension "html"
    compile $ getResourceString
      <&> runPandoc
      >>= saveSnapshot "content"
      >>= loadAndApplyTemplate "templates/blog-post.md" context
      >>= defaultPage

  match "blog/index.html" $ do
    route   $ setExtension "html"
    compile $ do
      posts <- loadAllSnapshots "blog/*/*.md" "content"
      let blogContext = listField "posts" postContext (return posts) <>
                        constField "title" "Blog" <>
                        context
      getResourceString
        >>= applyAsTemplate blogContext
        >>= loadAndApplyTemplate "templates/default.html" blogContext
        >>= relativizeUrls

  match (deep "misc") $ do
    route $ removeDir "misc"
    compile copyFileCompiler

  let supportFiles = alternates $ map deep ["img", "js", "fonts", "images"]
  match (supportFiles .||. "*.html" .||. "**/*.html") $ do
    route   idRoute
    compile copyFileCompiler

  match (deep "css") $ do
    route   idRoute
    compile compressCssCompiler
      
  match ("*.md" .||. "**/*.md") $ do
    route $ setExtension "html"
    compile $ getResourceString >>= defaultPage
 
defaultPage :: Item String -> Compiler (Item String)
defaultPage content = do
  includes <- setIncludes =<< getUnderlying
  applyAsTemplate includes content
    <&> runPandoc
    >>= loadAndApplyTemplate "templates/default.html" context
    >>= relativizeUrls

runPandoc :: Item String -> Item String
runPandoc = renderPandocWith defaultHakyllReaderOptions pandocOptions

postContext :: Context String
postContext = mapContext Path.takeDirectory (urlField "url")
           <> teaserField "teaser" "content"
           <> context

context :: Context String
context = defaultContext <> include "imports.html"

setIncludes :: Identifier -> Compiler (Context a)
setIncludes (Path.takeDirectory . toFilePath -> dir) = mconcat . map include <$> files
  where files = unsafeCompiler $ do
          exists <- Dir.doesDirectoryExist $ dir </> "include"
          if exists
            then clean <$> Dir.getDirectoryContents (dir </> "include")
            else return []
        clean = List.delete "." . List.delete ".." . filter ((/= '~') . last)

include :: String -> Context a
include file = field name $ \ item -> unsafeCompiler $ do
  printf "Including %s as %s\n" file name
  let dir = Path.takeDirectory . toFilePath $ itemIdentifier item
  exists <- Dir.doesFileExist $ dir </> "include" </> file
  if exists then readFile $ dir </> "include" </> file
            else return ""
  where name = Path.takeBaseName file

pandocOptions :: P.WriterOptions
pandocOptions = defaultHakyllWriterOptions {
  P.writerHTMLMathMethod = P.MathJax ""
}

removeDir :: String -> Routes
removeDir dir = customRoute $ remove . toFilePath
  where remove file = Path.joinPath . filter (/= target) $ Path.splitPath file
        target      = Path.addTrailingPathSeparator dir

deep :: String -> Pattern
deep name = pat "%s/**" .||. pat "**/%s/**"
  where pat spec = fromString $ printf spec name

alternates :: [Pattern] -> Pattern
alternates = foldr1 (.||.)
