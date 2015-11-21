#!/usr/bin/env runhaskell
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE NamedFieldPuns            #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE ViewPatterns              #-}

import           Control.Monad        (mapM)

import           Data.Char            (toUpper)
import           Data.Functor         ((<$>))
import qualified Data.List            as List
import           Data.Monoid          ((<>), mconcat)
import qualified Data.Set             as Set
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

  match (deep "misc") $ do
    route $ removeDir "misc"
    compile copyFileCompiler

  match ("blog/*/*.md" .||. "drafts/*/*.md") $ do
    route $ setExtension "html"
    compile $ getResourceString
      >>= saveSnapshot "content"
      >>= loadAndApplyTemplate "templates/blog-post.md" context
      >>= defaultPage

  match "blog/index.html" $ do
    route   $ setExtension "html"
    compile $ do
      content <- recentFirst =<< loadAllSnapshots "blog/*/*.md" "content"
      posts   <- mapM runPandoc content
      let blogContext = listField "posts" postContext (return posts) <>
                        constField "title" "Blog" <>
                        context
      getResourceString
        >>= applyAsTemplate blogContext
        >>= loadAndApplyTemplate "templates/default.html" blogContext
        >>= relativizeUrls

  let feed render = do
        route idRoute
        compile $ do
          let feedContext = postContext <> bodyField "description"
          posts    <- recentFirst =<< loadAllSnapshots "blog/*/*.md" "content"
          pandoced <- mapM runPandoc posts
          render blogFeedConfig feedContext pandoced

  create ["blog/atom.xml"] $ feed renderAtom
  create ["blog/rss.xml"]  $ feed renderRss

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
 
blogFeedConfig :: FeedConfiguration
blogFeedConfig = FeedConfiguration
    { feedTitle       = "blog | jelv.is" -- TODO: come up with better title!
    , feedDescription = "My technical blog containing articles about programming languages, functional programming and general CS."
    , feedAuthorName  = "Tikhon Jelvis"
    , feedAuthorEmail = "tikhon@jelv.is"
    , feedRoot        = "http://jelv.is"
    }

defaultPage :: Item String -> Compiler (Item String)
defaultPage content = do
  includes <- setIncludes =<< getUnderlying
  applyAsTemplate includes content
    >>= runPandoc
    >>= loadAndApplyTemplate "templates/default.html" context
    >>= relativizeUrls

runPandoc :: Item String -> Compiler (Item String)
runPandoc = renderPandocWith readerOptions writerOptions
  where writerOptions = defaultHakyllWriterOptions
         { P.writerHTMLMathMethod = P.MathJax ""
         , P.writerExtensions     = exts
         }

        readerOptions = defaultHakyllReaderOptions
          { P.readerExtensions = exts }

        exts = Set.insert P.Ext_tex_math_single_backslash $
               Set.insert P.Ext_all_symbols_escapable $
               P.pandocExtensions

postContext :: Context String
postContext = mapContext Path.takeDirectory (urlField "url")
           <> teaser
           <> context
  where teaser = field "teaser" $ \ item ->
          let name = show $ itemIdentifier item in
          case needlePrefix "<!--more-->" $ itemBody item of
            Nothing   -> error $ printf "No teaser defined for %s!" name
            Just body -> return body

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

removeDir :: String -> Routes
removeDir dir = customRoute $ remove . toFilePath
  where remove file = Path.joinPath . filter (/= target) $ Path.splitPath file
        target      = Path.addTrailingPathSeparator dir

deep :: String -> Pattern
deep name = pat "%s/**" .||. pat "**/%s/**"
  where pat spec = fromString $ printf spec name

alternates :: [Pattern] -> Pattern
alternates = foldr1 (.||.)
