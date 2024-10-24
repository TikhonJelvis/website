{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE NamedFieldPuns            #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE ViewPatterns              #-}

module Main where

import           Control.Monad          (forM, mapM, (<=<))

import qualified Data.Char              as Char
import           Data.Functor           ((<$>))
import qualified Data.List              as List
import           Data.Monoid            (mconcat, (<>))
import           Data.String            (fromString)

import qualified System.Directory       as Dir
import           System.FilePath        ((</>))
import qualified System.FilePath        as Path

import           Text.HTML.TagSoup      (Tag (..))
import           Text.Pandoc.Definition (Block (..), Pandoc)
import qualified Text.Pandoc.Generic    as Pandoc
import qualified Text.Pandoc.Options    as P
import           Text.Printf            (printf)

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
      >>= saveSnapshot "blurb"
      >>= loadAndApplyTemplate "templates/blog-post.md" context
      >>= defaultPage

  match "blog/index.html" $ do
    route   $ setExtension "html"
    compile $ do
      let blogContext =
            listField "posts" postContext blogBlurbs <>
            constField "title" "Blog" <>
            context
      getResourceString
        >>= applyAsTemplate blogContext
        >>= loadAndApplyTemplate "templates/default.html" blogContext
        >>= relativizeUrls

  create ["blog/atom.xml"] $ blogFeed renderAtom
  create ["blog/rss.xml"]  $ blogFeed renderRss

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

-- | Render an Atom or RSS feed for my blog.
blogFeed :: ( FeedConfiguration ->
              Context String    ->
              [Item String]     ->
              Compiler (Item String)
            )
         -> Rules ()
blogFeed render = do
  route idRoute
  compile $ render config feedContext =<< blogBlurbs
  where feedContext = postContext <> bodyField "description"
        config = FeedConfiguration
          { feedTitle       = "blog | jelv.is" -- TODO: come up with better title!
          , feedDescription =
            "My technical blog containing articles about programming languages, \
            \functional programming and general CS."
          , feedAuthorName  = "Tikhon Jelvis"
          , feedAuthorEmail = "tikhon@jelv.is"
          , feedRoot        = "http://jelv.is"
          }

blogBlurbs :: Compiler [Item String]
blogBlurbs = do
  blurbs <- recentFirst =<< loadAllSnapshots "blog/*/*.md" "blurb"
  forM blurbs $ \ blurb ->
    adjustUrls <$> runPandoc blurb
  where adjustUrls item = do
          let path = toFilePath (itemIdentifier item)
              base = "/" </> Path.takeDirectory path
          withUrls (toAbsolute base) <$> item

        toAbsolute base path
          | Path.isRelative path = base </> path
          | otherwise            = path

defaultPage :: Item String -> Compiler (Item String)
defaultPage content = do
  includes <- setIncludes =<< getUnderlying
  applyAsTemplate includes content
    >>= runPandoc
    >>= loadAndApplyTemplate "templates/default.html" context
    >>= relativizeUrls

runPandoc :: Item String -> Compiler (Item String)
runPandoc = titleToAlt <=< pandoc
  where pandoc item = writePandocWith writerOptions
                  <$> (fmap pandocFilters <$> readPandocWith readerOptions item)

        pandocFilters = ghciCodeBlocks

        writerOptions = defaultHakyllWriterOptions
         { P.writerHTMLMathMethod = P.MathJax ""
         , P.writerExtensions     = exts
         }

        readerOptions = defaultHakyllReaderOptions
          { P.readerExtensions = exts }

        exts = P.enableExtension P.Ext_tex_math_single_backslash $
               P.enableExtension P.Ext_all_symbols_escapable $
               P.pandocExtensions

-- | Have custom processing for @ghci@ code blocks as part of pandoc
-- compilation.
ghciCodeBlocks :: Pandoc -> Pandoc
ghciCodeBlocks = Pandoc.topDown renderBlock
  where renderBlock :: Block -> Block
        renderBlock block@(CodeBlock (_, classes, _) contents)
          | "ghci" `elem` classes =
            RawBlock "html" $ ghciHtml contents
          | otherwise             = block
        renderBlock block = block

        ghciHtml :: String -> String
        ghciHtml contents = "<pre class='ghci'><code>" <> wrap contents <> "</code></pre>"

        prompt = escape "λ>"

        wrap = trimNewlines' . unlines . map (highlight . escape) . lines
        highlight line
          | List.isPrefixOf prompt line = wrapSpan "ghci-input" $ highlightPrompt line
          | all Char.isSpace line       = line
          | otherwise                   = wrapSpan "ghci-output" line
        wrapSpan class_ line =
          "<span class='" <> class_ <> "'>" <> line <> "</span>"
        highlightPrompt line =
          "<span class='ghci-prompt'>" <> prompt <> "</span>" <> drop (length prompt) line
        trimNewlines' text = trimNewlines text
        trimNewlines "" = ""
        trimNewlines text
          | head text == '\n' && last text == '\n' = tail (init text)
          | head text == '\n'                      = tail text
          | last text == '\n'                      = init text
          | otherwise                              = text

        escape :: String -> String
        escape = concatMap $ \case
          '<' -> "&lt;"
          '>' -> "&gt;"
          x   -> [x]

-- | Set the @alt@ of each @img@ tag to the text in its @title@ and
-- remove the @title@ attribute altogether.
--
-- Pandoc doesn't support specifying alt tags and captions separately,
-- but it does support specifying an img tag's title. This hack lets
-- me repurpose the title functionality to specify alt tags instead.
titleToAlt :: Item String -> Compiler (Item String)
titleToAlt item = pure $ withTags fixImg <$> item
  where fixImg = \case
          TagOpen "img" attributes -> TagOpen "img" $ swapTitle attributes
          other                    -> other

        swapTitle attributes = ("alt", titleText) : clean attributes
          where clean = filter $ not . oneOf ["alt", "title"]
                titleText = case List.find (oneOf ["title"]) attributes of
                  Just ("title", titleText) -> titleText
                  _                         -> ""
                oneOf atts (att, _) = att `elem` atts

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
