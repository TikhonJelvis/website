{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE NamedFieldPuns            #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE PatternGuards             #-}
{-# LANGUAGE ViewPatterns              #-}

module Main where

import           Control.Monad          (forM, mapM, (<=<))

import qualified Data.Char              as Char
import qualified Data.Foldable          as Foldable
import           Data.Functor           ((<$>))
import qualified Data.List              as List
import           Data.Monoid            (mconcat, (<>))
import           Data.String            (fromString)
import           Data.Text              (Text)
import qualified Data.Text              as Text

import qualified System.Directory       as Dir
import qualified System.FilePath        as Path
import           System.FilePath        ((</>))

import           Text.HTML.TagSoup      (Tag (..))
import           Text.Pandoc.Definition (Block (..), Pandoc)
import qualified Text.Pandoc.Generic    as Pandoc
import qualified Text.Pandoc.Options    as P
import qualified Text.Pandoc.Walk       as Pandoc
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
      >>= blogPost

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
  metas <- getMetadataField (itemIdentifier content) "metas"
  applyAsTemplate includes content
    >>= runPandoc
    >>= loadAndApplyTemplate "templates/default.html" context
    >>= relativizeUrls

blogPost :: Item String -> Compiler (Item String)
blogPost content = do
  includes <- setIncludes =<< getUnderlying

  title <- fmap Text.pack <$> getMetadataField (itemIdentifier content) "title"
  description <- fmap Text.pack <$> getMetadataField (itemIdentifier content) "description"
  image <- fmap Text.pack <$> getMetadataField (itemIdentifier content) "image"
  let metaTags = Text.unlines [meta' Title title, meta' Description description, meta' Image image]
      postContext = constField "metas" (Text.unpack metaTags) <> context

  applyAsTemplate includes content
    >>= runPandoc
    >>= loadAndApplyTemplate "templates/default.html" postContext
    >>= relativizeUrls

runPandoc :: Item String -> Compiler (Item String)
runPandoc = imageLinks <=< titleToAlt <=< pandoc
  where pandoc item = writePandocWith writerOptions
                  <$> (fmap pandocFilters <$> readPandocWith readerOptions item)

        pandocFilters = pullQuotes . ghciCodeBlocks

        writerOptions = defaultHakyllWriterOptions
         { P.writerHTMLMathMethod = P.MathJax ""
         , P.writerExtensions     = exts
         }

        readerOptions = defaultHakyllReaderOptions
          { P.readerExtensions = exts }

        exts = P.enableExtension P.Ext_tex_math_single_backslash $
               P.enableExtension P.Ext_all_symbols_escapable $
               P.enableExtension P.Ext_inline_notes $
               P.enableExtension P.Ext_attributes $
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

        ghciHtml :: Text -> Text
        ghciHtml contents = "<pre class='ghci'><code>" <> wrap contents <> "</code></pre>"

        prompt = escape "λ>"

        wrap = Text.strip . Text.unlines . map (highlight . escape) . Text.lines
        highlight line
          | Text.isPrefixOf prompt line = wrapSpan "ghci-input" $ highlightPrompt line
          | Text.all Char.isSpace line  = line
          | otherwise                   = wrapSpan "ghci-output" line
        wrapSpan class_ line =
          tag "span" [class_] [] <> line <> "</span>"
        highlightPrompt line =
          let dropped = Text.drop (Text.length prompt) line in
          "<span class='ghci-prompt'>" <> prompt <> "</span>" <> dropped

        escape = Text.pack . escape' . Text.unpack
        escape' = concatMap $ \case
          '<' -> "&lt;"
          '>' -> "&gt;"
          x   -> [x]


-- | Turn div.pull-quote blocks elements into aside.pull-quote.
--
-- Semantic HTML tags are great!
pullQuotes :: Pandoc -> Pandoc
pullQuotes = Pandoc.walk $ concatMap renderBlock
  where renderBlock :: Block -> [Block]
        renderBlock block@(Div (_, classes, attributes) contents)
          | "pull-quote" `elem` classes =
            [ RawBlock "html" $ tag "aside" classes attributes ] <>
            contents <>
            [ RawBlock "html" "</aside>" ]
          | otherwise                   = [block]
        renderBlock block = [block]

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


-- | Make images inside figures into links to the expanded version of the image.
imageLinks :: Item String -> Compiler (Item String)
imageLinks item = pure $ withTagList (concatMap wrapImg) <$> item
  where wrapImg :: Tag String -> [Tag String]
        wrapImg = \case
          TagOpen "img" attributes
            | Just src <- getSrc attributes ->
              [ TagOpen "a" [("href", src), ("class", "image-enlarge")]
              , TagOpen "img" attributes
              , TagClose "a"
              ]
          other                    -> [other]

        getSrc = fmap snd . Foldable.find (\ (attr, _) -> attr == "src")

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
context = defaultContext <> include "imports.html" <> constField "metas" ""

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

-- * HTML Functions

-- | Generate an opening tag with the given CSS classes and
-- attributes.
--
-- >>> tag "div" ["foo", "bar"] [("id", "baz"), ("data-foo", "foo")]
-- "<div class='foo bar' id='baz' data-foo='foo'>"
tag :: Text
    -- ^ tag name
    -> [Text]
    -- ^ classes
    -> [(Text, Text)]
    -- ^ attributes
    -> Text
tag tagName [] [] = "<" <> tagName <> ">"
tag tagName [] attrs =
  let attributes = Text.unwords [attr <> "='" <> escapeChars val <> "'" | (attr, val) <- attrs]
  in "<" <> tagName <> " " <> attributes <> ">"
tag tagName classes attrs =
  tag tagName [] (("class", Text.unwords classes) : attrs)

-- | All the specific types of meta tags I currently support
-- generating.
data Meta = Title | Description | Image
  deriving (Show, Eq, Ord, Enum, Bounded)

-- | Each type of meta tag I support can generate multiple actual meta
-- tags in different style (twitter, open-graph (og), standardized, etc).
metaTags :: Meta -> [Text -> Text]
metaTags = \case
  Title ->
    [wrap ("property", "og:title"), wrap ("name", "twitter:title")]
  Description ->
    [wrap ("name", "description"), wrap ("property", "og:description"), wrap ("name", "twitter:description")]
  Image ->
    [wrap ("property", "og:image"), wrap ("name", "twitter:image")]
  where wrap attribute content = tag "meta" [] [("content", content), attribute]

-- | Generate the given type of meta tag.
--
-- This hardcodes some logic to handle standard, og and twitter meta
-- tags as appropriate. I will probably add other site-specific tags
-- down the line.
meta :: Meta
     -- ^ type of meta tag to generate
     -> Text
     -- ^ content
     -> Text
meta type_ content = Text.unlines [metaTag content | metaTag <- metaTags type_]

-- | Generate an optional meta tag. If the value is 'Nothing', this
-- generates no tags at all.
meta' :: Meta -> Maybe Text -> Text
meta' _ Nothing              = ""
meta' metaTag (Just content) = meta metaTag content

-- | Somewhat ad hoc logic to escape troublesome HTML characters.
escapeChars :: Text -> Text
escapeChars = Text.replace "\"" "&quot;" . Text.replace "'" "&apos;" . Text.replace "&" "&amp;"
  -- "&" has to be last, otherwise it escapes the & from &quot;/etc!
