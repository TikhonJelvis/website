#!/usr/bin/env runhaskell
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE NamedFieldPuns            #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE ViewPatterns              #-}

import           Control.Monad        (forM_)

import           Data.Functor         ((<$>))
import qualified Data.List            as List
import qualified Data.Map             as Map
import           Data.Monoid          ((<>), mconcat)
import           Data.String          (fromString)

import qualified System.Directory     as Dir
import qualified System.FilePath      as Path
import           System.FilePath      ((</>))

import qualified Text.Pandoc.Options  as P
import           Text.Printf          (printf)

import           Hakyll

main = hakyll $ do
  match "templates/*" $ do
    compile templateCompiler

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
    route   $ setExtension "html"
    compile $
      do includes <- setIncludes =<< getUnderlying
         getResourceString
           >>= applyAsTemplate includes 
           >>= return . runPandoc
           >>= loadAndApplyTemplate "templates/default.html" context
           >>= relativizeUrls
      where context = defaultContext <> title <> imports
            runPandoc = renderPandocWith defaultHakyllReaderOptions pandocOptions
  
title = field "title" $ \ item -> do
  metadata <- getMetadata (itemIdentifier item)
  return $ case Map.lookup "title" metadata of
    Just title -> printf "%s | jelv.is" title
    Nothing    -> "jelv.is"

imports = include "imports.html"

setIncludes (Path.takeDirectory . toFilePath -> dir) = mconcat . map include <$> files
  where files = unsafeCompiler $ do
          exists <- Dir.doesDirectoryExist $ dir </> "include"
          if exists then do
            printf "including contents of %s\n" dir
            files <- clean <$> Dir.getDirectoryContents (dir </> "include")
            forM_ files putStrLn
            putStrLn "--------------------"
            return files
            else do
            printf "%s does not exist!\n" dir
            return []
        clean = List.delete "." . List.delete ".." . filter ((/= '~') . last)

include file = field name $ \ item -> unsafeCompiler $ do
  printf "Including %s as %s\n" file name
  let dir = Path.takeDirectory . toFilePath $ itemIdentifier item
  exists <- Dir.doesFileExist $ dir </> "include" </> file
  if exists then readFile $ dir </> "include" </> file
            else return ""
  where name = Path.takeBaseName file
  

pandocOptions = defaultHakyllWriterOptions {
  P.writerHTMLMathMethod = P.MathJax ""
}

removeDir dir = customRoute $ remove . toFilePath
  where remove file = Path.joinPath . filter (/= target) $ Path.splitPath file
        target      = Path.addTrailingPathSeparator dir

deep :: String -> Pattern
deep name = pat "%s/**" .||. pat "**/%s/**"
  where pat spec = fromString $ printf spec name

alternates = foldr1 (.||.)
