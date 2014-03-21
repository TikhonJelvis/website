#!/usr/bin/env runhaskell
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}

-- | A handy script that generates blog posts automatically using the
--   directory structure expected by my Hakyll setup.
module Main where

import           Control.Concurrent        (threadDelay)
import           Control.Monad             (when)

import           Data.Functor              ((<$))
import           Data.List                 (isPrefixOf)
import           Data.String.Interpolation (str)
import qualified Data.Time                 as Time
import           Data.Time                 (LocalTime)

import           Options.Applicative       hiding (str)

import qualified System.Directory          as Dir
import qualified System.FilePath           as Path
import           System.FilePath           ((</>))
import qualified System.IO.Strict          as Strict
import           System.Process            (runCommand)

import           Text.Printf               (printf)

main :: IO ()
main = execParser cmdParser >>= run

data Settings = Settings { name    :: String
                         , publish :: Bool
                         }

cmdParser :: ParserInfo Settings
cmdParser = info (helper <*> options)
            ( fullDesc
           <> progDesc desc )

 where
   desc = [str| Automatically start a blog post draft or move a draft to the published directory. |]

options :: Parser Settings
options = Settings <$> argument return
                     ( help "The name of the blog post."
                    <> metavar "NAME" )
                   <*> switch
                     ( help "Publish an existing blog post rather than starting a draft."
                    <> long "publish"
                    <> short 'p' )

index :: String -> String
index name = [str|
---
title: $name$
author: Tikhon Jelvis
---

|]

addTimeVar :: String -> LocalTime -> String -> String
addTimeVar name time file =
  [str|---
#l in header:$l$|
#
$name$: $:time$
#l in rest:$l$|
#

|]
  where (header, rest) = (takeWhile (not . isPrefixOf "---") . drop 1 $ lines file,
                          dropWhile (not . isPrefixOf "---") . drop 1 $ lines file)
        varLine        = [str|$name$: $:time$|]

escape :: String -> String
escape str = str >>= \case '\'' -> "'\\''"; s -> [s]

run :: Settings -> IO ()
run Settings {..} = do
  Dir.setCurrentDirectory "/home/tikhon/Public/website/drafts"
  exists <- Dir.doesDirectoryExist name
  if | not publish && not exists -> do
    Dir.createDirectory name
    printf "Creating post %s\n" name
    Dir.setCurrentDirectory name
    writeFile "index.md" $ index name

     | not publish && exists -> 
    putStrLn "Draft already exists! Not doing anything."
     
     | publish && exists -> do
    published <- Dir.doesDirectoryExist $ ".." </> "blog" </> name
    let var = if published then "modified" else "published"
    putStrLn $ "Publishing " ++ name
    Dir.setCurrentDirectory name

    zone <- Time.getCurrentTimeZone
    time <- Time.getCurrentTime
    let local = Time.utcToLocalTime zone time

    Strict.readFile "index.md"
      >>= writeFile "index.md" . addTimeVar var local
    
    () <$ runCommand [str|cp -r '../$escape name$' ../../blog/|]

     | publish && not exists -> 
    error "Cannot publish draft that doesn't exist!"
