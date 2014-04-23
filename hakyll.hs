{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Data.Monoid (mappend)
import Hakyll

main :: IO ()
main = hakyllWith config $ do
  -- create ["home.html"] $ do
  --     route idRoute
  --     compile $ do
  --         makeItem ""
  --         >>= loadAndApplyTemplate "templates/default.html" defaultContext
  --         >>= relativizeUrls

  match "templates/*" $ compile templateCompiler

  match (fromList ["humans.txt", "robots.txt"]) $ do
      route idRoute
      compile copyFileCompiler
    
  match "css/*.css" $ do
      route   idRoute
      compile compressCssCompiler

  match "tech/*" $ do
      route $ setExtension "html"
      compile $ pandocCompiler
          -- >>= loadAndApplyTemplate "templates/post.html"    postCtx
          >>= loadAndApplyTemplate "templates/default.html" postCtx
          >>= relativizeUrls

  match (fromList ["index.markdown", "404.markdown"]) $ do
      route   $ setExtension "html"
      compile $ pandocCompiler
          >>= loadAndApplyTemplate "templates/default.html" defaultContext
          >>= relativizeUrls          

    -- public GPG key
  match "files/ianweatherhogg.asc" $ do
      route   (constRoute "ianweatherhogg.asc")
      compile copyFileCompiler
        
  create ["archive.html"] $ do
      route idRoute
      compile $ do
          let archiveCtx =
                  field "tech" (\_ -> techList recentFirst) `mappend`
                  constField "title" "Archives"              `mappend`
                  constField "description" "Archive of all postings" `mappend`
                  constField "tags" "archive" `mappend`
                  defaultContext

          makeItem ""
              >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
              >>= loadAndApplyTemplate "templates/default.html" archiveCtx
              >>= relativizeUrls

  match "images/***" $ do
      route   idRoute
      compile copyFileCompiler              
                
postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" `mappend`
    defaultContext

techList :: ([Item String] -> Compiler [Item String]) -> Compiler String
techList sortFilter = do
    posts   <- sortFilter =<< loadAll "tech/*"
    itemTpl <- loadBody "templates/post-item.html"
    list    <- applyTemplateList itemTpl postCtx posts
    return list

-- `ian` is an alias in `.ssh/config` which points to target
-- host, user, port    
config :: Configuration
config = defaultConfiguration
    { deployCommand = "rsync --checksum -ave ssh \
                      \_site/* ian:www/ianweatherhogg.com/"
    }
