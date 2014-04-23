---
title: creating a hakyll static web site
description: creating a static hakyll web site
tags: haskell hakyll
---

# Background

Could angle this as being a helloworld thing
just getting the bare minimum up and running

# index.html

create versus match in site.hs
opinion is divided
thinking about having a splash page scenerio and then hiving off other stuff (posts) elsewhere
with link from this splash/home page

what filters to run through

# haskell stuff

## overloaded strings - what is this?
reference to fpcomplete
Interesting though as to why it's required in the site.hs file
(note: site.hs/hakyll.hs/default.hs) synonomus - declare this

at the top of site.hs

```haskell
{-# LANGUAGE OverloadedStrings #-}
```
http://jaspervdj.be/hakyll/tutorials/03-rules-routes-compilers.html

The OverloadedStrings extension allows us to just write Strings here, which are interpreted as globs — all files in the images/ directory, and all files in the css/ directory.


## what is included as default in site.hs

default site.hs from hakyll-init

```haskell
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid (mappend)
import           Hakyll
```

Durval /home/ian/Code/haskell/github/divarvel/blog/hakyll.hs

```haskell
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Functor ((<$>))
import Data.List (isPrefixOf)
import Data.Monoid (mappend)
import Data.Text (pack, unpack, replace, empty)

import Hakyll
```

I suspect that it depends on what we want to do with it
but I've seen variation

## hakyll monad

```haskell
hakyll :: Rules a -> IO ()
hakyll = hakyllWith Config.defaultConfiguration
```

### rules

```haskell
-- Hakyll.Core.Rules.Internal
newtype Rules a = Rules
    { unRules :: RWST RulesRead RuleSet RulesState IO a
    } deriving (Monad, Functor, Applicative)
```

```haskell
-- Hakyll.Core.Rules
match :: Pattern -> Rules () -> Rules ()
match pattern rules = do
    tellPattern pattern
    flush
    ids <- getMatches pattern
    tellResources ids
    Rules $ local (setMatches ids) $ unRules $ rules >> flush
  where
    setMatches ids env = env {rulesMatches = ids}

create :: [Identifier] -> Rules () -> Rules ()
create ids rules = do
    flush
    Rules $ local setMatches $ unRules $ rules >> flush
  where
    setMatches env = env {rulesMatches = ids}
```
### do syntax

Taken from /home/ian/Code/haskell/github/sebastiaanvisser/sebastiaanvisser.github.com/src/Site.hs

```haskell
main :: IO ()
main = hakyll $
  do images
     files
     styles
     pages
     posts
     templates

templates :: Rules ()
templates = match "template/*" (compile templateCompiler)

images :: Rules ()
images = match "image/*" $
  do route   idRoute
     compile copyFileCompiler

files :: Rules ()
files = match "file/*" $
  do route   idRoute
     compile copyFileCompiler
```
Point being, do systax with Rules () type
# haskyll-init or just it yourself

reference to code to illustrate what is brought in
I'm more of a grass roots level person, cracking on from the bottom 

# deployment options

thinking rsync to a VPS with nginx sub domains
static sites, github, heroku

# further (questions)

## what's with .nojekyll?


