--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid (mappend)
import           Hakyll
import           Text.Pandoc.SideNote (usingSideNotes)
import           Text.Pandoc.Options
import           Control.Monad


--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match "bib/references.bib" $ compile biblioCompiler
    match "bib/style.csl" $ compile cslCompiler

    match "css/IBM-Plex-Serif/**" $ do
      route idRoute
      compile copyFileCompiler

    match "css/IBM-Plex-Mono/**" $ do
      route idRoute
      compile copyFileCompiler

    match "css/styles/*.css" $ do
      route idRoute
      compile compressCssCompiler

    match "js/*" $ do
      route idRoute
      compile copyFileCompiler

    match (fromList ["about.rst", "contact.markdown"]) $ do
        route   $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/info.html" defaultContext
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    match "posts/*" $ do
        route $ setExtension "html"
        compile $ bibtexCompiler "bib/style.csl" "bib/references.bib"
            >>= loadAndApplyTemplate "templates/post.html"    postCtx
            >>= loadAndApplyTemplate "templates/default.html" postCtx
            >>= relativizeUrls

    create ["archive.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let archiveCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    constField "title" "Archives"            `mappend`
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
                >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                >>= relativizeUrls


    match "index.html" $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let indexCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    defaultContext

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls

    match "templates/*" $ compile templateBodyCompiler


--------------------------------------------------------------------------------
postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" `mappend`
    defaultContext

myWriterOptions :: WriterOptions
myWriterOptions = defaultHakyllWriterOptions {
  writerHTMLMathMethod = MathJax defaultMathJaxURL }

myReaderOptions :: ReaderOptions
myReaderOptions = defaultHakyllReaderOptions {
  -- At the moment YAML extension does not do anything...
  readerExtensions = enableExtensionsIn defaultExts [Ext_citations,
                                                     Ext_yaml_metadata_block] }
  where defaultExts = readerExtensions defaultHakyllReaderOptions

enableExtensionsIn :: Extensions -> [Extension] -> Extensions
enableExtensionsIn oldExts newExts = foldr enableExtension oldExts newExts

bibtexCompiler :: String -> String -> Compiler (Item String)
bibtexCompiler cslFileName bibFileName = do
  csl <- load $ fromFilePath cslFileName
  bib <- load $ fromFilePath bibFileName
  liftM (writePandocWith myWriterOptions)
    (getResourceBody
      >>= readPandocBiblio myReaderOptions csl bib
      >>= withItemBody (pure . usingSideNotes))
