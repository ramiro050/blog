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

    -- build up tags
    tags <- buildTags "posts/*" (fromCapture "tags/*.html")

    tagsRules tags $ \tag pattern -> do
        let title = "Posts tagged <q>" ++ tag ++ "</q>"
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll pattern
            let ctx = constField "title" title
                      `mappend` listField "posts" (postCtxWithTags tags) (return posts)
                      `mappend` defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/tag.html" ctx
                >>= loadAndApplyTemplate "templates/default.html" ctx
                >>= relativizeUrls

    match "posts/*" $ do
        route $ setExtension "html"
        compile $ bibtexCompiler "bib/style.csl" "bib/references.bib"
            >>= loadAndApplyTemplate "templates/post.html"    (postCtxWithTags tags)
            >>= loadAndApplyTemplate "templates/default.html" (postCtxWithTags tags)
            >>= relativizeUrls

    create ["archive.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            tagList <- renderTagList tags
            let archiveCtx =
                  constField "tags" tagList                `mappend`
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
postCtx = dateField "date" "%B %e, %Y" `mappend` defaultContext

postCtxWithTags :: Tags -> Context String
postCtxWithTags tags = tagsField "tags" tags `mappend` postCtx

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
