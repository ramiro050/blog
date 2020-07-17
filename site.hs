--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid (mappend)
import           Hakyll
import           Text.Pandoc.SideNote (usingSideNotes)
import           Text.Pandoc.Options
import           Text.Pandoc.Definition
import           Text.CSL.Pandoc (processCites)
import qualified Text.CSL as CSL
import qualified Data.Map as M
import qualified Data.Text as T
import           Control.Monad
import           System.Directory
import           System.FilePath


--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

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

    match "bib/*.bib" $ compile biblioCompiler
    match "bib/style.csl" $ compile cslCompiler

    match "posts/*" $ do
        route $ setExtension "html"
        compile $ postCompiler "bib/style.csl" "bib/"
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


--  CONTEXT FUNCTIONS  ---------------------------------------------------------
postCtx :: Context String
postCtx = dateField "date" "%B %e, %Y" `mappend` defaultContext

postCtxWithTags :: Tags -> Context String
postCtxWithTags tags = tagsField "tags" tags `mappend` postCtx


--  PANDOC OPTIONS FUNCTIONS  --------------------------------------------------
myWriterOptions :: WriterOptions
myWriterOptions = defaultHakyllWriterOptions {
  writerHTMLMathMethod = MathJax defaultMathJaxURL }

myReaderOptions :: ReaderOptions
myReaderOptions = defaultHakyllReaderOptions { readerExtensions = defaultExts <> otherExts }
  where
    defaultExts = readerExtensions defaultHakyllReaderOptions
    otherExts = extensionsFromList [Ext_citations]


--  BIBTEX FUNCTIONS  ----------------------------------------------------------
getBibFilePath :: FilePath -> Compiler (Maybe FilePath)
getBibFilePath dir = do
  currFileName <- (takeBaseName . toFilePath) <$> getUnderlying
  unsafeCompiler $ do
    let bibFilePath = dir ++ currFileName <.> "bib"
    bibFileExists <- doesFileExist bibFilePath
    case bibFileExists of
      True -> return $ Just bibFilePath
      False -> return Nothing

-- | 'postCompiler' checks if there is a bib file in @bibDir@ that has the same
-- name as the file that is currently being compiled.
--
-- If bibFile is found, post is compiled with references. Otherwise, it is compiled
-- without them.
postCompiler :: FilePath -> FilePath -> Compiler (Item String)
postCompiler cslFilePath bibDir = do
  maybeBibFilePath <- getBibFilePath bibDir
  case maybeBibFilePath of
    Just bibFilePath -> bibtexCompiler cslFilePath bibFilePath
    Nothing -> pandocCompilerWithTransform myReaderOptions myWriterOptions usingSideNotes

bibtexCompiler :: String -> String -> Compiler (Item String)
bibtexCompiler cslFilePath bibFilePath = do
  csl <- load $ fromFilePath cslFilePath
  bib <- load $ fromFilePath bibFilePath
  getResourceBody
    >>= readPandocBiblioWithTransform myReaderOptions csl bib biblioTransform
    >>= pure . (writePandocWith myWriterOptions)

-- | Rewrite of 'readPandocBiblio' function from Hakyll in order to include an
-- Pandoc transformer before pandoc is passed to 'processCites'
readPandocBiblioWithTransform :: ReaderOptions
                              -> Item CSL
                              -> Item Biblio
                              -> (Pandoc -> Pandoc)
                              -> (Item String)
                              -> Compiler (Item Pandoc)
readPandocBiblioWithTransform ropt csl biblio f item = do
    -- Parse CSL file, if given
    style <- unsafeCompiler $ CSL.readCSLFile Nothing . toFilePath . itemIdentifier $ csl

    -- We need to know the citation keys, add then *before* actually parsing the
    -- actual page. If we don't do this, pandoc won't even consider them
    -- citations!
    let Biblio refs = itemBody biblio
    pandoc <- itemBody <$> readPandocWith ropt item
    let pandoc' = processCites style refs (f pandoc)
    return $ fmap (const pandoc') item


--  PANDOC FUNCTIONS  ----------------------------------------------------------
setMetaData :: [(T.Text, MetaValue)] -> Pandoc -> Pandoc
setMetaData metaData (Pandoc meta b) = Pandoc meta' b
  where meta' = Meta $ foldl step (unMeta meta) metaData
        step m (t, metaVal) = M.insert t metaVal m

addHRule :: Pandoc -> Pandoc
addHRule (Pandoc m b) = Pandoc m (b ++ [HorizontalRule])

biblioTransform :: Pandoc -> Pandoc
biblioTransform = usingSideNotes . addHRule . (setMetaData metaData)
  where metaData = [("link-citations", MetaBool True),
                    -- ("reference-section-title", MetaString "References"),
                    ("suppress-bibliography", MetaBool False)]
