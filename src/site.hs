--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.List (isSuffixOf)
import qualified Data.Map as Map
import           Data.Monoid (mappend)
import           Skylighting.Styles
import           Skylighting.Types hiding (Context)
import           System.FilePath (takeDirectory, (<.>), (</>))
import           Text.Pandoc.Highlighting (Style, styleToCss)
import           Text.Pandoc.Options      (ReaderOptions (..), WriterOptions (..))

import           Hakyll

--------------------------------------------------------------------------------

nord0 = RGB 46 52 64
nord1 = RGB 59 66 82
nord2 = RGB 67 76 94
nord3 = RGB 76 86 106
nord4 = RGB 216 222 233
nord5 = RGB 229 233 240
nord6 = RGB 236 239 244
nord7 = RGB 143 188 187
nord8 = RGB 136 192 208
nord9 = RGB 129 161 193
nord10 = RGB 94 129 172
nord11 = RGB 191 97 106
nord12 = RGB 208 135 112
nord13 = RGB 235 203 139
nord14 = RGB 163 190 140
nord15 = RGB 180 142 173

-- | Style from the breeze-dark KDE syntax highlighting theme.
nord :: Style
nord = Style
    { tokenStyles = Map.fromList
        [ ( KeywordTok, defStyle { tokenColor = Just nord9, tokenBold = True })
        , ( DataTypeTok, defStyle { tokenColor = Just nord9 })
        , ( DecValTok, defStyle { tokenColor = Just nord15 })
        , ( BaseNTok, defStyle { tokenColor = Just nord15 })
        , ( FloatTok, defStyle { tokenColor = Just nord15 })
        , ( ConstantTok, defStyle { tokenColor = Just nord6, tokenBold = True })
        , ( CharTok, defStyle { tokenColor = Just nord13 })
        , ( SpecialCharTok, defStyle { tokenColor = Just nord13 })
        , ( StringTok, defStyle { tokenColor = Just nord14 })
        , ( VerbatimStringTok, defStyle { tokenColor = Just (RGB 141 174 112) })
        , ( SpecialStringTok, defStyle { tokenColor = Just nord12 })
        , ( ImportTok, defStyle { tokenColor = Just nord14 })
        , ( CommentTok, defStyle { tokenColor = Just (RGB 97 110 136) })
        , ( DocumentationTok, defStyle { tokenColor = Just nord10 })
        , ( AnnotationTok, defStyle { tokenColor = Just nord12 })
        , ( CommentVarTok, defStyle { tokenColor = Just nord5 })
        , ( OtherTok, defStyle { tokenColor = Just nord7 })
        , ( FunctionTok, defStyle { tokenColor = Just nord8 })
        , ( VariableTok, defStyle { tokenColor = Just nord10 })
        , ( ControlFlowTok, defStyle { tokenColor = Just nord9, tokenBold = True })
        , ( OperatorTok, defStyle { tokenColor = Just nord9 })
        , ( BuiltInTok, defStyle { tokenColor = Just nord8 })
        , ( ExtensionTok, defStyle { tokenColor = Just nord7, tokenBold = True })
        , ( PreprocessorTok, defStyle { tokenColor = Just nord10 })
        , ( AttributeTok, defStyle { tokenColor = Just nord7 })
        , ( RegionMarkerTok, defStyle { tokenColor = Just nord8, tokenBackground = Just nord1 })
        , ( InformationTok, defStyle { tokenColor = Just nord13 })
        , ( WarningTok, defStyle { tokenColor = Just nord11 })
        , ( AlertTok, defStyle { tokenColor = Just nord11, tokenBackground = Just nord1, tokenBold = True })
        , ( ErrorTok, defStyle { tokenColor = Just nord11, tokenUnderline = True })
        , ( NormalTok, defStyle { tokenColor = Just nord4 })
        ]
    , defaultColor = Just nord4 -- Guess here based on Normal
    , backgroundColor = Just nord0
    , lineNumberColor = Just nord3
    , lineNumberBackgroundColor = Just nord0
    }

pandocCodeStyle :: Style
pandocCodeStyle = nord

pandocCompiler' :: Compiler (Item String)
pandocCompiler' =
  pandocCompilerWith
    defaultHakyllReaderOptions
    defaultHakyllWriterOptions
      { writerHighlightStyle   = Just pandocCodeStyle
      }

main :: IO ()
main = do

  hakyll $ do
    create ["css/syntax.css"] $ do
      route idRoute
      compile $ do
        makeItem $ styleToCss pandocCodeStyle

    match "css/**" $ do
        route   idRoute
        compile compressCssCompiler

    match "fonts/**" $ do
        route   idRoute
        compile copyFileCompiler

    match ("posts/*/*.lhs" .||. ("posts/*/*.md" .&&. complement ("posts/*/README.md"))) $ do
        route $ (customRoute removeFileNameRoute)
        compile $ pandocCompiler'
            >>= loadAndApplyTemplate "templates/post.html"    postCtx
            >>= loadAndApplyTemplate "templates/default.html" postCtx
            >>= relativizeUrls
            >>= cleanIndexUrls

    create ["archive.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*/*"
            let archiveCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    constField "title" "Archives"            `mappend`
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
                >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                >>= relativizeUrls
                >>= cleanIndexUrls

    create ["index.md"] $ do
        route $ setExtension "html"
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*/*"
            let indexCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    constField "title" "Home"                `mappend`
                    defaultContext

            pandocCompiler
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls
                >>= cleanIndexUrls

    match "templates/*" $ compile templateBodyCompiler


--------------------------------------------------------------------------------
postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" `mappend`
    defaultContext

removeFileNameRoute :: Identifier -> FilePath
removeFileNameRoute = (</> "index.html") . takeDirectory . toFilePath

-- Totally stolen from
-- https://www.rohanjain.in/hakyll-clean-urls/
cleanIndexUrls :: Item String -> Compiler (Item String)
cleanIndexUrls = return . fmap (withUrls cleanIndex)

cleanIndexHtmls :: Item String -> Compiler (Item String)
cleanIndexHtmls = return . fmap (replaceAll pattern replacement)
    where
      pattern = "/index.html"
      replacement = const "/"

cleanIndex :: String -> String
cleanIndex url
    | idx `isSuffixOf` url = take (length url - length idx) url
    | otherwise            = url
  where idx = "index.html"
