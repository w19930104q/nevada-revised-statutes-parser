module ChapterFile
  ( ChapterMap
  , buildReferenceRegistry
  , chapterNumberToFilename
  , chapterNumberToFilenameText
  , enrichChapterReferences
  , fillInEmptyChapter
  , makeChapterData
  , parseChapter
  , parseChapterFileTitle
  , subChapterHeadingGroups
  )
where

import           BasicPrelude
import           Control.Monad.Writer.Strict    ( censor
                                                , runWriterT
                                                , tell
                                                , writer
                                                )
import qualified Data.Attoparsec.Text           ( Parser
                                                , parseOnly
                                                , takeText
                                                , takeWhile
                                                )
import           Data.Char                      ( isAlpha
                                                , isSpace
                                                )
import qualified Data.HashMap.Lazy             as HM
import qualified Data.Text                     as T
import           Text.HTML.TagSoup
import           Text.Parser.Char
import           Text.Printf
import           Text.InterpolatedString.Perl6  ( qq )

import           Config
import           FileUtil                       ( RelativePath
                                                , toRelativePath
                                                )
import           HtmlUtil                       ( Html
                                                , titleText
                                                , toText
                                                )
import qualified ReferenceParser               as References
import qualified SimpleChapterFile              ( parseSectionsFromJustHtml
                                                , parseSectionsFromHeadingGroup
                                                , isSimpleSubChapter
                                                )
import           SimpleChapterFile              ( attachSectionReferences )
import           Models.Chapter                as Chapter
import           Models.ParseFailure            ( ParseResult
                                                , ParseResultT
                                                , atChapter
                                                , parseFailure
                                                )
import           Models.SubChapter             as SubChapter
import           Models.SubSubChapter          as SubSubChapter
import           Parsing                        ( ChapterData(..)
                                                , TagList
                                                )
import           TextUtil                       ( normalizeWhiteSpace
                                                , titleize
                                                )
import           Util                           ( (|>) )

type ChapterMap = HashMap RelativePath Html


heading2P :: String
heading2P = "<p class=COHead2>"

heading4P :: String
heading4P = "<p class=COHead4>"

horizontalRule :: String
horizontalRule = "<p class=J-Dash"


fillInEmptyChapter
  :: ChapterMap -> References.AnchorRegistry -> Chapter -> ParseResult Chapter
fillInEmptyChapter chapterMap registry emptyChapter =
  let number'   = Chapter.number emptyChapter
      fileText  = chapterNumberToFilenameText number'
      key       = toRelativePath (T.unpack fileText)
      located msg = parseFailure fileText msg |> atChapter number'
      fallback msg = tell [located msg] >> pure emptyChapter
  in  if number' `elem` chaptersToSkip
        then fallback "skipped by Config.chaptersToSkip"
        else case HM.lookup key chapterMap of
          Just html -> case runWriterT (parseChapter html) of
            Right result ->
              writer result >>= enrichChapterReferences registry key
            Left message -> fallback (T.pack message)
          Nothing -> fallback ("Chapter file not found: " ++ fileText)


-- The anchor registry only depends on the mirrored pages, so it is
-- built once and shared by every chapter enrichment pass.
buildReferenceRegistry :: ChapterMap -> References.AnchorRegistry
buildReferenceRegistry = References.buildAnchorRegistry . HM.toList


chapterNumberToFilenameText :: Text -> Text
chapterNumberToFilenameText chapterNumber =
  T.pack (chapterNumberToFilenameString chapterNumber)


chapterNumberToFilenameString :: Text -> String
chapterNumberToFilenameString chapterNumber =
  let format | isAlpha (T.last chapterNumber) = "%04s"
             | otherwise                      = "%03s"
  in  "NRS-" ++ printf format chapterNumber ++ ".html"


chapterNumberToFilename :: Text -> RelativePath
chapterNumberToFilename =
  toRelativePath . chapterNumberToFilenameString


parseChapter :: Html -> ParseResultT (Either String) Chapter
parseChapter chapterHtml = do
  let chapterData = makeChapterData chapterHtml
  rawTitle <- lift $ titleText (headings chapterData)
  (rawNumber, rawName) <- lift $ parseChapterFileTitle rawTitle
  sectionsOrSubChapters <- censor (fmap (atChapter rawNumber)) $
    chapterContent chapterData
  pure Chapter
    { Chapter.name    = rawName
    , Chapter.number  = rawNumber
    , Chapter.url     = chapterUrlPrefix ++ rawNumber ++ ".html"
    , Chapter.content = sectionsOrSubChapters
    }


-- Attach ordered references to every section of a parsed chapter using
-- the shared anchor registry. Relative hrefs resolve against the
-- chapter's own page; link diagnostics are collected without failing
-- the enclosing section. Base-monad polymorphic so the same pass runs
-- inside either the plain ParseResult writer or the Either-backed
-- structural parsers.
enrichChapterReferences
  :: Monad m
  => References.AnchorRegistry
  -> RelativePath
  -> Chapter
  -> ParseResultT m Chapter
enrichChapterReferences registry sourcePath chapter = do
  content' <- enrichContent registry sourcePath (Chapter.content chapter)
  pure chapter { Chapter.content = content' }


enrichContent :: Monad m
  => References.AnchorRegistry
  -> RelativePath
  -> ChapterContent
  -> ParseResultT m ChapterContent
enrichContent registry sourcePath content' = case content' of
  SimpleChapterContent chapterSections ->
    SimpleChapterContent
      <$> attachSectionReferences registry sourcePath chapterSections
  ComplexChapterContent subChapters ->
    ComplexChapterContent <$> mapM enrichSubChapter subChapters
 where
  enrichSubChapter subChapter = case SubChapter.children subChapter of
    SubChapterSections chapterSections -> do
      sections' <- attachSectionReferences registry sourcePath chapterSections
      pure subChapter { SubChapter.children = SubChapterSections sections' }
    SubSubChapters subSubs -> do
      subSubs' <- mapM enrichSubSub subSubs
      pure subChapter { SubChapter.children = SubSubChapters subSubs' }
  enrichSubSub subSub = do
    sections' <-
      attachSectionReferences registry sourcePath (SubSubChapter.sections subSub)
    pure subSub { SubSubChapter.sections = sections' }


makeChapterData :: Html -> ChapterData
makeChapterData chapterHtml =
  let fullPage    = parseTags $ toText chapterHtml
      topHalf'    = topHalf fullPage
      bottomHalf' = bottomHalf fullPage
  in  ChapterData
        { headings      = topHalf'
        , content       = bottomHalf'
        , sectionGroups = findSectionGroups bottomHalf'
        }


findSectionGroups :: TagList -> [TagList]
findSectionGroups contentHalf =
  partitions (~== ("<span class=Section" :: String)) contentHalf


topHalf :: TagList -> TagList
topHalf fullPage = takeWhile (~/= horizontalRule) fullPage

bottomHalf :: TagList -> TagList
bottomHalf fullPage = dropWhile (~/= horizontalRule) fullPage


chapterContent :: ChapterData -> ParseResultT (Either String) ChapterContent
chapterContent chapterData =
  case subChapterHeadingGroups chapterData of
    [] -> SimpleChapterContent <$>
      SimpleChapterFile.parseSectionsFromJustHtml chapterData
    groups -> ComplexChapterContent <$> mapM (newSubChapter chapterData) groups


newSubChapter
  :: ChapterData -> TagList -> ParseResultT (Either String) SubChapter
newSubChapter chapterData headingGroup = do
  name' <- lift $ subChapterNameFromGroup headingGroup
  children' <- if SimpleChapterFile.isSimpleSubChapter headingGroup
    then SubChapterSections <$>
      SimpleChapterFile.parseSectionsFromHeadingGroup chapterData headingGroup
    else SubSubChapters <$> parseSubSubChapters chapterData headingGroup
  pure SubChapter { SubChapter.name = name', SubChapter.children = children' }


parseSubSubChapters
  :: ChapterData -> TagList -> ParseResultT (Either String) [SubSubChapter]
parseSubSubChapters chapterData headingGroup =
  mapM (parseSubSubChapter chapterData) (subSubChapterHeadingGroups headingGroup)


parseSubSubChapter
  :: ChapterData -> TagList -> ParseResultT (Either String) SubSubChapter
parseSubSubChapter chapterData headingGroup = do
  name' <- lift $ extractSubSubChapterName headingGroup
  sections' <-
    SimpleChapterFile.parseSectionsFromHeadingGroup chapterData headingGroup
  pure SubSubChapter
    { SubSubChapter.name = name', SubSubChapter.sections = sections' }


extractSubSubChapterName :: TagList -> Either String Text
extractSubSubChapterName headingGroup =
  let linesOfText = lines $ innerText headingGroup
  in  case linesOfText of
        (x : _) -> Right $ normalizeWhiteSpace x
        _ ->
          Left [qq|Couldn't parse sub sub chapter name from: $headingGroup|]


subChapterNameFromGroup :: TagList -> Either String Text
subChapterNameFromGroup (_ : y : _) = Right $ titleize $ fromTagText y
subChapterNameFromGroup tags =
  Left [qq|Couldn't get a chapter name from the group: $tags|]


subChapterHeadingGroups :: ChapterData -> [TagList]
subChapterHeadingGroups chapterData =
  partitions (~== heading2P) (headings chapterData)


subSubChapterHeadingGroups :: TagList -> [TagList]
subSubChapterHeadingGroups subChapterHeadingGroup =
  partitions (~== heading4P) subChapterHeadingGroup


-- Input:  "NRS: CHAPTER 432B - PROTECTION OF CHILDREN FROM ABUSE AND NEGLECT"
--    or:  "NRS: PRELIMINARY CHAPTER - GENERAL PROVISIONS"
-- Output: ("432B", "Protection of Children from Abuse and Neglect")
parseChapterFileTitle :: Text -> Either String (Text, Text)
parseChapterFileTitle input = if input == chapterZeroTitle
  then Right ("0", "Preliminary Chapter – General Provisions")
  else case Data.Attoparsec.Text.parseOnly chapterTitleParser input of
    Left  e -> Left [qq|Couldn't parse chapter file title $input $e|]
    Right b -> Right b


-- Input:  "NRS: CHAPTER 432B - PROTECTION OF CHILDREN FROM ABUSE AND NEGLECT"
-- Output: ("432B", "Protection of Children from Abuse and Neglect")
chapterTitleParser :: Data.Attoparsec.Text.Parser (Text, Text)
chapterTitleParser = do
  _     <- string "NRS: CHAPTER "
  num   <- Data.Attoparsec.Text.takeWhile (not . isSpace)
  _     <- string " - "
  title <- Data.Attoparsec.Text.takeText
  return (num, normalizeWhiteSpace $ titleize title)
