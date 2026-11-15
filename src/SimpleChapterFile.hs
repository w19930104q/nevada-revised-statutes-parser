module SimpleChapterFile
  ( isSimpleSubChapter
  , parseSectionsFromJustHtml
  , parseSectionFromHeadingParagraph
  , parseSectionBody
  , parseSectionsFromHeadingGroup
  , parseNumberFromRawNumberText
  , attachSectionReferences
  )
where

import           BasicPrelude
import           Control.Monad.Writer.Strict    ( tell )
import qualified Data.Text                     as T
import           Text.HTML.TagSoup
import           Text.InterpolatedString.Perl6  ( qq )

import           FileUtil                       ( RelativePath
                                                , relativeToFilePath
                                                )
import           HtmlUtil                       ( Html(NewHtml)
                                                , shaveBackTagsToLastClosingP
                                                )
import           Models.ParseFailure            ( ParseFailure
                                                , ParseResultT
                                                , accumulate
                                                , atSection
                                                , parseFailure
                                                )
import           Models.References              ( SectionReference )
import           Models.Section                as Section
import           Parsing                        ( ChapterData(..)
                                                , TagList
                                                )
import qualified ReferenceParser               as References
import           ReferenceParser                ( AnchorRegistry
                                                , LinkDiagnostic(..)
                                                , LinkIssue(..)
                                                )
import qualified SectionParser
import           TextUtil                       ( normalizeWhiteSpace
                                                , normalizedInnerText
                                                )
import           Util                           ( (|>) )


closingA :: String
closingA = "</a>"

closingP :: String
closingP = "</p>"

leadlineP :: String
leadlineP = "<p class=COLeadline>"

heading4P :: String
heading4P = "<p class=COHead4>"

isSimpleSubChapter :: TagList -> Bool
isSimpleSubChapter headingGroup =
  null (partitions (~== heading4P) headingGroup)


parseSectionsFromJustHtml :: Monad m => ChapterData -> ParseResultT m [Section]
parseSectionsFromJustHtml chapterData =
  accumulateSections chapterData (headingParagraphsWithContent chapterData)


parseSectionsFromHeadingGroup
  :: Monad m => ChapterData -> TagList -> ParseResultT m [Section]
parseSectionsFromHeadingGroup chapterData headingGroup =
  accumulateSections chapterData (headingParagraphsWithContent' headingGroup)


accumulateSections :: Monad m => ChapterData -> [TagList] -> ParseResultT m [Section]
accumulateSections chapterData =
  accumulate (parseSectionFromHeadingParagraph chapterData) sectionFailure


sectionFailure :: TagList -> String -> ParseFailure
sectionFailure paragraph err =
  let base = parseFailure "" (T.pack err)
  in  case sectionNumberHint paragraph of
        Just n  -> base |> atSection n
        Nothing -> base


sectionNumberHint :: TagList -> Maybe Text
sectionNumberHint paragraph =
  let secName       = sectionNameFromParagraph paragraph
      rawNumberText = rawNumberTextFromParagraph paragraph
  in  case parseNumberFromRawNumberText rawNumberText secName of
        Right n -> Just n
        Left _  -> Nothing


headingParagraphsWithContent :: ChapterData -> [TagList]
headingParagraphsWithContent chapterData = filter SectionParser.isTOCEntry
                                                  putativeTOCEntries
 where
  putativeTOCEntries =
    (takeWhile (~/= closingP))
      <$> (partitions (~== leadlineP) (headings chapterData))


headingParagraphsWithContent' :: TagList -> [TagList]
headingParagraphsWithContent' headingGroup = filter SectionParser.isTOCEntry
                                                    putativeTOCEntries
 where
  putativeTOCEntries =
    (takeWhile (~/= closingP)) <$> (partitions (~== leadlineP) headingGroup)


parseSectionFromHeadingParagraph
  :: ChapterData -> TagList -> Either String Section
parseSectionFromHeadingParagraph chapterData paragraph = do
  let secName       = sectionNameFromParagraph paragraph
  let rawNumberText = rawNumberTextFromParagraph paragraph
  secNumber <- parseNumberFromRawNumberText rawNumberText secName
  secBody   <- parseSectionBody secNumber chapterData
  (name', number', body') <- toThreeSectionFields secName secNumber secBody
  return Section
    { Section.name       = name'
    , Section.number     = number'
    , Section.body       = body'
    -- References are attached after the whole chapter has been parsed,
    -- once the anchor registry for every mirrored page is available.
    , Section.references = []
    }


sectionNameFromParagraph :: TagList -> Text
sectionNameFromParagraph paragraph =
  normalizedInnerText $ takeWhile (~/= closingP) $ dropWhile (~/= closingA)
                                                             paragraph


rawNumberTextFromParagraph :: TagList -> Text
rawNumberTextFromParagraph paragraph =
  normalizedInnerText $ takeWhile (~/= closingA) paragraph


toThreeSectionFields
  :: Text
  -> Text
  -> Html
  -> Either String (SectionName, SectionNumber, SectionBody)
toThreeSectionFields name' number' body' = do
  name''   <- toSectionName name' [qq| $number' $body' |]
  number'' <- toSectionNumber number' [qq| $name' $body' |]
  body''   <- toSectionBody body' [qq| $name' $number' |]
  return (name'', number'', body'')


parseNumberFromRawNumberText :: Text -> Text -> Either String Text
parseNumberFromRawNumberText numberText secName = case words numberText of
  (_ : x : _) -> case toSectionNumber x [qq|name: $secName|] of
    Left  message -> Left [qq|while parsing $numberText - $message|]
    Right _       -> Right x
  _ -> Left [qq|Expected sec. $numberText $secName to have >= 2 words|]


parseSectionBody :: Text -> ChapterData -> Either String Html
parseSectionBody secNumber chapterData = sectionHtml
 where
  rawSectionGroup =
    rawSectionGroupFromSectionGroups secNumber (sectionGroups chapterData)
  sectionHtml = case rawSectionGroup of
    Right secGroup ->
      Right $ NewHtml $ "<p class=SectBody>" ++ normalizeWhiteSpace
        (renderTags $ drop 6 $ dropWhile
          (~/= ("<span class=Leadline>" :: String))
          secGroup
        )
    Left x -> Left x


rawSectionGroupFromSectionGroups :: Text -> [TagList] -> Either String TagList
rawSectionGroupFromSectionGroups secNumber secGroups =
  let bodyNumbers = filter (isSectionBodyNumber secNumber) secGroups
  in  case bodyNumbers of
        (x : _) -> Right $ shaveBackTagsToLastClosingP x
        _ ->
          Left
            [qq|Couldn't find sec. body for num. "$secNumber" in sec. groups.|]


isSectionBodyNumber :: Text -> TagList -> Bool
isSectionBodyNumber secNumber sectionGroup =
  parseSectionBodyNumber sectionGroup == secNumber


parseSectionBodyNumber :: TagList -> Text
parseSectionBodyNumber sectionGroup =
  innerText $ takeWhile (~/= ("</span>" :: String)) sectionGroup


-- Walk already-parsed sections in order, extract the ordered in-domain
-- references from each body, and emit one diagnostic per rejected link.
-- A bad link never invalidates its section: every input section comes
-- back, with references attached or an empty list.
--
-- Diagnostics deliberately carry no chapter or section number so the
-- existing failure statistics and exit semantics stay unchanged: a link
-- problem must not count as either a failed chapter or a failed section.
-- The source page and section number remain available in the message.
attachSectionReferences
  :: Monad m
  => AnchorRegistry
  -> RelativePath
  -> [Section]
  -> ParseResultT m [Section]
attachSectionReferences registry sourcePath parsedSections =
  mapM attachOne parsedSections
 where
  attachOne section = do
    let (refs, diagnostics) =
          References.extractSectionReferences
            registry
            sourcePath
            (sectionBodyHtml section)
    mapM_ (tell . pure . toFailure section) diagnostics
    pure section { Section.references = dedupeInOrder refs }
  sectionBodyHtml = NewHtml . T.pack . show . Section.body
  toFailure section diagnostic =
    parseFailure
      (T.pack (relativeToFilePath sourcePath))
      (linkDiagnosticMessage section diagnostic)


-- Repeated links inside one section collapse to their first occurrence;
-- document order is otherwise preserved, so the index is stable.
dedupeInOrder :: [SectionReference] -> [SectionReference]
dedupeInOrder = go []
 where
  go _   []           = []
  go acc (ref : rest)
    | ref `elem` acc = go acc rest
    | otherwise      = ref : go (ref : acc) rest


linkDiagnosticMessage :: Section -> LinkDiagnostic -> Text
linkDiagnosticMessage section diagnostic =
  [qq|$label link in section $sectionNumberText: href="$hrefText" text="$linkTextText"|]
 where
  label = case issue diagnostic of
    OffSiteLink      -> "Off-site" :: Text
    MalformedLink    -> "Malformed"
    UnresolvedTarget -> "Unresolved target for"
  sectionNumberText = show (Section.number section)
  hrefText          = ReferenceParser.rawHref diagnostic
  linkTextText      = ReferenceParser.linkText diagnostic
