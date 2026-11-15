{-# LANGUAGE ExtendedDefaultRules #-}
{-# OPTIONS_GHC -Wno-x-partial #-}

module SectionReferencesSpec where

import           BasicPrelude
import           Control.Monad.Writer.Strict    ( runWriterT )
import           Data.Foldable                  ( toList )
import qualified Data.Aeson                    as Aeson
import qualified Data.Aeson.KeyMap             as KM
import qualified Data.HashMap.Lazy             as HM
import qualified Data.Text                     as T
import           Test.Hspec

import           ChapterFile
import           FileUtil                       ( (./) )
import           HtmlUtil
import           Models.Chapter                as Chapter
import           Models.ParseFailure            ( ParseFailure
                                                , chapterNumber
                                                , message
                                                , sectionNumber
                                                , sourceFile
                                                )
import           Models.References
import           Models.Section                as Section
import           Models.SubChapter             as SubChapter
import           Models.SubSubChapter          as SubSubChapter


spec :: SpecWith ()
spec = parallel $ describe "section cross references" $ do

  it "attaches ordered references while keeping every section" $ do
    (chapter, fails) <- parseReferenceFixture
    let sections = simpleSections chapter
    map (show . Section.number) sections `shouldBe`
      ["777.010", "777.020", "777.030"]
    -- Bad links never drop a section: link diagnostics carry no section
    -- number, so every parsed section still appears in the tree.
    all isNothing (map sectionNumber fails) `shouldBe` True

  it "extracts nested relative, fragment and document references" $ do
    (chapter, _) <- parseReferenceFixture
    let firstSection = head (simpleSections chapter)
        refs          = Section.references firstSection
    map targetType refs `shouldBe`
      [ SectionReferenceTarget
      , SectionReferenceTarget
      , ChapterReferenceTarget
      , NrsDocumentReferenceTarget
      ]
    map href refs `shouldBe`
      [ "#NRS777Sec020"
      , "NRS-036.html#NRS036Sec010"
      , "../NRS/NRS-036.html#NRS036"
      , "../NRS/NRS-036.html"
      ]
    map fragment refs `shouldBe`
      ["NRS777Sec020", "NRS036Sec010", "NRS036", ""]
    map linkText refs `shouldBe`
      [ "NRS 777.020"
      , "NRS 36.010"
      , "chapter 36"
      , "the chapter 36 document"
      ]

  it "collapses duplicate references but keeps first-document order" $ do
    (chapter, _) <- parseReferenceFixture
    let secondSection = simpleSections chapter !! 1
        refs          = Section.references secondSection
    length refs `shouldBe` 1
    href (head refs) `shouldBe` "#NRS777Sec010"
    linkText (head refs) `shouldBe` "NRS 777.010"

  it "leaves an empty reference list when every link is unresolvable" $ do
    (chapter, _) <- parseReferenceFixture
    let thirdSection = simpleSections chapter !! 2
    Section.references thirdSection `shouldBe` []

  it "diagnoses off-site, malformed and unresolved links distinctly" $ do
    (_, fails) <- parseReferenceFixture
    let messages = map message fails
    length fails `shouldBe` 6
    length (filter ("Off-site" `T.isInfixOf`) messages) `shouldBe` 3
    length (filter ("Malformed" `T.isInfixOf`) messages) `shouldBe` 1
    length (filter ("Unresolved" `T.isInfixOf`) messages) `shouldBe` 2
    -- Diagnostics carry the source page but no chapter or section
    -- number, so summary statistics stay unchanged.
    nub (map sourceFile fails) `shouldBe` ["NRS-777.html"]
    all isNothing (map chapterNumber fails) `shouldBe` True
    all isNothing (map sectionNumber fails) `shouldBe` True

  describe "nested chapters" $ do
    it "attaches references through sub-chapters and sub-sub-chapters" $ do
      chapterHtml <- htmlFixture "NRS-432B.html"
      let chapterMap = HM.singleton ((./) "NRS-432B.html") chapterHtml
          registry   = buildReferenceRegistry chapterMap
      case runWriterT
        (parseChapter chapterHtml
          >>= enrichChapterReferences registry ((./) "NRS-432B.html")) of
        Left err -> expectationFailure err
        Right (chapter, fails) -> do
          let sections = nestedSections chapter
          -- The fixture's 12 sub-chapters (some with sub-sub-chapters)
          -- all survive; link diagnostics never remove a section.
          length sections `shouldBe` 183
          let withRefs = filter (not . null . Section.references) sections
          length withRefs `shouldSatisfy` (> 100)
          let firstSection = head sections
          map href (Section.references firstSection) `shouldBe`
            [ "#NRS432BSec020"
            , "#NRS432BSec110"
            ]
          all isNothing (map sectionNumber fails) `shouldBe` True
          all isNothing (map chapterNumber fails) `shouldBe` True

  it "keeps the body HTML byte-for-byte and only appends a references field" $ do
    (chapter, _) <- parseReferenceFixture
    let firstSection = head (simpleSections chapter)
        bodyString   = show (Section.body firstSection)
    bodyString `shouldSatisfy` isInfixOf "NRS 777.020"
    bodyString `shouldSatisfy` isInfixOf "href=\"#NRS777Sec020\""

  it "emits references as stable JSON with the documented field names" $ do
    (chapter, _) <- parseReferenceFixture
    let firstSection = head (simpleSections chapter)
    case Aeson.toJSON firstSection of
      Aeson.Object obj -> do
        obj KM.!? "name" `shouldSatisfy` isJust
        obj KM.!? "number" `shouldSatisfy` isJust
        obj KM.!? "body" `shouldSatisfy` isJust
        case obj KM.!? "references" of
          Just (Aeson.Array refs) -> do
            length refs `shouldBe` 4
            case toList refs of
              (firstRef : _) -> case firstRef of
                Aeson.Object refObj -> do
                  refObj KM.!? "linkText" `shouldBe`
                    Just (Aeson.String "NRS 777.020")
                  refObj KM.!? "fragment" `shouldBe`
                    Just (Aeson.String "NRS777Sec020")
                  refObj KM.!? "href" `shouldBe`
                    Just (Aeson.String "#NRS777Sec020")
                  refObj KM.!? "targetType" `shouldBe`
                    Just (Aeson.String "section")
                other -> expectationFailure ("expected object, got " ++ show other)
              [] -> expectationFailure "expected a non-empty references array"
          other -> expectationFailure ("expected array, got " ++ show other)
      other -> expectationFailure ("expected object, got " ++ show other)


--
-- Helpers
--

parseReferenceFixture :: IO (Chapter, [ParseFailure])
parseReferenceFixture = do
  chapterHtml <- htmlFixture "NRS-777.html"
  targetHtml  <- htmlFixture "NRS-036.html"
  let chapterMap = HM.fromList
        [ ((./) "NRS-777.html", chapterHtml)
        , ((./) "NRS-036.html", targetHtml)
        ]
      registry = buildReferenceRegistry chapterMap
  case runWriterT
    (parseChapter chapterHtml
      >>= enrichChapterReferences registry ((./) "NRS-777.html")) of
    Left err     -> error err
    Right parsed -> pure parsed


simpleSections :: Chapter -> [Section]
simpleSections chapter = case Chapter.content chapter of
  SimpleChapterContent sections -> sections
  ComplexChapterContent xs      ->
    error ("expected sections, got subchapters " ++ show xs)


-- Flatten every section of a complex chapter, walking sub-chapters and
-- their sub-sub-chapters in tree order.
nestedSections :: Chapter -> [Section]
nestedSections chapter = case Chapter.content chapter of
  SimpleChapterContent sections -> sections
  ComplexChapterContent subs    -> concatMap subChapterSections subs
 where
  subChapterSections sc = case SubChapter.children sc of
    SubChapterSections sections -> sections
    SubSubChapters subSubs ->
      concatMap SubSubChapter.sections subSubs


main :: IO ()
main = hspec spec
