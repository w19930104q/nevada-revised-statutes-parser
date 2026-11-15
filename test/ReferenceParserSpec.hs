{-# LANGUAGE ExtendedDefaultRules #-}
{-# OPTIONS_GHC -Wno-x-partial #-}

module ReferenceParserSpec where

import           BasicPrelude
import qualified Data.HashMap.Lazy             as HM
import qualified Data.Set                      as Set
import qualified Data.Text                     as T
import           Test.Hspec
import           Text.HTML.TagSoup

import           FileUtil                       ( RelativePath
                                                , (./)
                                                )
import           HtmlUtil                       ( makeHtml )
import           Models.References              ( ReferenceTargetType(..)
                                                , fragment
                                                , href
                                                , linkText
                                                , targetType
                                                )
import qualified ReferenceParser                as RP
import           ReferenceParser                ( AnchorRegistry
                                                , LinkIssue(..)
                                                , ReferenceResolution(..)
                                                )


spec :: SpecWith ()
spec = parallel $ do
  describe "isNrsPagePath" $ do
    it "accepts chapter pages under /NRS/" $ do
      RP.isNrsPagePath "/NRS/NRS-388B.html" `shouldBe` True
      RP.isNrsPagePath "/NRS/NRS-000.html" `shouldBe` True

    it "rejects same-host pages outside the NRS directory" $ do
      RP.isNrsPagePath "/Statutes/59th/Stats197701.html" `shouldBe` False
      RP.isNrsPagePath "/Const/NvConst.html" `shouldBe` False
      RP.isNrsPagePath "/nrs/NRS-001.html" `shouldBe` False

  describe "parseNrsPagePath" $ do
    it "turns an NRS path into the mirrored relative filename" $
      RP.parseNrsPagePath "/NRS/NRS-432B.html"
        `shouldBe` Just ((./) "NRS-432B.html")

    it "rejects non-NRS paths" $
      RP.parseNrsPagePath "/Statutes/x.html" `shouldBe` Nothing

  describe "classifyBodyLinks" $ do
    it "resolves relative hrefs against the source page and keeps order" $ do
      let results = RP.classifyBodyLinks registry sourcePage (parseTags bodyHtml)
      length results `shouldBe` 10
      let refs = [r | InDomain r <- results]
      length refs `shouldBe` 4
      map href refs `shouldBe`
        [ "#NRS777Sec020"
        , "NRS-036.html#NRS036Sec010"
        , "../NRS/NRS-036.html#NRS036"
        , "../NRS/NRS-036.html"
        ]
      map targetType refs `shouldBe`
        [ SectionReferenceTarget
        , SectionReferenceTarget
        , ChapterReferenceTarget
        , NrsDocumentReferenceTarget
        ]
      map fragment refs `shouldBe`
        [ "NRS777Sec020"
        , "NRS036Sec010"
        , "NRS036"
        , ""
        ]
      linkText (head refs) `shouldBe` "NRS 777.020"

    it "preserves the raw href and link text on every reference" $ do
      let results = RP.classifyBodyLinks registry sourcePage (parseTags bodyHtml)
          refs    = [r | InDomain r <- results]
      all (not . T.null . href) refs `shouldBe` True
      all (not . T.null . linkText) refs `shouldBe` True

    it "diagnoses off-site, malformed and unresolved links separately" $ do
      let results = RP.classifyBodyLinks registry sourcePage (parseTags bodyHtml)
          issues  = [RP.issue d | LinkProblem d <- results]
      issues `shouldBe`
        [ OffSiteLink
        , OffSiteLink
        , MalformedLink
        , OffSiteLink
        , UnresolvedTarget
        , UnresolvedTarget
        ]
      let diags = [d | LinkProblem d <- results]
      map RP.rawHref diags `shouldBe`
        [ "https://example.test/elsewhere"
        , "../Statutes/59th/Stats197701.html#Stats197701page181"
        , "http://[bad"
        , "../Const/NvConst.html#Art6Sec4"
        , "../NRS/NRS-998.html#NRS998Sec010"
        , "../NRS/NRS-998.html#NRS998"
        ]

    it "treats an in-domain link to an unknown page as unresolved" $ do
      let results =
            RP.classifyBodyLinks registry sourcePage
              (parseTags "<a href=\"../NRS/NRS-998.html#NRS998\">x</a>")
      [LinkProblem diag] <- pure results
      RP.issue diag `shouldBe` UnresolvedTarget
      RP.rawHref diag `shouldBe` "../NRS/NRS-998.html#NRS998"

    it "treats a missing fragment anchor on a present page as unresolved" $ do
      let results =
            RP.classifyBodyLinks registry sourcePage
              (parseTags "<a href=\"NRS-036.html#NRS036Sec999\">x</a>")
      [LinkProblem diag] <- pure results
      RP.issue diag `shouldBe` UnresolvedTarget

    it "resolves a fragmentless link whenever the page is in the registry" $ do
      let results =
            RP.classifyBodyLinks registry sourcePage
              (parseTags "<a href=\"NRS-036.html\">x</a>")
      [InDomain ref] <- pure results
      targetType ref `shouldBe` NrsDocumentReferenceTarget
      fragment ref `shouldBe` ""

  describe "buildAnchorRegistry" $ do
    it "indexes the named anchors of every page" $ do
      let built = RP.buildAnchorRegistry
            [ ((./) "NRS-036.html", makeHtml
                "<a name=\"NRS036\"></a><a name=\"NRS036Sec010\"></a>")
            ]
          anchors = built HM.! ((./) "NRS-036.html")
      Set.member "NRS036" anchors `shouldBe` True
      Set.member "NRS036Sec010" anchors `shouldBe` True
      Set.member "missing" anchors `shouldBe` False


--
-- Helpers and fixtures
--

sourcePage :: RelativePath
sourcePage = (./) "NRS-777.html"

registry :: AnchorRegistry
registry = HM.fromList
  [ ( (./) "NRS-777.html"
    , Set.fromList ["NRS777", "NRS777Sec010", "NRS777Sec020", "NRS777Sec030"]
    )
  , ( (./) "NRS-036.html"
    , Set.fromList ["NRS036", "NRS036Sec010"]
    )
  ]

bodyHtml :: Text
bodyHtml =
  "<a href=\"#NRS777Sec020\">NRS 777.020</a>"
    <> "<a href=\"NRS-036.html#NRS036Sec010\">NRS 36.010</a>"
    <> "<a href=\"../NRS/NRS-036.html#NRS036\">chapter 36</a>"
    <> "<a href=\"../NRS/NRS-036.html\">the chapter 36 document</a>"
    <> "<a href=\"https://example.test/elsewhere\">outside the NRS</a>"
    <> "<a href=\"../Statutes/59th/Stats197701.html#Stats197701page181\">1977, 181</a>"
    <> "<a href=\"http://[bad\">broken</a>"
    <> "<a href=\"../Const/NvConst.html#Art6Sec4\">constitution</a>"
    <> "<a href=\"../NRS/NRS-998.html#NRS998Sec010\">NRS 998.010</a>"
    <> "<a href=\"../NRS/NRS-998.html#NRS998\">chapter 998</a>"


main :: IO ()
main = hspec spec
