module ReferenceParser
  ( LinkDiagnostic(..)
  , LinkIssue(..)
  , ReferenceResolution(..)
  , AnchorRegistry
  , buildAnchorRegistry
  , extractSectionReferences
  , classifyBodyLinks
  , nrsHost
  , nrsUrlBase
  , parseNrsPagePath
  , isNrsPagePath
  , relativeSourcePath
  )
where

import           BasicPrelude
import qualified Data.HashMap.Lazy             as HM
import qualified Data.Set                      as Set
import qualified Data.Text                     as T
import           Network.URI                    ( URI(..)
                                                , URIAuth(..)
                                                , nullURI
                                                , parseURI
                                                , parseURIReference
                                                , relativeTo
                                                )
import           Text.HTML.TagSoup

import           FileUtil                       ( RelativePath
                                                , (./)
                                                , relativeToFilePath
                                                )
import           HtmlUtil                       ( Html
                                                , toText
                                                )
import           Models.References
import           TextUtil                       ( normalizeWhiteSpace )


-- The mirrored site only contains www.leg.state.nv.us pages; links to
-- any other host are outside the NRS domain.
nrsHost :: Text
nrsHost = "www.leg.state.nv.us"

-- Every chapter page is served below this directory. A same-host link
-- outside of it (e.g. /Statutes/... or /Const/...) is not an NRS link.
nrsPathPrefix :: Text
nrsPathPrefix = "/NRS/"

-- Absolute base used to turn relative hrefs into URLs for resolution.
nrsUrlBase :: Text
nrsUrlBase = "https://" ++ nrsHost ++ "/NRS/"


-- What is wrong with a link that cannot be turned into a reference.
data LinkIssue
  = OffSiteLink
  | MalformedLink
  | UnresolvedTarget
  deriving (Eq, Show)

-- A diagnostic for a single rejected link. These never carry a section
-- number in the ParseFailure sense: one bad link must not make a valid
-- section count as failed, so the location is embedded in the message.
data LinkDiagnostic = LinkDiagnostic
  { issue    :: LinkIssue
  , rawHref  :: Text
  , linkText :: Text
  } deriving (Eq, Show)


-- The outcome of resolving one body anchor.
data ReferenceResolution
  = InDomain SectionReference
  | LinkProblem LinkDiagnostic


-- All @<a name=...>@ anchors known across the mirrored chapter pages.
-- The key is the source-relative chapter filename; the value is the set
-- of anchor names declared on that page.
type AnchorRegistry = HashMap RelativePath (Set Text)


buildAnchorRegistry :: [(RelativePath, Html)] -> AnchorRegistry
buildAnchorRegistry pages = HM.fromList
  [ (path, Set.fromList (mapMaybe anchorName (parseTags (toText html))))
  | (path, html) <- pages
  ]
 where
  anchorName tag = case tag of
    TagOpen "a" attrs -> lookup "name" attrs
    _                 -> Nothing


-- Extract the ordered references and link diagnostics from one rendered
-- section body. The source filename says which chapter page the body
-- came from, so relative hrefs resolve against the correct page.
extractSectionReferences
  :: AnchorRegistry -> RelativePath -> Html -> ([SectionReference], [LinkDiagnostic])
extractSectionReferences registry sourcePath bodyHtml =
  foldr classify ([], []) (anchorPairs (parseTags (toText bodyHtml)))
 where
  classify (anchor, inner) (refs, diags) =
    case resolveAnchor registry sourcePath anchor inner of
      InDomain ref      -> (ref : refs, diags)
      LinkProblem diag  -> (refs, diag : diags)


-- Classify every anchor in an arbitrary tag stream. Exposed for unit
-- tests that build tags directly rather than rendering a section body.
classifyBodyLinks
  :: AnchorRegistry -> RelativePath -> [Tag Text] -> [ReferenceResolution]
classifyBodyLinks registry sourcePath tags =
  uncurry (resolveAnchor registry sourcePath) <$> anchorPairs tags


-- Pair each opening anchor that carries an href with the tags nested
-- inside it. Name-only anchors (<a name=...>) are targets, not links.
anchorPairs :: [Tag Text] -> [(Tag Text, [Tag Text])]
anchorPairs = go
 where
  go [] = []
  go (tag@(TagOpen "a" attrs) : rest)
    | hasHref attrs =
        let inner = takeWhile (~/= ("</a>" :: String)) rest
            after = drop 1 (dropWhile (~/= ("</a>" :: String)) rest)
        in  (tag, inner) : go after
    | otherwise = go rest
  go (_ : rest) = go rest
  hasHref = any ((== "href") . fst)


resolveAnchor
  :: AnchorRegistry
  -> RelativePath
  -> Tag Text
  -> [Tag Text]
  -> ReferenceResolution
resolveAnchor registry sourcePath anchor inner =
  let rawHref = fromAttrib "href" anchor
      text    = normalizeWhiteSpace (innerText inner)
  in  case parseURIReference (T.unpack rawHref) of
        Nothing -> problem MalformedLink rawHref text
        Just ref ->
          let base      = fromMaybe nullURI
                (parseURI (T.unpack (nrsUrlBase ++ relativeSourcePath sourcePath)))
              uri       = ref `relativeTo` base
              host      = maybe "" (T.pack . uriRegName) (uriAuthority uri)
              path      = T.pack (uriPath uri)
              fragment  = dropFragmentMarker (T.pack (uriFragment uri))
          in  if host /= nrsHost || not (isNrsPagePath path)
                then problem OffSiteLink rawHref text
                else case parseNrsPagePath path of
                  Nothing -> problem MalformedLink rawHref text
                  Just targetPath
                    | targetResolvable registry targetPath fragment -> InDomain
                      SectionReference
                        { linkText   = text
                        , fragment   = fragment
                        , href       = rawHref
                        , targetType = classifyTarget fragment
                        }
                    | otherwise -> problem UnresolvedTarget rawHref text
 where
  problem kind href' text' =
    LinkProblem LinkDiagnostic
      { issue = kind, rawHref = href', linkText = text' }


-- Render the filename wrapped by a RelativePath (e.g. "NRS-000.html").
relativeSourcePath :: RelativePath -> Text
relativeSourcePath = T.pack . relativeToFilePath


-- A path is in the NRS domain when it is a chapter page beneath /NRS/.
isNrsPagePath :: Text -> Bool
isNrsPagePath path =
  nrsPathPrefix `T.isPrefixOf` path && isChapterFilename (basename path)


-- /NRS/NRS-388B.html -> NRS-388B.html
parseNrsPagePath :: Text -> Maybe RelativePath
parseNrsPagePath path
  | isNrsPagePath path = Just ((./) (T.unpack (basename path)))
  | otherwise          = Nothing


-- A section fragment means "section", a chapter fragment means
-- "chapter", no fragment means the whole document.
classifyTarget :: Text -> ReferenceTargetType
classifyTarget fragment
  | T.null fragment        = NrsDocumentReferenceTarget
  | isSectionFragment fragment = SectionReferenceTarget
  | otherwise              = ChapterReferenceTarget


-- A reference can only be emitted when the mirrored chapter page exists
-- and, for a fragment link, the named anchor is declared on that page.
targetResolvable :: AnchorRegistry -> RelativePath -> Text -> Bool
targetResolvable registry targetPath fragment =
  case HM.lookup targetPath registry of
    Nothing -> False
    Just anchors
      | T.null fragment -> True
      | otherwise       -> fragment `Set.member` anchors


-- Fragments naming a section look like NRS388BSec100. Chapter anchors
-- look like NRS388B. Document links have no fragment at all.
isSectionFragment :: Text -> Bool
isSectionFragment = T.isInfixOf "Sec"


basename :: Text -> Text
basename = T.reverse . T.takeWhile (/= '/') . T.reverse


isChapterFilename :: Text -> Bool
isChapterFilename name =
  "NRS-" `T.isPrefixOf` name && ".html" `T.isSuffixOf` name


dropFragmentMarker :: Text -> Text
dropFragmentMarker fragment =
  fromMaybe fragment (T.stripPrefix "#" fragment)
