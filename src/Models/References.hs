module Models.References
  ( SectionReference(..)
  , ReferenceTargetType(..)
  , referenceTargetTypeText
  )
where

import           BasicPrelude
import           Data.Aeson                     ( ToJSON
                                                , Value(String)
                                                , toJSON
                                                )
import           GHC.Generics                   ( Generic )


-- One ordered, in-NRS-domain link found inside a section body.
--
-- * linkText: rendered text of the anchor, whitespace-normalized
-- * fragment: the part after @#@ (without the @#@), or empty
-- * href: the href exactly as it appeared in the source HTML
-- * targetType: what the link points at, once its relative path is resolved
data SectionReference = SectionReference
  { linkText   :: Text
  , fragment   :: Text
  , href       :: Text
  , targetType :: ReferenceTargetType
  } deriving (Eq, Generic, Show)

instance ToJSON SectionReference


-- What an in-domain link resolves to. The string codes are part of the
-- emitted JSON, so they stay stable even though the Haskell names move.
data ReferenceTargetType
  = SectionReferenceTarget
  | ChapterReferenceTarget
  | NrsDocumentReferenceTarget
  deriving (Eq, Generic, Show)

referenceTargetTypeText :: ReferenceTargetType -> Text
referenceTargetTypeText SectionReferenceTarget      = "section"
referenceTargetTypeText ChapterReferenceTarget      = "chapter"
referenceTargetTypeText NrsDocumentReferenceTarget  = "nrs-document"

instance ToJSON ReferenceTargetType where
  toJSON = String . referenceTargetTypeText
