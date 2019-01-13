{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-
Copyright (C) 2019 John MacFarlane <jgm@berkeley.edu>

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
-}

{- |
   Module      : Text.Pandoc.Ipynb
   Copyright   : Copyright (C) 2006-2018 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

Data structure and JSON serializers for ipynb (Jupyter notebook) format.
The format is documented here:
<https://nbformat.readthedocs.io/en/latest/format_description.html>.
This module so far only works with version 4 of the spec.

TODO: handle version 3.
-}
module Text.Pandoc.Ipynb ( )
where
import Prelude
import Text.Pandoc.Definition
import qualified Data.Map as M
import qualified Data.HashMap.Strict as HM
import Data.Text (Text)
import qualified Data.Text as T
import Data.ByteString (ByteString)
import Data.Aeson as Aeson
import qualified Data.Vector as V
import Control.Applicative ((<|>))

data Notebook = Notebook
  { nbMetadata    :: NotebookMeta
  , nbFormat      :: Int
  , nbFormatMinor :: Int
  , nbCells       :: [Cell]
  } deriving (Show)

instance FromJSON Notebook where
  parseJSON = withObject "Notebook" $ \v -> Notebook
     <$> (M.map valueToMetaValue <$> v .: "metadata")
     <*> (v .: "nbformat" >>= checkVersion)
     <*> v .: "nbformat_minor"
     <*> v .: "cells"

checkVersion :: Monad m => Int -> m Int
checkVersion 4 = return 4
checkVersion _ = fail "Only version 4 of ipynb is supported currently"

instance ToJSON Notebook where
  toJSON nb =
    object [ "metadata" .= nbMetadata nb
           , "nbformat" .= nbFormat nb
           , "nbformat_minor" .= nbFormatMinor nb
           , "cells" .= nbCells nb
           ]

type NotebookMeta = M.Map String MetaValue

data Cell = Cell
  { cellMetadata    :: CellMeta
  , cellContents    :: CellContent
  , cellAttachments :: [M.Map Text MimeBundle]
} deriving (Show)

instance FromJSON Cell where
  parseJSON = withObject "Cell" $ \v -> do
    metadata <- v .: "metadata"
    source <- v .: "source" <|> (T.unlines <$> v .: "source")
    -- cellType <- v .: "cell_type"
    return $ Cell
      { cellMetadata = M.map valueToMetaValue metadata
      , cellContents = RawCell source
      , cellAttachments = mempty -- TODO
      }

instance ToJSON Cell where
  toJSON = undefined

type CellMeta = M.Map String MetaValue

valueToMetaValue :: Value -> MetaValue
valueToMetaValue (Object m) =
  MetaMap $ M.fromList [ (T.unpack k, valueToMetaValue v)
                       | (k,v) <- HM.toList m ]
valueToMetaValue (Array v)  = MetaList (map valueToMetaValue (V.toList v))
valueToMetaValue (String t) = MetaString (T.unpack t)
valueToMetaValue (Number n) = MetaString (show n)
valueToMetaValue (Bool b)   = MetaBool b
valueToMetaValue Aeson.Null = MetaString mempty

data CellContent =
    MarkdownCell [Block]
  | RawCell Text
  | CodeCell
  { codeText           :: Text
  , codeExecutionCount :: Int
  , codeOutputs        :: [CodeOutput]
  }
  deriving (Show)

instance FromJSON CellContent where
  parseJSON = undefined

instance ToJSON CellContent where
  toJSON = undefined

data CodeOutput =
    StreamOutput
    { streamType   :: Text
    , streamText   :: Text
    }
  | DisplayData
  | ExecuteResults
    { executeCount    :: Int
    , executeData     :: MimeBundle
    , executeMetadata :: MimeMetadata
    }
  deriving (Show)

instance FromJSON CodeOutput where
  parseJSON = undefined

instance ToJSON CodeOutput where
  toJSON = undefined

type MimeType = Text

type MimeMetadata = M.Map MimeType MetaValue

newtype MimeBundle = MimeBundle [MimeData]
  deriving (Show)

instance FromJSON MimeBundle where
  parseJSON = undefined

instance ToJSON MimeBundle where
  toJSON = undefined

data MimeData =
    BinaryData MimeType ByteString
  | TextualData Text
  | JsonData Value
  deriving (Show)

{-
instance FromJSON MimeData where
  parseJSON = withObject "MimeData" $ \v -> undefined

instance ToJSON MimeData where
  toJSON (BinaryData MimeType bs) = undefined -- convert to base64
  toJSON (TextualData t) = T.unlines t
  toJSON (JsonData v) = object [ "json" .= v ]
-}

