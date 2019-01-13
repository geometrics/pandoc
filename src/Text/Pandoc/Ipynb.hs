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
-}
module Text.Pandoc.Ipynb ( )
where
import Prelude
import qualified Data.Map as M
import qualified Data.HashMap.Strict as HM
import Data.Text (Text)
import qualified Data.Text as T
import Data.ByteString (ByteString)
import Data.Aeson as Aeson
import qualified Data.Vector as V
import Control.Applicative ((<|>))

--- for testing only, to remove:
import qualified Data.ByteString.Lazy as BL

readNotebookFile :: FilePath -> IO (Notebook a)
readNotebookFile fp = do
  bs <- BL.readFile fp
  case eitherDecode bs of
    Right nb -> return nb
    Left err -> error err

printNotebook :: Notebook a -> IO ()
printNotebook = BL.putStr . encode
---

data NbVer3
data NbVer4

data Notebook a = Notebook
  { nbMetadata    :: JSONMeta
  , nbFormat      :: Int
  , nbFormatMinor :: Int
  , nbCells       :: [Cell a]
  } deriving (Show)

instance FromJSON (Notebook a) where
  parseJSON = withObject "Notebook" $ \v -> do
     format <- v .: "nbformat"
     formatMinor <- v .: "nbformat_minor"
     meta <- v .: "metadata"
     cells <- if format >= 4
                 then v .: "cells"
                 else do
                   (worksheets :: [Object]) <- v .: "worksheets"
                   -- collapse multiple worksheets into one
                   concat <$>
                     mapM (\worksheet -> worksheet .: "cells") worksheets
     return Notebook
      { nbMetadata    = meta
      , nbFormat      = format
      , nbFormatMinor = formatMinor
      , nbCells       = cells }

instance ToJSON (Notebook a) where
  toJSON nb =
    object $
      [ "metadata" .= nbMetadata nb
      , "nbformat" .= nbFormat nb
      , "nbformat_minor" .= nbFormatMinor nb
      ] ++
      if nbFormat nb >= 4
         then [ "cells" .= nbCells nb ]
         else [ "worksheets" .= ([ "cells" .= nbCells nb ] :: [(Text, Value)]) ]

type JSONMeta = M.Map Text Value

data Cell a = Cell
  { cellType           :: CellType a
  , cellMetadata       :: JSONMeta
  , cellText           :: Text
  , cellExecutionCount :: Maybe Int
  , cellOutputs        :: [Output a]
  , cellAttachments    :: M.Map Text MimeBundle
} deriving (Show)

instance FromJSON (Cell a) where
  parseJSON = withObject "Cell" $ \v ->
    Cell
      <$> v .: "cell_type"
      <*> v .: "metadata"
      <*> (v .: "source" <|> (mconcat <$> v .: "source"))
      <*> v .:? "execution_count"
      <*> v .:? "outputs" .!= mempty
      <*> v .:? "attachments" .!= mempty

instance ToJSON (Cell a) where
  toJSON cell =
    object [ "cell_type" .= cellType cell
           , "source" .= toLines (cellText cell)
           , "metadata" .= cellMetadata cell
           , "attachments" .= cellAttachments cell
           , "outputs" .= cellOutputs cell ]

data CellType a =
    MarkdownCell
  | RawCell
  | CodeCell
  | HeadingCell -- v3 only
  deriving (Show)

instance FromJSON (CellType a) where
  parseJSON (String "markdown") = return MarkdownCell
  parseJSON (String "raw") = return RawCell
  parseJSON (String "code") = return CodeCell
  parseJSON (String "heading") = return HeadingCell
  parseJSON (String x) = fail $ "Unknown cell_type: " ++ T.unpack x
  parseJSON _ = fail "Unknown cell_type"

instance ToJSON (CellType a) where
  toJSON MarkdownCell = String "markdown"
  toJSON RawCell = String "raw"
  toJSON CodeCell = String "code"
  toJSON HeadingCell = String "heading"

data OutputType a =
    Stream
  | DisplayData
  | ExecuteResult
  deriving (Show)

instance FromJSON (OutputType a) where
  parseJSON (String "stream") = return Stream
  parseJSON (String "display_data") = return DisplayData
  parseJSON (String "execute_result") = return ExecuteResult
  parseJSON (String x) = fail $ "Unknown output_type: " ++ T.unpack x
  parseJSON _ = fail "Unknown output_type"
-- TODO what about pyout? v3

instance ToJSON (OutputType a) where
  toJSON Stream = String "stream"
  toJSON DisplayData = String "display_data"
  toJSON ExecuteResult = String "execute_result"

data Output a = Output{
    outputType         :: OutputType a
  , outputText         :: Maybe Text
  , outputName         :: Maybe Text
  , outputData         :: Maybe (M.Map MimeType MimeData)
  , outputMetadata     :: Maybe (M.Map MimeType JSONMeta)
  , outputExecuteCount :: Maybe Int
  } deriving (Show)

instance FromJSON (Output a) where
  parseJSON = withObject "Output" $ \v ->
    Output
      <$> v .: "output_type"
      <*> v .:? "text"
      <*> v .:? "name"
      <*> v .:? "data"
      <*> v .:? "metadata"
      <*> v .:? "execution_count"
      
instance ToJSON (Output a) where
  toJSON o = object $
    ("output_type" .= (outputType o)) :
    maybe [] (\x -> ["text" .= x]) (outputText o) ++
    maybe [] (\x -> ["name" .= x]) (outputName o) ++
    maybe [] (\x -> ["data" .= x]) (outputData o) ++
    maybe [] (\x -> ["metadata" .= x]) (outputMetadata o) ++
    maybe [] (\x -> ["execution_count" .= x]) (outputMetadata o)

type MimeType = Text

data MimeBundle = MimeBundle
  { mimeData        :: M.Map MimeType MimeData
  , mimeMetadata    :: M.Map MimeType JSONMeta
  }
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

instance FromJSON MimeData where
  parseJSON = withObject "MimeData" $ \v -> undefined

instance ToJSON MimeData where
  toJSON (BinaryData mime bs) = undefined -- convert to base64
  toJSON (TextualData t) = toJSON (toLines t)
  toJSON (JsonData v) = object [ "json" .= v ]

toLines :: Text -> [Text]
toLines = map (<> "\n") . T.lines

{- -- for pandoc conversion, move:
valueToMetaValue :: Value -> MetaValue
valueToMetaValue (Object m) =
  MetaMap $ M.fromList [ (T.unpack k, valueToMetaValue v)
                       | (k,v) <- HM.toList m ]
valueToMetaValue (Array v)  = MetaList (map valueToMetaValue (V.toList v))
valueToMetaValue (String t) = MetaString (T.unpack t)
valueToMetaValue (Number n) = MetaString (show n)
valueToMetaValue (Bool b)   = MetaBool b
valueToMetaValue Aeson.Null = MetaString mempty
-}

