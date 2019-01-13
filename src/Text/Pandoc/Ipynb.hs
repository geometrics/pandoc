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
We only support v4.  To convert an older notebook to v4 use nbconvert:
@ipython nbconvert --to=notebook testnotebook.ipynb@.
-}
module Text.Pandoc.Ipynb ( )
where
import Prelude
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import qualified Data.Text as T
import Data.ByteString (ByteString)
import Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.Vector as V
import Control.Applicative ((<|>))
import Control.Monad (when)
import qualified Data.ByteString.Base64 as Base64

--- for testing only, to remove:
import qualified Data.ByteString.Lazy as BL

readNotebookFile :: FilePath -> IO Notebook
readNotebookFile fp = do
  bs <- BL.readFile fp
  case eitherDecode bs of
    Right nb -> return nb
    Left err -> error err

writeNotebookFile :: FilePath -> Notebook -> IO ()
writeNotebookFile fp = BL.writeFile fp . encode
---


data Notebook = Notebook
  { nbMetadata    :: JSONMeta
  , nbFormat      :: Int
  , nbFormatMinor :: Int
  , nbCells       :: [Cell]
  } deriving (Show)

instance FromJSON Notebook where
  parseJSON = withObject "Notebook" $ \v -> do
     format <- v .: "nbformat"
     when (format /= 4) $ fail "Only v4 of the nbconvert format is supported" 
     formatMinor <- v .: "nbformat_minor"
     meta <- v .: "metadata"
     cells <- v .: "cells"
     return Notebook
      { nbMetadata    = meta
      , nbFormat      = format
      , nbFormatMinor = formatMinor
      , nbCells       = cells }

instance ToJSON Notebook where
  toJSON nb =
    object $
      [ "metadata" .= nbMetadata nb
      , "nbformat" .= nbFormat nb
      , "nbformat_minor" .= nbFormatMinor nb
      , "cells" .= nbCells nb ]

type JSONMeta = M.Map Text Value

data Cell = Cell
  { cellType           :: CellType
  , cellMetadata       :: JSONMeta
  , cellText           :: Text
  , cellExecutionCount :: Maybe Int
  , cellOutputs        :: Maybe [Output]
  , cellAttachments    :: Maybe MimeBundle
} deriving (Show)

instance FromJSON Cell where
  parseJSON = withObject "Cell" $ \v ->
    Cell
      <$> v .: "cell_type"
      <*> v .: "metadata"
      <*> (v .: "source" <|> (mconcat <$> v .: "source"))
      <*> v .:? "execution_count"
      <*> v .:? "outputs"
      <*> v .:? "attachments"

instance ToJSON Cell where
  toJSON cell =
    object [ "cell_type" .= cellType cell
           , "source" .= toLines (cellText cell)
           , "metadata" .= cellMetadata cell
           , "attachments" .= cellAttachments cell
           , "outputs" .= cellOutputs cell
           ]

data CellType =
    MarkdownCell
  | RawCell
  | CodeCell
  deriving (Show)

instance FromJSON CellType where
  parseJSON (String "markdown") = return MarkdownCell
  parseJSON (String "raw") = return RawCell
  parseJSON (String "code") = return CodeCell
  parseJSON (String x) = fail $ "Unknown cell_type: " ++ T.unpack x
  parseJSON _ = fail "Unknown cell_type"

instance ToJSON CellType where
  toJSON MarkdownCell = String "markdown"
  toJSON RawCell = String "raw"
  toJSON CodeCell = String "code"

data OutputType =
    Stream
  | DisplayData
  | ExecuteResult
  deriving (Show)

instance FromJSON OutputType where
  parseJSON (String "stream") = return Stream
  parseJSON (String "display_data") = return DisplayData
  parseJSON (String "execute_result") = return ExecuteResult
  parseJSON (String x) = fail $ "Unknown output_type: " ++ T.unpack x
  parseJSON _ = fail "Unknown output_type"

instance ToJSON OutputType where
  toJSON Stream = String "stream"
  toJSON DisplayData = String "display_data"
  toJSON ExecuteResult = String "execute_result"

newtype MimeBundle = MimeBundle{ unMimeBundle :: M.Map MimeType MimeData }
  deriving (Show)

data Output = Output{
    outputType         :: OutputType
  , outputText         :: Maybe Text
  , outputName         :: Maybe Text
  , outputData         :: Maybe MimeBundle
  , outputMetadata     :: Maybe (M.Map MimeType JSONMeta)
  , outputExecuteCount :: Maybe Int
  } deriving (Show)

instance FromJSON Output where
  parseJSON = withObject "Output" $ \v ->
    Output
      <$> v .: "output_type"
      <*> v .:? "text"
      <*> v .:? "name"
      <*> v .:? "data"
      <*> v .:? "metadata"
      <*> v .:? "execution_count"

mimeDataToPair :: MimeData -> (MimeType, MimeData)
mimeDataToPair x@(BinaryData mt _) = (mt, x)
mimeDataToPair x@(TextualData t) = ("text/plain", x)
mimeDataToPair x@(JsonData v) = ("text/json", x)

instance ToJSON Output where
  toJSON o = object $
    ("output_type" .= (outputType o)) :
    maybe [] (\x -> ["text" .= x]) (outputText o) ++
    maybe [] (\x -> ["name" .= x]) (outputName o) ++
    maybe [] (\x -> ["data" .= x]) (outputData o) ++
    maybe [] (\x -> ["metadata" .= x]) (outputMetadata o) ++
    maybe [] (\x -> ["execution_count" .= x]) (outputMetadata o)

type MimeType = Text

data MimeData =
    BinaryData MimeType ByteString
  | TextualData Text
  | JsonData Value
  deriving (Show)

instance FromJSON MimeBundle where
  parseJSON v = do
    m <- parseJSON v >>= mapM pairToMimeData . M.toList
    return $ MimeBundle $ M.fromList m

pairToMimeData :: (MimeType, Value) -> Aeson.Parser (MimeType, MimeData)
pairToMimeData ("text/plain", v) = do
  t <- parseJSON v <|> (mconcat <$> parseJSON v)
  return $ ("text/plain", TextualData t)
pairToMimeData ("text/json", v) = return $ ("text/json", JsonData v)
pairToMimeData (mt, v) = do
  t <- parseJSON v <|> (mconcat <$> parseJSON v)
  return (mt, BinaryData mt (Base64.decodeLenient . TE.encodeUtf8 $ t))

instance ToJSON MimeBundle where
  toJSON (MimeBundle m) =
    let mimeBundleToValue (BinaryData mt bs) =
          toJSON (toLines $ TE.decodeUtf8 $ Base64.joinWith "\n" 64 $
                  Base64.encode bs)
        mimeBundleToValue (JsonData v) = v
        mimeBundleToValue (TextualData t) = toJSON (toLines t)
    in  toJSON $ M.map mimeBundleToValue m

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

