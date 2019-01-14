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
   Module      : Text.Pandoc.Writers.Ipynb
   Copyright   : Copyright (C) 2019 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

Ipynb (Jupyter notebook JSON format) writer for pandoc.
-}
module Text.Pandoc.Writers.Ipynb ( writeIpynb )
where
import Prelude
import Text.Pandoc.Options
import Text.Pandoc.Builder
import Text.Pandoc.Ipynb
import Text.Pandoc.Class
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Lazy as BL
import Data.Aeson as Aeson

writeIpynb :: PandocMonad m => WriterOptions -> Pandoc -> m Text
writeIpynb opts doc = do
  notebook <- pandocToNotebook opts doc
  return $ TE.decodeUtf8 $ BL.toStrict $ encode notebook

pandocToNotebook :: PandocMonad m => WriterOptions -> Pandoc -> m Notebook
pandocToNotebook doc =
  undefined
 
