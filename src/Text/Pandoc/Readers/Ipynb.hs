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
   Module      : Text.Pandoc.Readers.Ipynb
   Copyright   : Copyright (C) 2019 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

Ipynb (Jupyter notebook JSON format) reader for pandoc.
-}
module Text.Pandoc.Readers.Ipynb ( readIpynb )
where
import Prelude
import Text.Pandoc.Options
import Text.Pandoc.Builder
import qualified Data.Text.Lazy.Encoding as TLE
import Data.Aeson as Aeson

readIpynb :: PandocMonad m => ReaderOptions -> Text -> m Pandoc
readIpynb opts t = do
  case eitherDecode (TLE.encodeUtf8 t) of
    Left err -> error "TODO FIXME"
    Right notebook -> notebookToPandoc notebook

notebookToPandoc :: Notebook -> Pandoc
notebookToPandoc notebook =
  undefined
 
