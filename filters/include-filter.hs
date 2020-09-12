#! /usr/bin/env runghc
-- | A Pandoc filter to include file content in code blocks with the "include"
-- attribute.
import Text.Pandoc.JSON

import qualified Data.Text as T
import qualified Data.Text.IO as T


--------------------------------------------------------------------------------
main :: IO ()
main = toJSONFilter doInclude


--------------------------------------------------------------------------------
doInclude :: Block -> IO Block
doInclude block@(CodeBlock (id, classes, namevals) contents) =
  case lookup (T.pack "include") namevals of
    Just f  -> CodeBlock (id, classes, namevals) <$> T.readFile (T.unpack f)
    Nothing -> return block
doInclude block = return block
