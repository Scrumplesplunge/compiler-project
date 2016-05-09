module MetaData where

import Data.Int
import Data.Word
import Text.JSON

data MetaData = MetaData {
  memory_start :: Int32,
  workspace_pointer :: Int32,
  memory_size :: Int32,
  assembly_file :: String,
  static_data :: [Word8]
}

encode :: MetaData -> String
encode = Text.JSON.encode . jsonify

jsonify :: MetaData -> JSObject JSValue
jsonify m =
  toJSObject [
    ("memory_start", showJSON $ memory_start m),
    ("workspace_pointer", showJSON $ workspace_pointer m),
    ("memory_size", showJSON $ memory_size m),
    ("assembly_file", showJSON $ assembly_file m),
    ("static_data", showJSON $ static_data m)]
