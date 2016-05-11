module MetaData where

import Data.Int
import Data.Word
import Text.JSON

data MetaData = MetaData {
  static_data :: [Word8],      -- Static data. Will be loaded at min. address.
  root_process_size :: Int32,  -- Size of the stack frame for the root process.
  assembly_file :: String      -- Name of the corresponding assembly file.
}

encode :: MetaData -> String
encode = Text.JSON.encode . jsonify

jsonify :: MetaData -> JSObject JSValue
jsonify m =
  toJSObject [
    ("static_data", showJSON $ static_data m),
    ("root_process_size", showJSON $ root_process_size m),
    ("assembly_file", showJSON $ assembly_file m)]
