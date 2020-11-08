module Constants.Flags where

import           Options.Applicative -- provided by optparse-applicative

-- `madlib transform` takes:
-- 1. an `input` which is one of (file / directory / stdin)
-- 2. an `output` which is of the same type as the input (f -> f / d -> d / stdin -> stdout)
-- 3. a `config` file which is yaml / json based: defaults to `madlib.yaml`
-- 4. a `literate` option: all / code / no-code
-- [VALID] examples
-- madlib transform --input ./some-input-directory --output ./some-output-directory
-- madlib transform --config ./custom-madlib-config.yaml -i src -o build
-- madlib transform --input ./a-file.mad --output ./a-file.js
-- madlib transform --i ./a-file.mad -o ./a-file.js
-- madlib tranform -i src -o docs --literate no-code
-- madlib tranform -i src -o build --literate code

-- cat ./a-file.mad | madlib transform -o ./a-file.js
-- cat ./a-file.mad | madlib transform > ./a-file.js

-- [INVALID] examples
-- madlib transform --input ./some-input-directory --output ./a-file.js

-- TRANSFORM
data FlagInput = FileInput FilePath | StdInput deriving (Show)
-- type FlagOutput = String
data FlagOutput = FileOutput FilePath | StdOutput deriving (Show)
data FlagConfig = FileConfig FilePath deriving (Show)
data FlagLiterate = LiterateAll | LiterateCode | LiterateNoCode deriving (Bounded, Enum, Show)

-- LINT
data Severity = Info | Warning | Error deriving (Bounded, Enum, Show)
