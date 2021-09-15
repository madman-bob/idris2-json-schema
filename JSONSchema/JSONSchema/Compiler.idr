module JSONSchema.Compiler

import Control.Monad.Identity
import Control.Monad.Writer
import Data.SnocList

import JSONSchema.Data

asIdrisType : JSONSchema -> String
asIdrisType JSNull = "()"
asIdrisType JSBoolean = "Bool"
asIdrisType JSNumber = "Double"
asIdrisType JSString = "String"

writeSchema : (name : String) -> JSONSchema -> SnocList String
writeSchema name schema = [<"\{name} : Type" , "\{name} = \{asIdrisType schema}"]

export
compileSchema : JSONSchema -> List String
compileSchema schema = cast {from = SnocList String} $ execWriter $ do
    tell $ writeSchema "Main" schema
