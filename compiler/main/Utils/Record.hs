module Utils.Record where
import Infer.Type
import qualified Data.Map as Map
import Utils.Hash (generateHashFromPath)
import Data.Hashable (hash)


chars :: [String]
chars = ("f_"++) . show <$> [0..]


generateRecordPredsAndType :: FilePath -> String -> [String] -> ([Pred], Type)
generateRecordPredsAndType astPath interfaceName fieldNames =
  let moduleHash = generateHashFromPath astPath
      fieldNamesWithVars = zip fieldNames chars
      fields             = TVar . ((`TV` Star) . hash) <$> Map.fromList fieldNamesWithVars
      -- TODO: Do we need this?
      -- fields             = TVar . (`TV` Star) . (++ moduleHash) <$> Map.fromList fieldNamesWithVars
      recordType         = TRecord fields Nothing mempty
      instPreds          = (\var -> IsIn interfaceName [var] Nothing) <$> Map.elems fields
  in  (instPreds, recordType)
