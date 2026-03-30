{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NamedFieldPuns #-}
-- | Closure conversion and lambda lifting for the LLVM backend.
--
-- This pass transforms nested function definitions that capture free variables
-- into top-level definitions with explicit parameters. At the original call site,
-- the nested function is replaced with a partial application of the lifted function
-- to the captured variables.
--
-- The pass also handles mutation tracking: variables that are reassigned are
-- heap-allocated (boxed as references) so that mutations are visible across
-- closure boundaries.
module Generate.LLVM.ClosureConvert(convertAST) where

import qualified Control.Monad.State           as MonadState
import qualified Data.Map                      as M
import qualified Data.Set                      as S
import           AST.Core
import           Infer.Type
import           Data.Maybe (mapMaybe)
import           Explain.Location (emptyArea)
import           Utils.Hash (addHashToName)
import qualified Utils.Hash                    as Hash


-- ---------------------------------------------------------------------------
-- Built-in operators (never free variables)
-- ---------------------------------------------------------------------------

builtinOperators :: S.Set String
builtinOperators = S.fromList
  [ "+", "unary-minus", "++", "-", "*", "/", "&", "|", "^", "~"
  , "<<", ">>", ">>>", "==", "!=", "!", ">", "<", ">=", "<="
  , "&&", "||", "%"
  ]

isBuiltinOrFieldAccess :: String -> Bool
isBuiltinOrFieldAccess name =
  S.member name builtinOperators || isFieldAccess name
  where
    isFieldAccess ('.' : _) = True
    isFieldAccess _         = False


-- ---------------------------------------------------------------------------
-- State and environment
-- ---------------------------------------------------------------------------

data ConvertState
  = ConvertState
  { csCount    :: !Int
  , csTopLevel :: [Exp]
    -- ^ Lifted definitions collected during conversion, prepended to the module
  }

initialState :: ConvertState
initialState = ConvertState { csCount = 0, csTopLevel = [] }

type Convert a = forall m . MonadState.MonadState ConvertState m => m a


-- | The environment threaded through conversion.
data Env
  = Env
  { envGlobalVars      :: S.Set String
    -- ^ Names visible at the top level (module-scope bindings, constructors, imports).
    --   A variable reference that is in this set is NOT a free variable.
  , envExclusions      :: S.Set String
    -- ^ Names that are excluded from the "already global" check. If a variable
    --   is both global and excluded, it IS treated as a free variable. This is used
    --   for closured names that are reassigned in an outer scope.
  , envDictsInScope    :: [String]
    -- ^ Dictionary parameters currently in scope. Used to filter out dictionary
    --   free variables that aren't actually available.
  , envStillTopLevel   :: Bool
    -- ^ Whether we are still at the module's top level. Top-level definitions
    --   don't need to be lifted.
  , envLifted          :: M.Map String (String, [Exp])
    -- ^ Map from original function name to (lifted name, captured arg expressions).
    --   When a variable reference hits this map, it's replaced with a call to the
    --   lifted version applied to the captured args.
  , envAllocatedMuts   :: [String]
    -- ^ Mutations already heap-allocated in the current scope. If a mutation
    --   is already allocated, subsequent assignments should store (not re-allocate).
  , envMutationsInScope :: [String]
    -- ^ All mutations that will happen in a function and its inner functions.
  , envModuleHash      :: String
  }

initialEnv :: Env
initialEnv = Env
  { envGlobalVars       = S.empty
  , envExclusions       = S.empty
  , envDictsInScope     = []
  , envStillTopLevel    = True
  , envLifted           = M.empty
  , envAllocatedMuts    = []
  , envMutationsInScope = []
  , envModuleHash       = ""
  }

-- | Enter a nested (non-top-level) context.
nested :: Env -> Env
nested env = env { envStillTopLevel = False }

addGlobalVar :: String -> Env -> Env
addGlobalVar name env = env { envGlobalVars = S.insert name (envGlobalVars env) }

addExclusions :: [String] -> Env -> Env
addExclusions vars env = env { envExclusions = envExclusions env `S.union` S.fromList vars }

addLiftedLambda :: String -> String -> [Exp] -> Env -> Env
addLiftedLambda originalName liftedName args env =
  env { envLifted = M.insert originalName (liftedName, args) (envLifted env) }


-- ---------------------------------------------------------------------------
-- State helpers
-- ---------------------------------------------------------------------------

generateLiftedName :: Env -> String -> Convert String
generateLiftedName env originalName = do
  let hashedName = addHashToName (envModuleHash env) originalName
  s <- MonadState.get
  let name = hashedName ++ "$lifted$" ++ show (csCount s)
  MonadState.put s { csCount = csCount s + 1 }
  return name

addTopLevelExp :: Exp -> Convert ()
addTopLevelExp e = do
  s <- MonadState.get
  MonadState.put s { csTopLevel = e : csTopLevel s }

getTopLevelExps :: Convert [Exp]
getTopLevelExps = MonadState.gets (reverse . csTopLevel)


-- ---------------------------------------------------------------------------
-- Mark captured arguments that reference mutated variables
-- ---------------------------------------------------------------------------

-- | Tag captured variable nodes with ReferenceArgument metadata when they
-- refer to mutated variables. This tells codegen to pass them by reference.
markMutationArgs :: Env -> [Exp] -> [Exp]
markMutationArgs env = map go
  where
    go (Typed argQt argArea argMeta (Var n False))
      | n `elem` envMutationsInScope env =
          Typed argQt argArea (ReferenceArgument : argMeta) (Var n False)
    go a = a


-- ---------------------------------------------------------------------------
-- Free variable analysis
--
-- findFreeVars and findFreeVarsInBody are mutually recursive. The global-var
-- filter is applied at each recursive call to findFreeVars so that variables
-- defined in an enclosing body (added to envGlobalVars by findFreeVarsInBody)
-- are correctly excluded from inner scopes.
-- ---------------------------------------------------------------------------

-- | Find free variables in an expression. Returns deduplicated (name, expression)
-- pairs. Variables in the global set (unless excluded) are filtered out.
findFreeVars :: Env -> Exp -> Convert [(String, Exp)]
findFreeVars env expr = do
  fvs <- case expr of
    Typed _ _ _ (Var name _)
      | isBuiltinOrFieldAccess name -> return []
      | otherwise -> do
          var' <- convertExp env expr
          case M.lookup name (envLifted env) of
            Just (_, vars) ->
              return $ map (\v@(Typed _ _ _ (Var n' _)) -> (n', v)) vars
            _ ->
              return [(name, var')]

    Typed _ _ _ (Definition params body) -> do
      vars <- findFreeVarsInBody env body
      return $ filter (\(n, _) -> n `notElem` (getValue <$> params)) vars

    Typed _ _ _ (Call f args) -> do
      fFvs   <- findFreeVars env f
      argFvs <- concatMapM (findFreeVars env) args
      return $ fFvs ++ argFvs

    Typed _ _ _ (Do exps) ->
      findFreeVarsInBody env exps

    Typed _ _ _ (If cond truthy falsy) ->
      concatMapM (findFreeVars env) [cond, truthy, falsy]

    Typed _ _ _ (While cond body) ->
      concatMapM (findFreeVars env) [cond, body]

    Typed _ _ _ (TupleConstructor exps) ->
      concatMapM (findFreeVars env) exps

    Typed _ _ _ (Access record field) ->
      concatMapM (findFreeVars env) [record, field]

    Typed _ _ _ (ArrayAccess arr index) ->
      concatMapM (findFreeVars env) [arr, index]

    Typed _ _ _ (ListConstructor exps) ->
      concatMapM (findFreeVars env . getListItemExp) exps

    Typed _ _ _ (Where whereExp iss) -> do
      expVars <- findFreeVars env whereExp
      issFvs  <- findFreeVarsInBranches env iss
      return $ expVars ++ issFvs

    Typed _ _ _ (Assignment lhs e) -> do
      lhsVars <- findFreeVars env lhs
      expVars <- findFreeVars env e
      return $ lhsVars ++ expVars

    Typed _ _ _ (Record fields) ->
      concatMapM (findFreeVarsInField env) fields

    _ ->
      return []

  let globalVarSet = envGlobalVars env `S.union` M.keysSet (envLifted env)
      deduped = M.toList (M.fromList fvs)
  return $ filter (\(name, _) -> S.notMember name globalVarSet || S.member name (envExclusions env)) deduped


findFreeVarsInField :: Env -> Field -> Convert [(String, Exp)]
findFreeVarsInField env field = case field of
  Typed _ _ _ (Field (_, e))   -> findFreeVars env e
  Typed _ _ _ (FieldSpread e)  -> findFreeVars env e
  _                            -> return []


findFreeVarsInBody :: Env -> [Exp] -> Convert [(String, Exp)]
findFreeVarsInBody _ [] = return []
findFreeVarsInBody env (e : es) = case e of
  Typed qt area _ (Assignment (Typed _ _ _ (Var name _)) rhs) -> do
    fvs <-
      if isFunctionType (getQualified qt) then
        findFreeVars (addGlobalVar name env) rhs
      else
        findFreeVars env rhs
    nextFVs <- findFreeVarsInBody (addGlobalVar name env) es
    if S.member name (envExclusions env) || name `elem` envAllocatedMuts env then
      return $ fvs ++ nextFVs ++ [(name, Typed qt area [] (Var name False))]
    else
      return $ fvs ++ nextFVs

  _ -> do
    fvs     <- findFreeVars env e
    nextFVs <- findFreeVarsInBody env es
    return $ fvs ++ nextFVs


findFreeVarsInBranches :: Env -> [Is] -> Convert [(String, Exp)]
findFreeVarsInBranches env = concatMapM (findFreeVarsInBranch env)

findFreeVarsInBranch :: Env -> Is -> Convert [(String, Exp)]
findFreeVarsInBranch env (Typed _ _ _ (Is pat e)) = do
  let patVars = getPatternVars pat
  expVars <- findFreeVars env e
  return $ filter (\(n, _) -> n `notElem` patVars) expVars
findFreeVarsInBranch _ _ = return []


-- ---------------------------------------------------------------------------
-- Mutation analysis
-- ---------------------------------------------------------------------------

-- | Find variables from @params@ that are reassigned within an expression.
findMutationsInExp :: [String] -> Exp -> [String]
findMutationsInExp params expr = case expr of
  Typed _ _ _ (Assignment (Typed _ _ _ (Var n _)) e) | n `elem` params ->
    n : findMutationsInExp params e

  Typed _ _ _ (Assignment _ e) ->
    findMutationsInExp params e

  Typed _ _ _ (Call fn args) ->
    findMutationsInExp params fn ++ concatMap (findMutationsInExp params) args

  Typed _ _ _ (Definition _ body) ->
    findMutationsInBody params body

  Typed _ _ _ (Access rec _) ->
    findMutationsInExp params rec

  Typed _ _ _ (ArrayAccess arr index) ->
    findMutationsInExp params arr ++ findMutationsInExp params index

  Typed _ _ _ (ListConstructor items) ->
    concatMap (findMutationsInExp params . getListItemExp) items

  Typed _ _ _ (TupleConstructor items) ->
    concatMap (findMutationsInExp params) items

  Typed _ _ _ (Record fields) ->
    concatMap (findMutationsInExp params . getFieldExp) fields

  Typed _ _ _ (If cond truthy falsy) ->
    findMutationsInExp params cond
    ++ findMutationsInExp params truthy
    ++ findMutationsInExp params falsy

  Typed _ _ _ (While cond body) ->
    findMutationsInExp params cond ++ findMutationsInExp params body

  Typed _ _ _ (Do exps) ->
    findMutationsInBody params exps

  Typed _ _ _ (Where e iss) ->
    findMutationsInExp params e
    ++ concatMap (findMutationsInExp params . getIsExpression) iss

  _ ->
    []


findMutationsInBody :: [String] -> [Exp] -> [String]
findMutationsInBody params = concatMap (findMutationsInExp params)


-- | Find ALL mutations: both reassignments and reads of previously assigned variables.
-- This is a broader analysis than 'findMutationsInExp' -- it also tracks variables
-- that are referenced after being assigned (indicating they need reference semantics).
findAllMutationsInExp :: [String] -> [String] -> Exp -> [String]
findAllMutationsInExp params assignments expr = case expr of
  Typed _ _ _ (Var n False) | n `elem` assignments ->
    [n]

  Typed _ _ _ (Assignment (Typed _ _ _ (Var n _)) e) | n `elem` (params ++ assignments) ->
    let nextAssignments = if isDefinition e then [] else [n]
    in  n : findAllMutationsInExp params (nextAssignments ++ assignments) e

  Typed _ _ _ (Assignment (Typed _ _ _ (Var n _)) e) ->
    let nextAssignments = if isDefinition e then [] else [n]
    in  findAllMutationsInExp params (nextAssignments ++ assignments) e

  Typed _ _ _ (Call fn args) ->
    findAllMutationsInExp params assignments fn
    ++ concatMap (findAllMutationsInExp params assignments) args

  Typed _ _ _ (Definition params' body) ->
    findAllMutationsInExps (params ++ (getValue <$> params')) assignments body

  Typed _ _ _ (Access rec _) ->
    findAllMutationsInExp params assignments rec

  Typed _ _ _ (ArrayAccess arr index) ->
    findAllMutationsInExp params assignments arr
    ++ findAllMutationsInExp params assignments index

  Typed _ _ _ (ListConstructor items) ->
    concatMap (findAllMutationsInExp params assignments . getListItemExp) items

  Typed _ _ _ (TupleConstructor items) ->
    concatMap (findAllMutationsInExp params assignments) items

  Typed _ _ _ (Record fields) ->
    concatMap (findAllMutationsInExp params assignments . getFieldExp) fields

  Typed _ _ _ (If cond truthy falsy) ->
    findAllMutationsInExp params assignments cond
    ++ findAllMutationsInExp params assignments truthy
    ++ findAllMutationsInExp params assignments falsy

  Typed _ _ _ (While cond body) ->
    findAllMutationsInExp params assignments cond
    ++ findAllMutationsInExp params assignments body

  Typed _ _ _ (Do exps) ->
    findAllMutationsInExps params assignments exps

  Typed _ _ _ (Where e iss) ->
    findAllMutationsInExp params assignments e
    ++ concatMap (findAllMutationsInExp params assignments . getIsExpression) iss

  _ ->
    []


findAllMutationsInExps :: [String] -> [String] -> [Exp] -> [String]
findAllMutationsInExps _ _ [] = []
findAllMutationsInExps params assignments (e : rest) = case e of
  Typed _ _ _ (Assignment (Typed _ _ _ (Var name _)) _) ->
    findAllMutationsInExp params assignments e
    ++ findAllMutationsInExps (name : params) assignments rest

  _ ->
    findAllMutationsInExp params assignments e
    ++ findAllMutationsInExps params assignments rest


-- ---------------------------------------------------------------------------
-- Expression conversion
-- ---------------------------------------------------------------------------

-- | Filter out dictionary free variables that aren't actually in scope.
removeDictsNotInScope :: Env -> [(String, Exp)] -> [(String, Exp)]
removeDictsNotInScope env =
  filter (\(name, _) -> name `elem` envDictsInScope env || not (isDictName name))
  where
    isDictName ('$' : _) = True
    isDictName _         = False


-- | Convert a body (sequence of expressions) in a non-top-level context.
-- All function definitions encountered here must be lifted.
convertBody :: [String] -> Env -> [Exp] -> Convert [Exp]
convertBody _ _ [] = return []
convertBody exclusionVars env (expr : rest) = case expr of
  -- Named function definition in a body: find free vars and lift it
  Typed _ _ _ (Assignment (Typed _ _ _ (Var name _)) abs@(Typed _ _ _ (Definition _ _))) -> do
    fvs <- findFreeVars (addGlobalVar name env) abs
    let captured = removeDictsNotInScope env fvs
    expr' <- convertDefinition (addExclusions exclusionVars env) name captured abs
    next  <- convertBody (name : exclusionVars) env rest
    return $ expr' : next

  -- Unnamed function definition in a body
  abs@(Typed _ _ _ (Definition _ _)) -> do
    expr' <- convertExp (addExclusions exclusionVars env) abs
    next  <- convertBody exclusionVars env rest
    return $ expr' : next

  -- Non-function assignment: handle mutation tracking
  Typed _ _ _ (Assignment (Typed _ _ _ (Var name _)) _) -> do
    (expr', env') <-
      if name `elem` envMutationsInScope env && name `elem` envAllocatedMuts env then do
        -- Already allocated: just store
        e' <- convertExp env expr
        return (Typed (getQualType e') (getArea e') (ReferenceStore : getMetadata e') (getValue e'), env)
      else if name `elem` envMutationsInScope env && name `notElem` envAllocatedMuts env then do
        -- First mutation: allocate on heap
        let nextEnv = env { envAllocatedMuts = name : envAllocatedMuts env }
        e' <- convertExp nextEnv expr
        return (Typed (getQualType e') (getArea e') (ReferenceAllocation : getMetadata e') (getValue e'), nextEnv)
      else do
        e' <- convertExp env expr
        return (e', env)
    next <- convertBody (name : exclusionVars) env' rest
    return $ expr' : next

  -- Everything else
  _ -> do
    expr' <- convertExp env expr
    next  <- convertBody exclusionVars env rest
    return $ expr' : next


-- | Convert a named function definition.
-- At the top level, just recurse into the body.
-- At a nested level, lift the function to the top level and replace it with
-- a partial application to captured free variables.
convertDefinition :: Env -> String -> [(String, Exp)] -> Exp -> Convert Exp
convertDefinition env functionName captured (Typed (ps :=> t) area metadata (Definition params body))
  | envStillTopLevel env = do
      -- Top-level: no lifting needed, just convert the body
      let allMutations = findAllMutationsInExps (getValue <$> params) [] body
      body' <- convertBody [] (nested env) { envAllocatedMuts = envAllocatedMuts env, envMutationsInScope = allMutations } body
      let assignNode = Typed (ps :=> t) area [] (Var functionName False)
          defNode    = Typed (ps :=> t) area metadata (Definition params body')
      return $ Typed (ps :=> t) area [] (Assignment assignNode defNode)

  | otherwise = do
      -- Nested: lift to top level
      functionName' <- generateLiftedName env functionName

      -- Build parameter list: captured vars become explicit parameters
      let capturedParams =
            (\(name, Typed qt _ _ _) ->
              Typed qt emptyArea [ReferenceParameter | name `elem` envMutationsInScope env] name
            ) <$> captured
          allParams = capturedParams ++ params

      let hasMutation = any ((`elem` envMutationsInScope env) . fst) captured
          fnNodeMetadata = [MutatingFunctionRef | hasMutation]

      -- Convert body with the lifted lambda registered in env
      let ownMutations = findAllMutationsInExps (getValue <$> params) [] body
          bodyEnv = addLiftedLambda functionName functionName' (snd <$> captured)
                      env { envMutationsInScope = envMutationsInScope env ++ ownMutations }
      body' <- convertBody [] bodyEnv body

      -- Build and register the lifted definition
      let liftedType = foldr fn t (getType . snd <$> captured)
          liftedAssign = Typed (ps :=> liftedType) area [] (Var functionName' False)
          liftedDef    = Typed (ps :=> liftedType) area metadata (Definition allParams body')
          liftedExp    = Typed (ps :=> liftedType) area [] (Assignment liftedAssign liftedDef)
      addTopLevelExp liftedExp

      let functionNode = Typed (ps :=> liftedType) area fnNodeMetadata (Var functionName' False)
          nameNode     = Typed (ps :=> t) area [] (Var functionName False)

      if null captured then
        return $ Typed (ps :=> t) area [] (Assignment nameNode functionNode)
      else
        let capturedArgs = markMutationArgs env (snd <$> captured)
            callNode     = Typed (ps :=> t) area metadata (Call functionNode capturedArgs)
        in  return $ Typed (ps :=> t) area [] (Assignment nameNode callNode)

convertDefinition _ _ _ e = return e


-- | Flatten nested Call(Call(f, args1), args2) into Call(f, args1 ++ args2).
-- This happens when a lifted lambda is looked up and applied to captured args,
-- then the result is called with the explicit args.
flattenNestedCalls :: Exp -> Exp
flattenNestedCalls expr = case expr of
  Typed qt area metadata (Call (Typed _ _ _ (Call fn' args')) args) ->
    flattenNestedCalls $ Typed qt area metadata (Call fn' (args' ++ args))
  _ ->
    expr


-- | Convert a single expression.
convertExp :: Env -> Exp -> Convert Exp
convertExp env fullExp@(Typed qt@(ps :=> t) area metadata e) = case e of
  JSExp js ->
    return $ Typed qt area metadata (JSExp js)

  Call fn args -> do
    fn'   <- convertExp (nested env) fn
    args' <- mapM (convertExp (nested env)) args
    return $ flattenNestedCalls $ Typed qt area metadata (Call fn' args')

  Access rec field -> do
    rec'   <- convertExp (nested env) rec
    field' <- convertExp (nested env) field
    return $ Typed qt area metadata (Access rec' field')

  ArrayAccess arr index -> do
    arr'   <- convertExp (nested env) arr
    index' <- convertExp (nested env) index
    return $ Typed qt area metadata (ArrayAccess arr' index')

  -- Exported named function definition
  Export (Typed _ _ _ (Assignment (Typed _ _ _ (Var name _)) abs@(Typed _ _ _ Definition{}))) -> do
    fvs <- findFreeVars (addGlobalVar name env) fullExp
    let captured = removeDictsNotInScope env fvs
    convertDefinition env name captured abs

  -- Named function definition
  Assignment (Typed _ _ _ (Var name _)) abs@(Typed _ _ _ Definition{}) -> do
    fvs <- findFreeVars (addGlobalVar name env) fullExp
    let captured = removeDictsNotInScope env fvs
    convertDefinition env name captured abs

  -- Anonymous function: generate a name and lift
  Definition params body -> do
    let allMutations = findAllMutationsInExps (getValue <$> params) [] body
    body' <- convertBody [] env { envMutationsInScope = envMutationsInScope env ++ allMutations } body
    fvs   <- findFreeVars env fullExp
    let captured = removeDictsNotInScope env fvs
    functionName <- generateLiftedName env "$lambda"

    let capturedParams =
          (\(n, e') -> Typed (getQualType e') emptyArea [ReferenceParameter | n `elem` envMutationsInScope env] n)
          <$> captured
        allParams = capturedParams ++ params

    let liftedType   = foldr fn t (getType . snd <$> captured)
        liftedAssign = Typed (ps :=> liftedType) area [] (Var functionName False)
        liftedDef    = Typed (ps :=> liftedType) area metadata (Definition allParams body')
        liftedExp    = Typed (ps :=> liftedType) area [] (Assignment liftedAssign liftedDef)
    addTopLevelExp liftedExp

    let functionNode = Typed (ps :=> liftedType) area [] (Var functionName False)

    if null captured then
      return functionNode
    else
      let capturedArgs = markMutationArgs env (snd <$> captured)
      in  return $ Typed qt area [] (Call functionNode capturedArgs)

  -- Non-function assignment
  Assignment name rhs -> do
    let env' =
          if envStillTopLevel env then
            let mutations    = findMutationsInBody [] [rhs]
                allMutations = findAllMutationsInExps [] [] [rhs]
            in  (nested env) { envAllocatedMuts = envAllocatedMuts env ++ mutations
                             , envMutationsInScope = allMutations }
          else
            env
    rhs' <- convertExp env' rhs
    return $ Typed qt area metadata (Assignment name rhs')

  Export inner ->
    convertExp env inner

  NameExport name ->
    return $ Typed qt area metadata (NameExport name)

  -- Variable reference: check if it was lifted
  Var name isConstructor ->
    case M.lookup name (envLifted env) of
      Just (newName, capturedArgs) ->
        let capturedArgs' = markMutationArgs env capturedArgs
        in  return $ Typed qt area metadata (Call (Typed qt area [] (Var newName isConstructor)) capturedArgs')
      Nothing ->
        return $ Typed qt area metadata (Var name isConstructor)

  ListConstructor items -> do
    items' <- mapM (convertListItem env) items
    return $ Typed qt area metadata (ListConstructor items')

  TupleConstructor exps -> do
    exps' <- mapM (convertExp env) exps
    return $ Typed qt area metadata (TupleConstructor exps')

  Record fields -> do
    fields' <- mapM (convertField (nested env)) fields
    return $ Typed qt area metadata (Record fields')

  If cond truthy falsy -> do
    cond'   <- convertExp (nested env) cond
    truthy' <- convertExp (nested env) truthy
    falsy'  <- convertExp (nested env) falsy
    return $ Typed qt area metadata (If cond' truthy' falsy')

  While cond body -> do
    cond' <- convertExp (nested env) cond
    body' <- convertExp (nested env) body
    return $ Typed qt area metadata (While cond' body')

  Do exps -> do
    exps' <- convertBody [] (nested env) exps
    return $ Typed qt area metadata (Do exps')

  Where wExp iss -> do
    wExp' <- convertExp (nested env) wExp
    iss'  <- mapM (convertIs (nested env)) iss
    return $ Typed qt area metadata (Where wExp' iss')

  Extern qt' name originalName ->
    return $ Typed qt area metadata (Extern qt' name originalName)

  _ ->
    return fullExp

convertExp _ e = return e


-- ---------------------------------------------------------------------------
-- Boilerplate traversals for sub-expression types
-- ---------------------------------------------------------------------------

convertListItem :: Env -> ListItem -> Convert ListItem
convertListItem env (Typed qt area metadata item) = case item of
  ListItem e -> do
    e' <- convertExp env e
    return $ Typed qt area metadata $ ListItem e'
  ListSpread e -> do
    e' <- convertExp env e
    return $ Typed qt area metadata $ ListSpread e'
convertListItem _ li = return li

convertField :: Env -> Field -> Convert Field
convertField env (Typed qt area metadata item) = case item of
  Field (name, e) -> do
    e' <- convertExp env e
    return $ Typed qt area metadata $ Field (name, e')
  FieldSpread e -> do
    e' <- convertExp env e
    return $ Typed qt area metadata $ FieldSpread e'
convertField _ f = return f

convertIs :: Env -> Is -> Convert Is
convertIs env (Typed qt area metadata (Is pat e)) = do
  pat' <- convertPattern env pat
  e'   <- convertExp env e
  return $ Typed qt area metadata (Is pat' e')
convertIs _ is = return is

convertPattern :: Env -> Pattern -> Convert Pattern
convertPattern env (Typed qt area metadata pat) = case pat of
  PVar name   -> return $ Typed qt area metadata $ PVar name
  PAny        -> return $ Typed qt area metadata PAny
  PNum num    -> return $ Typed qt area metadata $ PNum num
  PStr str    -> return $ Typed qt area metadata $ PStr str
  PChar c     -> return $ Typed qt area metadata $ PChar c
  PBool bool  -> return $ Typed qt area metadata $ PBool bool

  PCon name pats -> do
    pats' <- mapM (convertPattern env) pats
    return $ Typed qt area metadata $ PCon name pats'

  PRecord pats restName -> do
    pats' <- mapM (convertPattern env) pats
    return $ Typed qt area metadata $ PRecord pats' restName

  PList pats -> do
    pats' <- mapM (convertPattern env) pats
    return $ Typed qt area metadata $ PList pats'

  PTuple pats -> do
    pats' <- mapM (convertPattern env) pats
    return $ Typed qt area metadata $ PTuple pats'

  PSpread pat -> do
    pat' <- convertPattern env pat
    return $ Typed qt area metadata $ PSpread pat'
convertPattern _ p = return p

convertTyping :: Env -> Typing -> Convert Typing
convertTyping env (Untyped area metadata typing) = case typing of
  TRSingle name ->
    return $ Untyped area metadata $ TRSingle name

  TRComp name typings -> do
    typings' <- mapM (convertTyping env) typings
    return $ Untyped area metadata $ TRComp name typings'

  TRArr left right -> do
    left'  <- convertTyping env left
    right' <- convertTyping env right
    return $ Untyped area metadata $ TRArr left' right'

  TRRecord fields base -> do
    fields' <- mapM (convertTyping env) fields
    base'   <- mapM (convertTyping env) base
    return $ Untyped area metadata $ TRRecord fields' base'

  TRTuple typings -> do
    typings' <- mapM (convertTyping env) typings
    return $ Untyped area metadata $ TRTuple typings'

  TRConstrained constraints typing -> do
    constraints' <- mapM (convertTyping env) constraints
    typing'      <- convertTyping env typing
    return $ Untyped area metadata $ TRConstrained constraints' typing'
convertTyping _ t = return t

convertTypeDecl :: Env -> TypeDecl -> Convert TypeDecl
convertTypeDecl env (Untyped area metadata adt@ADT{}) = do
  ctors <- mapM convertCtor $ adtconstructors adt
  return $ Untyped area metadata $ adt { adtconstructors = ctors }
  where
    convertCtor :: Constructor -> Convert Constructor
    convertCtor (Untyped a meta (Constructor name typings t)) = do
      typings' <- mapM (convertTyping env) typings
      return $ Untyped a meta $ Constructor name typings' t
    convertCtor c = return c
convertTypeDecl _ td = return td

convertImport :: Import -> Convert Import
convertImport imp@(Untyped _ _ (NamedImport _ _ _)) = return imp
convertImport imp = return imp


-- ---------------------------------------------------------------------------
-- Gathering global names from the AST
-- ---------------------------------------------------------------------------

getConstructorNames :: [TypeDecl] -> [String]
getConstructorNames = concatMap getCtorNames
  where
    getCtorNames (Untyped _ _ ADT{ adtconstructors }) =
      (\(Untyped _ _ (Constructor name _ _)) -> name) <$> adtconstructors
    getCtorNames _ = []

getGlobalsFromImports :: [Import] -> [String]
getGlobalsFromImports = concatMap getNames
  where
    getNames (Untyped _ _ (NamedImport names _ _)) = getImportName <$> names
    getNames _ = []


-- ---------------------------------------------------------------------------
-- AST conversion (entry point)
-- ---------------------------------------------------------------------------

convertAST :: AST -> AST
convertAST ast =
  MonadState.evalState (convertASTInner initialEnv ast) initialState

convertASTInner :: Env -> AST -> Convert AST
convertASTInner _ ast@AST{ apath = Nothing } = return ast
convertASTInner env ast@AST{ apath = Just path } = do
  let globalVars         = mapMaybe getExpName (aexps ast)
      globalConstructors = getConstructorNames (atypedecls ast)
      globalsFromImports = getGlobalsFromImports (aimports ast)
      allGlobals         = globalVars ++ globalConstructors ++ globalsFromImports ++ ["$"]
      env' = env { envGlobalVars  = S.fromList allGlobals
                 , envModuleHash  = Hash.generateHashFromPath path
                 }

  imports   <- mapM convertImport $ aimports ast
  exps      <- mapM (convertExp env') $ aexps ast
  typeDecls <- mapM (convertTypeDecl env') $ atypedecls ast

  defs <- getTopLevelExps

  return AST { aimports   = imports
             , aexps      = defs ++ exps
             , atypedecls = typeDecls
             , apath      = apath ast
             }


-- ---------------------------------------------------------------------------
-- Utility
-- ---------------------------------------------------------------------------

concatMapM :: Monad m => (a -> m [b]) -> [a] -> m [b]
concatMapM f xs = concat <$> mapM f xs
