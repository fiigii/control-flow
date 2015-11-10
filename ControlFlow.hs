module ControlFlow where

import Control.Monad.Writer

import LabeledAst
import Data.Set (Set, union, singleton, isSubsetOf, member)
import qualified Data.Set as Set
import Data.Map (Map, findWithDefault)
import qualified Data.Map as Map
import Data.List (partition)
import Ast (Ast)

data Abstract = Cache Label
              | Envir String
              deriving (Eq, Ord, Show)
                

data Constraint = Concrete LAst Abstract
                | Subset Abstract Abstract
                | Conditional LAst Abstract Abstract Abstract
                deriving (Eq, Ord, Show)

type Cache = Map Label [LAst]
type Envir = Map String [LAst]

controlFlow :: Ast -> (Cache, Envir)
controlFlow ast = let (lAst, maxL) = convert ast
                      vars = allVar lAst
                      result = solve $ constraints lAst
                      (caches, envirs) = Map.partitionWithKey (\k _ -> isCache k) result
                      cache = Map.fromList $
                              map (\n -> (n, Set.toList $ findWithDefault Set.empty (Cache n) caches))
                              [1..maxL]
                      envir = Map.fromList $
                              map (\v -> (v, Set.toList $ findWithDefault Set.empty (Envir v) envirs))
                              (Set.toList vars)
                  in (cache, envir)

constraints :: LAst -> Set Constraint
constraints program = genConstraints (allFunction program) program

genConstraints :: Set LAst -> LAst -> Set Constraint
genConstraints _ (Var x l) = singleton $ Subset (Envir x)  (Cache l)
genConstraints a f@(Function _ body l) = genConstraints a body `union`
                                         singleton (Concrete f $ Cache l)
genConstraints allfun (Application t1 t2 l) =
  genConstraints allfun t1 `union` genConstraints allfun t2 `union`
  Set.map (\t@(Function x _ _) ->
             Conditional t (Cache l1) (Cache l2) (Envir x)) allfun `union`
  Set.map (\t@(Function _ body _) ->
             Conditional t (Cache l1) (Cache $ labelOf body) (Cache l)) allfun
  where l1 = labelOf t1
        l2 = labelOf t2
genConstraints a (IfExpr t0 t1 t2 l) =
  genConstraints a t0 `union` genConstraints a t1 `union` genConstraints a t2 `union`
  Set.fromList [Subset (Cache l1) (Cache l), Subset (Cache l2) (Cache l)]
  where l1 = labelOf t1
        l2 = labelOf t2
genConstraints a (LetRec binds body l) =
  let inits = map snd binds
      vs = map fst binds
      initsCons = map (genConstraints a) inits
      l1s = map labelOf inits
      l2 = labelOf body
      vsCons = map (\(x, l1) -> singleton $ Subset (Cache l1) (Envir x)) $ zip vs l1s
  in genConstraints a body `union` Set.unions initsCons `union`
     Set.unions vsCons `union` singleton (Subset (Cache l2) (Cache l))
genConstraints a (BinaryExpr _ t1 t2 _) = genConstraints a t1 `union`
                                          genConstraints a t2
genConstraints _ _ = Set.empty

solve :: Set Constraint -> Map Abstract (Set LAst)
solve consts =
  let (concretes, subAndCondi) = partition isConcrete $ Set.toList consts
      dataArray :: Map Abstract (Set LAst)
      (dataArray, workList) = buildData Map.empty [] concretes
      edgeArray :: Map Abstract [Constraint]
      edgeArray = buildEdge Map.empty subAndCondi
  in iteration workList edgeArray dataArray
  where buildEdge es [] = es
        buildEdge es (cc@(Subset p1 _) : cs) =
          let remain = buildEdge es cs
          in Map.insert p1 (cc : findE p1 remain) remain 
        buildEdge es (cc@(Conditional _ p p1 _) : cs) =
          let remain = buildEdge es cs
              remain' =  Map.insert p (cc : findE p remain) remain
          in Map.insert p1 (cc : findE p1 remain') remain'
        buildData d w [] = (d, w)
        buildData d w (Concrete t p : cs) =
          let (d', w') = add p (singleton t) d w
          in buildData d' w' cs
        iteration [] _ d = d
        iteration (q : w) e d = let (newDataArray, w') = buildResult (findE q e) d w
                                in iteration w' e newDataArray

buildResult :: [Constraint] -> Map Abstract (Set LAst) -> [Abstract] -> (Map Abstract (Set LAst), [Abstract])    
buildResult [] d w = (d, w)
buildResult (Subset p1 p2 : cs) d w = 
  let (d', w') = add p2 (findD p1 d) d w 
  in buildResult cs d' w'
buildResult (Conditional t p p1 p2 : cs) d w=
  if t `member` findD p d
  then let (d', w') = add p2 (findD p1 d) d w
       in buildResult cs d' w'
  else (d, w)                     

add :: Abstract -> Set LAst -> Map Abstract (Set LAst) -> [Abstract] -> (Map Abstract (Set LAst), [Abstract])
add q d da w =
  let dq = findD q da
  in if not $ d `isSubsetOf` dq
     then (Map.insert q (dq `union` d) da, q : w)
     else (da, w)

findE :: Abstract -> Map Abstract [Constraint] -> [Constraint]
findE =  findWithDefault []

findD :: Abstract ->  Map Abstract (Set LAst) ->  (Set LAst)
findD = findWithDefault Set.empty

isConcrete :: Constraint -> Bool
isConcrete (Concrete _ _) = True
isConcrete _ = False

isCache :: Abstract -> Bool
isCache (Cache _) = True
isCache _ = False
