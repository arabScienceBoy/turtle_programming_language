module LanguageImplementation where

import DrawShapes
import Graphics.Gloss
import LanguageCore
import qualified Data.Map as M
import Data.Map.Lens
import Control.Applicative
import Control.Lens.Extras
import Control.Lens

evalListOfCommands :: TurtleState -> [Commands] -> [TurtleConfiguration]
evalListOfCommands Turtle{..} []        = (\(Just x) -> x) (M.lookup "Main" stack)
evalListOfCommands Turtle{..} (Stop:_)  = (\(Just x) -> x) (M.lookup "Main" stack)
evalListOfCommands ts ((IF cond ys):zs) = case cond of
                                           LitB True -> evalListOfCommands ts ys
                                           LitB False -> evalListOfCommands ts zs
evalListOfCommands ts (x:xs)               = evalListOfCommands ts' xs
  where ts' = evalGlobal ts x

evalGlobal :: TurtleState -> Commands -> TurtleState
evalGlobal ts (FuncSetArg name args lst)          = insertToScopeArgs name args lst ts
evalGlobal ts@Turtle{..} i@(Forward (LitF x))     = setStack (forward x) ts
evalGlobal ts (Forward t@(OpV _))                 = evalGlobal ts (Forward (evalLocal "Main" ts t))
evalGlobal ts@Turtle{..} i@(Backward (LitF x))    = setStack (backward x) ts
evalGlobal ts (Backward t@(OpV _))                = evalGlobal ts (Backward (evalLocal "Main" ts t))
evalGlobal ts@Turtle{..} i@(TurnR (LitF x))       = setStack (turnR x) ts
evalGlobal ts (TurnR t@(OpV _))                   = evalGlobal ts (TurnR (evalLocal "Main" ts t))
evalGlobal ts@Turtle{..} i@(TurnL (LitF x))       = setStack (turnL x) ts
evalGlobal ts (TurnL t@(OpV _))                   = evalGlobal ts (TurnL (evalLocal "Main" ts t))
evalGlobal ts (Error n err)                       = setStack (addError n err) ts
evalGlobal ts@Turtle{..} i@(Colors c)             = case c of
                                                      Red    -> setStack (setColor red) ts
                                                      Blue   -> setStack (setColor blue) ts
                                                      Black  -> setStack (setColor black) ts
                                                      White  -> setStack (setColor white) ts
                                                      Green  -> setStack (setColor green) ts
                                                      Yellow -> setStack (setColor yellow) ts
                                                      Cyan   -> setStack (setColor cyan) ts
evalGlobal ts@Turtle{..} i@(Penup)                = setStack penUp ts
evalGlobal ts@Turtle{..} i@(Pendown)              = setStack penDown ts
evalGlobal ts@Turtle{..} i@(Repeat (LitF s) xs)   = mapEvalGlobal ts (reverse $ repeateFunc s xs)
evalGlobal ts@Turtle{..} (Repeat t@(OpV s)  xs)   = evalGlobal ts (Repeat (evalLocal "Main" ts t) xs)
evalGlobal ts@Turtle{..} i@(Comment)              = setStack comment ts
evalGlobal ts@Turtle{..} (Coms xs)                = mapEvalGlobal ts (xs)
evalGlobal ts@Turtle{..} i@(FuncGetArg name ((LitFs xs):_))           = evalGlobal ts (getFuncWithArgs name [] ts)
evalGlobal ts@Turtle{..} i@(FuncGetArg name xs)
  | all (is _LitF) xs     = evalGlobal ts (evalLocal name ts i)
  | all (is _OpV) xs      = evalGlobal ts (FuncGetArg name (map (evalLocal "Main" ts) xs))
  | filter (is _Error) xs /= []     = mapEvalGlobal ts (filter (is _Error) xs)
  | otherwise             = evalGlobal ts (FuncGetArg name (map (evalLocal "Main" ts) xs))
evalGlobal ts@Turtle{..} i@(IF cond xs)           = case cond of
                                                      (LitB True)  -> mapEvalGlobal ts xs
                                                      (LitB False) -> ts
                                                      otherwise    -> if (filter (is _OpV) xs) /= []
                                                                          then evalGlobal ts (evalLocal "Main" ts i) -- IF (LitB True) xs
                                                                          else addError "arr" "variable in Main outside functions!" ts

setStack :: TurtleConfiguration -> TurtleState -> TurtleState
setStack f ts@Turtle{..} = ts {stack = M.adjust (f:) "Main" stack}

mapEvalGlobal :: TurtleState -> [Commands] -> TurtleState
mapEvalGlobal ts [] = ts
mapEvalGlobal ts (Stop:_) = ts
mapEvalGlobal ts ((IF cond ys):zs) = case cond of
                                        LitB True -> mapEvalGlobal ts ys
                                        LitB False -> mapEvalGlobal ts zs
mapEvalGlobal ts (x:xs) = mapEvalGlobal ts' xs
  where ts' = evalGlobal ts x

evalLocal :: String -> TurtleState -> Commands -> Commands
evalLocal _ _ i@(LitB _)                           = i
evalLocal fn ts (Var x)                            = (getArgValue fn x ts)
evalLocal _ _ (Add (LitF x) (LitF y))              = LitF (x + y)
evalLocal _ _ (Sub (LitF x) (LitF y))              = LitF (x - y)
evalLocal _ _ (Mult (LitF x) (LitF y))             = LitF (x * y)
evalLocal _ _ (Div (LitF x) (LitF y))              = LitF (x / y)
evalLocal _ _ i@(LitF _)                           = i
evalLocal _ _ (Eq (LitF x) (LitF y))               = LitB (x == y)
evalLocal _ _ (BT (LitF x) (LitF y))               = LitB (x > y)
evalLocal _ _ (ST (LitF x) (LitF y))               = LitB (x < y)
evalLocal _ _ (BEQ (LitF x) (LitF y))              = LitB (x >= y)
evalLocal _ _ (SEQ (LitF x) (LitF y))              = LitB (x <= y)
evalLocal _ _ (NOT (LitF x) (LitF y))              = LitB (x /= y)
evalLocal fn ts (Add x y)                          = evalLocal fn ts (Add (evalLocal fn ts x) (evalLocal fn ts y))
evalLocal fn ts (Sub x y)                          = evalLocal fn ts (Sub (evalLocal fn ts x) (evalLocal fn ts y))
evalLocal fn ts (Mult x y)                         = evalLocal fn ts (Mult (evalLocal fn ts x) (evalLocal fn ts y))
evalLocal fn ts (Div x y)                          = evalLocal fn ts (Div (evalLocal fn ts x) (evalLocal fn ts y))
evalLocal fn ts (Eq x y)                           = evalLocal fn ts (Eq (evalLocal fn ts x) (evalLocal fn ts y))
evalLocal fn ts (BT x y)                           = evalLocal fn ts (BT (evalLocal fn ts x) (evalLocal fn ts y))
evalLocal fn ts (ST x y)                           = evalLocal fn ts (ST (evalLocal fn ts x) (evalLocal fn ts y))
evalLocal fn ts (BEQ x y)                          = evalLocal fn ts (BEQ (evalLocal fn ts x) (evalLocal fn ts y))
evalLocal fn ts (SEQ x y)                          = evalLocal fn ts (SEQ (evalLocal fn ts x) (evalLocal fn ts y))
evalLocal fn ts (NOT x y)                          = evalLocal fn ts (NOT (evalLocal fn ts x) (evalLocal fn ts y))
evalLocal fn ts (OpV x)                            = evalLocal fn ts x
evalLocal fn ts (Forward x)                        = Forward (evalLocal fn ts x)
evalLocal fn ts (Backward x)                       = Backward (evalLocal fn ts x)
evalLocal fn ts (TurnR x)                          = TurnR (evalLocal fn ts x)
evalLocal fn ts (TurnL x)                          = TurnL (evalLocal fn ts x)
evalLocal _ _ Penup                                = Penup
evalLocal _ _ Pendown                              = Pendown
evalLocal _ _ Comment                              = Comment
evalLocal _ _ Stop                                 = Stop
evalLocal _ _ (Error a b)                          = Error a b
evalLocal fn ts (Repeat x xs)                      = Repeat (evalLocal fn ts x) (map (evalLocal fn ts) xs)
evalLocal fn ts (IF cond xs)                       = IF (evalLocal fn ts cond) (map (evalLocal fn ts) xs)
evalLocal _ _  i@(FuncSetArg _ _ _)                = i
evalLocal _ _ i@(FuncGetArg name [LitFs []])       = i
evalLocal fn ts i@(FuncGetArg name xs)             = getFuncWithArgs name (map (evalLocal fn ts)xs) ts


getArgValue :: String -> String -> TurtleState -> Commands
getArgValue fn argn ts@Turtle{..} = case M.lookup fn argsValue of
                                  Just l ->
                                    case M.lookup argn l of
                                      Just s -> LitF s
                                      Nothing -> Error fn ("Turtle <**Error>: Function \"" ++ fn ++ "\" not in Scope!")
                                  Nothing -> Error argn ("Turtle <**Error>: Variable \"" ++ argn ++ "\" not in Scope!")

insertToScopeArgs :: String -> [String] -> [Commands] -> TurtleConfiguration
insertToScopeArgs funcName []   xs ts@Turtle{..} = ts {scope = M.insert funcName xs scope}
insertToScopeArgs funcName args xs ts@Turtle{..} = ts {
  scope       = M.insert funcName xs scope,
  getArgOrder = M.insert funcName (M.fromList $ zip [1..] args) getArgOrder
  }

getFuncWithArgs :: String -> [Commands] -> TurtleState -> Commands
getFuncWithArgs fN xsx tss = case M.lookup fN (scope tss) of
                                   Just _  -> helpFunc fN xsx tss
                                   Nothing -> Error fN ("Turtle <**Error>: Fuction \"" ++ fN ++ "\" is not in scope! <getFuncWithArgs>")
  where
    helpFunc funcName xs ts
      | xs == [] && numOfArgs == []   = case M.lookup funcName (scope ts) of
                                          Just l  -> Coms l
      | length xs == length numOfArgs = case M.lookup funcName (scope newState) of
                                          Just l  -> Coms (map (evalLocal funcName newState) l)
      | xs == [] && numOfArgs /= []   = Error funcName ("Turtle <**Error>: Function "  ++ funcName ++ " has no given arguments!")
      | length xs > length numOfArgs  = Error funcName ("Turtle <**Error>: Function " ++ funcName ++ " has too much given arguments!")
      | otherwise                     = Error funcName ("Turtle <**Error>: Function " ++ funcName ++ " has too few given arguments!")
      where newState = matchArgsWithValues funcName (map toFloat xs) ts
            numOfArgs = case M.lookup funcName (getArgOrder ts) of
                          Just l  -> M.toList l
                          Nothing -> []
            toFloat = (\(LitF x) -> x)

matchArgsWithValues :: String -> [Float] -> TurtleConfiguration
matchArgsWithValues funcName val ts@Turtle{..} =
  case M.lookup funcName scope of
    Just l -> case xs of
                Right xss -> ts {argsValue = M.insert funcName (M.fromList xss) argsValue}
  where
    xs = helpFunc funcName val ts
      where
        helpFunc name ys tss@Turtle{..} =
          case M.lookup name getArgOrder of
            Just l -> case (helpFunc2 ys 1 l tss) of
                        Right l -> Right l
        helpFunc2 [] _ _ _ = Right []
        helpFunc2 (s:ss) n ls tu = case M.lookup n ls of
                                      Just q -> (:) <$> Right (q, s) <*> (helpFunc2 ss (n+1) ls tu)

addError :: String -> String ->TurtleConfiguration
addError name err ts@Turtle{..} = ts {errors = M.insert name err errors}
