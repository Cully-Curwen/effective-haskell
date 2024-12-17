module Exercises where

import Text.Read (readEither)

data BinaryTree a = Leaf | Branch (BinaryTree a) a (BinaryTree a)

-- Turn a binary tree of strings into a pretty-printed string
showStringTree :: BinaryTree String -> String
showStringTree Leaf = ""
showStringTree (Branch l a r) = showStringTree l <> show a <> showStringTree r

-- Add a new integer into a binary tree of integers
-- Don’t worry about keeping your binary tree balanced. For now, try to insert elements using the following rules:
-- If the new element is smaller than the root of the tree, insert the element on the left
-- If the new element is larger than the root of the tree, insert the element on the right
-- If the new element is the same as the root of the tree, do nothing
-- If the tree is empty, insert an element by creating a new root whose left and write sides are both empty leaves
addElementToIntTree :: BinaryTree Int -> Int -> BinaryTree Int
addElementToIntTree Leaf n = Branch Leaf n Leaf
addElementToIntTree (Branch l a r) n
  | n < a = Branch (addElementToIntTree l n) a r
  | n > a = Branch l a (addElementToIntTree r n)
  | n == a = Branch l n l

-- Check to see if an int value exists in a binary tree of ints
doesIntExist :: BinaryTree Int -> Int -> Bool
doesIntExist Leaf _ = False
doesIntExist (Branch l a r) n
  | n < a = doesIntExist l n
  | n > a = doesIntExist r n
  | n == a = True

---------------------------------------------------------

data Expr = Lit Int
  | Sub Expr Expr
  | Add Expr Expr
  | Mul Expr Expr
  | Div Expr Expr

data StringParser = StringParser { runStringParser :: String -> (String, String) }

eval :: Expr -> Int
eval expr =
  case expr of
    Lit num -> num
    Add arg1 arg2 -> eval' (+) arg1 arg2
    Sub arg1 arg2 -> eval' (-) arg1 arg2
    Mul arg1 arg2 -> eval' (*) arg1 arg2
    Div arg1 arg2 -> eval' div arg1 arg2
    where
      eval' :: (Int -> Int -> Int) -> Expr -> Expr -> Int
      eval' operator arg1 arg2 = operator (eval arg1) (eval arg2)

safeEval :: Expr -> Either String Int
safeEval expr =
  case expr of
    Lit num   -> Right num
    Add arg1 arg2 -> eval' (opHelper (+)) arg1 arg2
    Sub arg1 arg2 -> eval' (opHelper (-)) arg1 arg2
    Mul arg1 arg2 -> eval' (opHelper (*)) arg1 arg2
    Div arg1 arg2 -> eval' safeDiv arg1 arg2
    where
      safeDiv :: Int -> Int -> Either String Int
      safeDiv a b
        | b == 0 = Left "Error: division by zero"
        | otherwise = Right $ a `div` b

      opHelper ::
        (Int -> Int -> Int) ->
        Int ->
        Int ->
        Either String Int
      opHelper op a b = Right $ a `op` b

      eval' ::
        (Int -> Int -> Either String Int) ->
        Expr ->
        Expr ->
        Either String Int
      eval' operator arg1 arg2 =
        case safeEval arg1 of
          Left err -> Left err
          Right a ->
            case safeEval arg2 of
              Left err -> Left err
              Right b -> operator a b

parse :: String -> Either String Expr
parse str =
  case parse' (words str) of
    Left err -> Left err
    Right (e,[]) -> Right e
    Right (_,rest) -> Left $ "Found extra tokens: " <> (unwords rest)

parse' (token:rest) =
  case token of
    "+" -> parseBinary Add rest
    "*" -> parseBinary Mul rest
    "-" -> parseBinary Sub rest
    "/" -> parseBinary Div rest
    lit ->
      case readEither lit of
        Left err -> Left err
        Right lit' -> Right (Lit lit', rest)

parseBinary :: (Expr -> Expr -> Expr) -> [String] -> Either String (Expr, [String])
parseBinary exprConstructor args =
  case parse' args of
    Left err -> Left err
    Right (firstArg,rest') ->
      case parse' rest' of
        Left err -> Left err
        Right (secondArg,rest'') ->
          Right $ (exprConstructor firstArg secondArg, rest'')

run :: String -> String
run expr =
  case parse expr of
    Left err -> "Error: " <> err
    Right expr' ->
      let answer = show $ eval expr'
      in "The answer is: " <> answer

---------------------------------------------

prettyPrint :: Expr -> String
prettyPrint expr = helper expr <> " = " <> show (eval expr)
  where
    helper :: Expr -> String
    helper expr =
      case expr of
        Lit num -> show num
        Add arg1 arg2 -> prettyHelper arg1 <> " + " <> prettyHelper arg2
        Sub arg1 arg2 -> prettyHelper arg1 <> " - " <> prettyHelper arg2
        Mul arg1 arg2 -> prettyHelper arg1 <> " × " <> prettyHelper arg2
        Div arg1 arg2 -> prettyHelper arg1 <> " ÷ " <> prettyHelper arg2

    prettyHelper :: Expr -> String
    prettyHelper arg = 
      case arg of
        Lit num -> helper arg
        _ -> "( " <> helper arg <> " )"

