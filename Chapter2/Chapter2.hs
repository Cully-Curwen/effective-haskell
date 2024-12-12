module Chapter2 where
 
isBalanced s =
  0 == isBalanced' 0 s
  where
    isBalanced' count s
      | null s = count
      | head s == '(' = isBalanced' (count + 1) (tail s)
      | head s == ')' = isBalanced' (count - 1) (tail s)
      | otherwise = isBalanced' count (tail s)

reduce func carryValue lst =
  if null lst then carryValue
  else
    let intermediateValue = func carryValue (head lst)
    in reduce func intermediateValue (tail lst)

isBalanced2 s = 0 == reduce checkBalance 0 str
  where
  checkBalance count letter
    | letter == '(' = count + 1
    | letter == ')' = count - 1
    | otherwise = count


