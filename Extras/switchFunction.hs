inType a
  | a == "String" = "String"
  | a == "Int" = "Int"
  | otherwise = "Null"

main = print (inType "X")
