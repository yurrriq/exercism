module HelloWorld exposing (helloWorld)

helloWorld : Maybe String -> String
helloWorld name =
  {- It's very surprising to me that Elm doesn't
     support pattern matching in function clauses. -}
  case name of
    Nothing   -> helloWorld (Just "World")
    Just name -> "Hello, " ++ name ++ "!"
