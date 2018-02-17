-module(hello_world).

-export([greet/0, greet/1]).

-spec greet() -> string().
greet() -> greet("World").

-spec greet(string()) -> string().
greet(Name) -> "Hello, " ++ Name ++ "!".
