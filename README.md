# JSON lenses with Elm

An exploration of lenses for JSON based on [Json.Lenses](https://grain-lang.org/docs/stdlib/json#Json-Lenses).

## Disclaimer

This is mostly to satisfy my curiosity. If you're new to Elm check out [`elm/json`](https://package.elm-lang.org/packages/elm/json/latest/). If you think you need this for some reason, please [read this](https://github.com/elm/package.elm-lang.org/blob/afe1a128b4bbf5ec0ebc21886d32b0b473794a9e/src/frontend/Page/Search.elm#L409-L429) before finalizing your decision.

## An example

```elm
import Array
import Json.Lenses exposing (..)


subject1 : Json
subject1 =
    Object
        [ ( "a"
          , Object
              [ ( "b"
                , Object
                    [ ( "c"
                      , Array <|
                          Array.fromList
                              [ Null
                              , Bool True
                              , Bool False
                              , Int 5
                              , Float 3.14
                              , String "Hello"
                              , Object
                                  [ ( "d", Array <| Array.fromList [ String "world" ] )
                                  ]
                              ]
                      )
                    ]
                )
              ]
          )
        ]


example1 : Maybe Json
example1 =
    get subject1 (at ["a", "b", "c"] |> compose (index 3))
    --
    -- == Just (Int 5)
    --


d0 : Lens Json Json
d0 =
    at ["a", "b", "c"]
        |> compose (index 6)
        |> compose (field "d")
        |> compose (index 0)


subject2 : Json
subject2 =
    set (String "Elm") subject1 d0
        |> Maybe.withDefault Null
    --
    -- == Object
    --     [ ( "a"
    --       , Object
    --           [ ( "b"
    --             , Object
    --                 [ ( "c"
    --                   , Array <|
    --                       Array.fromList
    --                           [ Null
    --                           , Bool True
    --                           , Bool False
    --                           , Int 5
    --                           , Float 3.14
    --                           , String "Hello"
    --                           , Object
    --                               [ ( "d", Array <| Array.fromList [ String "Elm" ] )
    --                               ]
    --                           ]
    --                   )
    --                 ]
    --             )
    --           ]
    --       )
    --     ]
    --


example2 : Json
example2 =
    get subject2 d0
        |> Maybe.withDefault Null
    --
    -- == String "Elm"
    --
```

## Public API

A high-level overview of the public API.

```elm
type Lens s a

get : s -> Lens s a -> Maybe a
set : a -> s -> Lens s a -> Maybe s

map : (a -> a) -> s -> Lens s a -> Maybe s

-- JSON

type Json
    = Null
    | Bool Bool
    | Int Int
    | Float Float
    | String String
    | Array (Array Json)
    | Object (List (String, Json))

-- PRIMITIVES

json : Lens Json Json
bool : Lens Json Bool
int : Lens Json Int
float : Lens Json Float
string : Lens Json String
array : Lens Json (Array Json)
list : Lens Json (List Json)
index : Int -> Lens Json Json
keyValuePairs : Lens Json (List (String, Json))
field : String -> Lens Json Json

-- OPERATIONS

nullable : Lens Json a -> Lens Json (Maybe a)
compose : Lens b c -> Lens a b -> Lens a c
at : List String -> Lens Json Json
```

## Further Reading

- [JSON lenses](https://grain-lang.org/blog/2025/04/28/new-release-grain-v0.7-farro/#JSON-lenses-%F0%9F%94%8D)
- [Racket: Lenses](https://docs.racket-lang.org/lens/index.html)
