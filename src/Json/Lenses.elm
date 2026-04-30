module Json.Lenses exposing
    ( Lens, get, set
    , map
    , Json(..)
    , json
    , bool, int, float, string, list, keyValuePairs
    , field
    , nullable
    , compose
    )


type Lens s a =
    --
    -- s represents the subject
    -- a represents the value
    --
    Lens
        { get : s -> Maybe a
        , set : a -> s -> Maybe s
        }


get : s -> Lens s a -> Maybe a
get subject (Lens lens) =
    lens.get subject


set : a -> s -> Lens s a -> Maybe s
set newValue subject (Lens lens) =
    lens.set newValue subject


map : (a -> a) -> s -> Lens s a -> Maybe s
map f subject (Lens lens) =
    case lens.get subject of
        Just value ->
            lens.set (f value) subject

        Nothing ->
            Nothing


--
-- I didn't expect map to be defined as it was done above.
--
-- Why?
--
-- I thought map would have the signature: (a -> b) -> Lens s a -> Lens s b.
--
-- Is that possible? Let's try:
--
--fmap : (a -> b) -> Lens s a -> Lens s b
--fmap f (Lens lens) =
--    Lens
--        { get = \subject ->
--            Maybe.map f (lens.get subject)
--        , set = \value subject ->
--            lens.set x subject
--            --
--            -- What is x?
--            --
--            --    value : b
--            -- lens.set : a -> s -> Maybe s
--            --        f : a -> b
--            --
--            -- There's no a and no way to get an a.
--            --
--            -- i.e. unless f has an inverse.
--            --
--        }
--
-- Does fmap exist on Lens s a? i.e. is Lens s a functor?
-- https://claude.ai/share/f0aefe33-47c2-4fea-bb2a-997806486d96
--


type Json
    = Null
    | Bool Bool
    | Int Int
    | Float Float
    | String String
    | Array (List Json)
    | Object (List (String, Json))


-- PRIMITIVES


json : Lens Json Json
json =
    Lens
        { get = Just
        , set = \newValue _ -> Just newValue
        }
--
-- get (String "abc") json == Just (String "abc")
--


bool : Lens Json Bool
bool =
    Lens
        { get = \subject ->
            case subject of
                Bool b ->
                    Just b

                _ ->
                    Nothing
        , set = \b _ -> Just (Bool b)
        }


int : Lens Json Int
int =
    Lens
        { get = \subject ->
            case subject of
                Int n ->
                    Just n

                _ ->
                    Nothing
        , set = \n _ -> Just (Int n)
        }


float : Lens Json Float
float =
    Lens
        { get = \subject ->
            case subject of
                Float f ->
                    Just f

                _ ->
                    Nothing
        , set = \f _ -> Just (Float f)
        }


string : Lens Json String
string =
    Lens
        { get = \subject ->
            case subject of
                String s ->
                    Just s

                _ ->
                    Nothing
        , set = \s _ -> Just (String s)
        }


list : Lens Json (List Json)
list =
    Lens
        { get = \subject ->
            case subject of
                Array l ->
                    Just l

                _ ->
                    Nothing
        , set = \l _ -> Just (Array l)
        }


keyValuePairs : Lens Json (List (String, Json))
keyValuePairs =
    Lens
        { get = \subject ->
            case subject of
                Object ps ->
                    Just ps

                _ ->
                    Nothing
        , set = \ps _ -> Just (Object ps)
        }


field : String -> Lens Json Json
field name =
    Lens
        { get = \subject ->
            case subject of
                Object ps ->
                    find name ps

                _ ->
                    Nothing
        , set = \newValue subject ->
            case subject of
                Object ps ->
                    Just (Object (update name newValue ps))

                _ ->
                    Nothing
        }


-- OPERATIONS


nullable : Lens Json a -> Lens Json (Maybe a)
nullable (Lens lens) =
    Lens
        { get = \subject ->
            case subject of
                Null ->
                    Just Nothing

                _ ->
                    case lens.get subject of
                        Just value ->
                            Just (Just value)

                        Nothing ->
                            Nothing
        , set = \maybeNewValue subject ->
            case maybeNewValue of
                Just newValue ->
                    lens.set newValue subject

                Nothing ->
                    Just Null
        }
--
-- Tests:
--
-- get (Number 123) (nullable int) == Just (Just 123)
-- get Null (nullable int) == Just Nothing
-- get (String "abc") (nullable int) == Nothing
-- set (Just 123) (String "abc") (nullable int) == Just (Number 123)
--


--
-- TODO:
--
-- [x] Reverse lens composition
-- [ ] at
--


compose : Lens b c -> Lens a b -> Lens a c
compose (Lens lens2) (Lens lens1) =
    Lens
        { get = \a ->
            case lens1.get a of
                Just b ->
                    lens2.get b

                Nothing ->
                    Nothing
        --
        -- lens2.set : c -> b -> Maybe b
        -- lens1.set : b -> a -> Maybe a
        --       set : c -> a -> Maybe a
        --
        -- Where does the b come from?
        --
        -- Idea: Get it from lens1.
        --
        , set = \c a ->
            case lens1.get a of
                Just b1 ->
                    case lens2.set c b1 of
                        Just b2 ->
                            lens1.set b2 a

                        Nothing ->
                            Nothing

                Nothing ->
                    Nothing
        }
--
-- Tests:
--
-- get (Object [("x", Int 123)]) (field "x" |> compose int) == Just 123
-- set "new" (Object [("x", Int 123)]) (field "x" |> compose string) == Just (Object [("x", String "new")])
-- ^
-- |-- I don't like the order of set's parameters
--
-- But I don't think I'd prefer the following:
--
--   set : s -> a -> Lens s a -> Maybe s
--
-- And, I do want the lens to be the last parameter.
--
-- Maybe the lens parameter shouldn't be last.
--
--   get : Lens s a -> s -> Maybe a
--   set : Lens s a -> a -> s -> Maybe s
--
-- The example from the article:
--
-- metadata = Object [("currency", String "$"), ("price", Int 119)]
-- price = get metadata (field "price" |> compose int) |> Maybe.withDefault 0
--


-- HELPERS


find : a -> List (a, b) -> Maybe b
find needle haystack =
    case haystack of
        [] ->
            Nothing

        (key, value) :: rest ->
            if needle == key then
                Just value

            else
                find needle rest


update : a -> b -> List (a, b) -> List (a, b)
update =
    updateHelper []


updateHelper : List (a, b) -> a -> b -> List (a, b) -> List (a, b)
updateHelper result key newValue assoc =
    case assoc of
        [] ->
            List.reverse result

        (currentKey, value) :: rest ->
            if key == currentKey then
                List.reverse result ++ ((key, newValue) :: rest)

            else
                updateHelper ((currentKey, value) :: result) key newValue rest
