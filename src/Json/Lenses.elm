module Json.Lenses exposing
    ( Lens, get, set
    , map
    , Json(..)
    , json
    , bool, number, string, array, objectProperties
    , property
    , nullable
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
    | Number Float
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


number : Lens Json Float
number =
    Lens
        { get = \subject ->
            case subject of
                Number f ->
                    Just f

                _ ->
                    Nothing
        , set = \f _ -> Just (Number f)
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


array : Lens Json (List Json)
array =
    Lens
        { get = \subject ->
            case subject of
                Array a ->
                    Just a

                _ ->
                    Nothing
        , set = \a _ -> Just (Array a)
        }


objectProperties : Lens Json (List (String, Json))
objectProperties =
    Lens
        { get = \subject ->
            case subject of
                Object ps ->
                    Just ps

                _ ->
                    Nothing
        , set = \ps _ -> Just (Object ps)
        }


property : String -> Lens Json Json
property name =
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
-- get (Number 123) (nullable number) == Just (Just 123)
-- get Null (nullable number) == Just Nothing
-- get (String "abc") (nullable number) == Nothing
-- set (Just 123) (String "abc") (nullable number) == Just (Number 123)
--


--
-- TODO:
--
-- [ ] Reverse lens composition
-- [ ] propertyPath
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
