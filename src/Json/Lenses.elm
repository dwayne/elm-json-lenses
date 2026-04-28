module Json.Lenses exposing (Lens, get, set, map, Json(..), json)

import Array exposing (Array)


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
    | Array (Array Json)
    | Object (List (String, Json))


json : Lens Json Json
json =
    Lens
        { get = Just
        , set = \newValue _ -> Just newValue
        }
--
-- get (String "abc") json == Just (String "abc")
--
