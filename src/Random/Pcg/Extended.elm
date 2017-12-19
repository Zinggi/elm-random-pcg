module Random.Pcg.Extended exposing (initialSeed, config, int, float)

{-| This is an extended version of the Pcg random generator.
It offers k dimensional equidistributed random numbers.

It has a larger space cost than the normal Pcg version, but it also offers a much higher period

@docs initialSeed, config

@docs int, float

-}

import Array exposing (Array)
import Bitwise
import Random.General as RNG exposing (Config(..))
import Internal.Pcg


type Seed
    = Seed { extension : Array Int, base : Internal.Pcg.Seed }


type alias Generator a =
    RNG.Generator Seed a


{-| Seed this generator with some random data.
Best get this via a flag from `window.crypto.getRandomValues(..)`
-}
initialSeed : Int -> List Int -> Seed
initialSeed baseSeed extendedSeed =
    Seed { extension = Array.fromList extendedSeed, base = Internal.Pcg.initialSeed baseSeed }


{-| The config for the PCG-extended variant
-}
config : Config Seed
config =
    RNG.makeConfig next peel


{-| -}
int : Int -> Int -> Generator Int
int =
    RNG.int config


{-| -}
float : Float -> Float -> Generator Float
float =
    RNG.float config


next : Seed -> Seed
next (Seed s) =
    let
        newBase =
            Internal.Pcg.next s.base
    in
        Seed
            { base = newBase
            , extension =
                if newBase == Internal.Pcg.Seed 0 0 then
                    -- only advance the extension if we cross the all zero state for the base generator
                    incrementExtension s.extension
                else
                    s.extension
            }


peel : Seed -> Int
peel (Seed s) =
    -- As in section 7.1 of the paper
    let
        baseOut =
            Internal.Pcg.peel s.base

        ( randIndex, _ ) =
            RNG.step (Internal.Pcg.int 0 (Array.length s.extension - 1)) s.base

        extension =
            Array.get randIndex s.extension |> Maybe.withDefault 0
    in
        Bitwise.xor baseOut extension
            |> Bitwise.shiftRightZfBy 0


incrementExtension : Array Int -> Array Int
incrementExtension arr =
    -- Increment as described in section 4.3.4 of the PCG paper
    incrementExtensionHelp 0 0 arr


incrementExtensionHelp : Int -> Int -> Array Int -> Array Int
incrementExtensionHelp index prev arr =
    if prev == 0 then
        case Array.get index arr of
            Just elem ->
                let
                    newElem =
                        -- TODO: is this the right way to increment?
                        -- Or should it use a different increment for each element of the array?
                        elem + 1013904223 |> Bitwise.shiftRightZfBy 0
                in
                    incrementExtensionHelp (index + 1) newElem (Array.set index newElem arr)

            Nothing ->
                arr
    else
        arr
