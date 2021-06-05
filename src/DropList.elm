module DropList exposing (DropList, Item, empty, fromList, mapToList, push, remove, toValueList, getValue)

import Array exposing (Array)


type DropList a
    = DropList (Array (Removable a))


type alias Index =
    Int


type Item a
    = Item Index a


getValue : Item a -> a
getValue (Item _ a) =
    a


type Removable a
    = Present a
    | Removed


empty : DropList a
empty =
    DropList Array.empty


push : a -> DropList a -> DropList a
push element (DropList array) =
    DropList (Array.push (Present element) array)


remove : Item a -> DropList a -> DropList a
remove (Item index _) (DropList array) =
    DropList (Array.set index Removed array)


fromList : List a -> DropList a
fromList =
    List.map Present >> Array.fromList >> DropList


toValueList : DropList a -> List a
toValueList (DropList array) =
    Array.foldr addPresent [] array


mapToList : (Item a -> b) -> DropList a -> List b
mapToList projection (DropList array) =
    array
        |> Array.toIndexedList
        |> List.filterMap toMaybeItem
        |> List.map projection


toMaybeItem : ( Index, Removable a ) -> Maybe (Item a)
toMaybeItem ( index, removable ) =
    case removable of
        Present element ->
            Just (Item index element)

        Removed ->
            Nothing


addPresent : Removable a -> List a -> List a
addPresent removable list =
    case removable of
        Present element ->
            element :: list

        Removed ->
            list
