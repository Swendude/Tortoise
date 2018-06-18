module Graphbuilder exposing (..)

import Debug exposing (log)
import Graph exposing (..)
import Graph.DOT exposing (output)
import IntDict exposing (..)
import TortoiseParser exposing (..)


type ControlFrame
    = REPEAT Int Int Int


type TortoiseNode l r
    = Simple l
    | Complex l r


printGraph : List Token -> String
printGraph tl =
    output newTokenToMaybe (\_ -> Nothing) (buildGraph tl)


newTokenToMaybe : TortoiseNode Token ControlFrame -> Maybe String
newTokenToMaybe t =
    case t of
        Simple l ->
            Just (TortoiseParser.tokenToText l)

        Complex l _ ->
            Just (TortoiseParser.tokenToText l)


buildGraph : List Token -> Graph (TortoiseNode Token ControlFrame) ()
buildGraph lt =
    List.foldr tokenIntoGraph Graph.empty lt


tokenIntoGraph : Token -> Graph (TortoiseNode Token ControlFrame) () -> Graph (TortoiseNode Token ControlFrame) ()
tokenIntoGraph token graph =
    case token of
        REPEAT_start n ->
            graph

        REPEAT_end ->
            graph

        _ ->
            let
                index =
                    Graph.size graph

                prev =
                    case index of
                        0 ->
                            IntDict.empty

                        i ->
                            IntDict.singleton (index - 1) ()

                newnode =
                    { node = Graph.Node index (Simple token)
                    , incoming = IntDict.empty
                    , outgoing = prev
                    }
            in
            Graph.insert newnode graph
