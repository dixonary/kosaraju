{-# LANGUAGE OverloadedLists, OverloadedStrings #-}

module Data.VASS.Examples where

import qualified Data.Map as Map

import Data.VASS

exampleOne = VASS {
    states      = ["q1","q2","q3"],
    transitions = Map.fromList 
    [
        ("q1", 
            [ ("t_a", ([0,0], [1,0], "q2")) -- a
        ]),
        ("q2", 
            [ ("t_b", ([1,0], [0,0], "q2")) -- b
            , ("t_c", ([0,0], [0,1], "q3")) -- c
        ]),
        ("q3", 
            [ ("t_d", ([0,1], [0,0], "q3")) -- d
            , ("t_e", ([0,0], [0,1], "q1")) -- e
        ])
    ]
}