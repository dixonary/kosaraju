module Data.GVASS.Examples where

import qualified Data.Vector as Vector
import Data.Vector (Vector)
import qualified Data.Map as Map
import Data.Map (Map)

import Data.GVASS
import Data.VASS

{- | A GVASS consisting of one component with two states.
    SCC decomposition should give us one GVASS comprising two components,
    each with one state.
-}

-- Can you get from [1,0] to [0,1] via a single transition with delta [-1,1]?
-- Hint: Yes
ex1 :: GVASS
ex1 = GVASS
    [ Component
        { dimension   = 2
        , states      = ["a", "b"]
        , transitions = [("a", [Transition 
            { name      = "t0"
            , pre       = [1,0]
            , post      = [0,1]
            , nextState = "b"
            }])]
        , initialState = "a"
        , finalState   = "b"
        , rigidCoords  = mempty
        , rigidValues  = mempty
        , initialConstrainedCoords = [0,1]
        , initialUnconstrainedCoords = []
        , finalConstrainedCoords = [0,1]
        , finalUnconstrainedCoords = []
        , initialVector = [(0,1), (1,0)]
        , finalVector   = [(0,0), (1,1)]
        , adjoinment    = Nothing
        }
    ]

{- | A GVASS consisting of two components. The second component is simple;
    the first is a graph with two routes of length 3.
    SCC decomposition should give two GVASSs each of four components.
-}
ex2 :: GVASS
ex2 = GVASS
    [ Component
        { dimension   = 2
        , states      = ["a", "b", "c", "d"]
        , transitions =
            [ ("a", [Transition 
                { name      = "t0"
                , pre       = [0,0]
                , post      = [10,0]
                , nextState = "b"
                }, Transition 
                { name      = "t1"
                , pre       = [0,0]
                , post      = [0,10]
                , nextState = "c"
                }])
            , ("b", [Transition 
                { name      = "t2"
                , pre       = [0,0]
                , post      = [0,0]
                , nextState = "d"
                }])
            , ("c", [Transition 
                { name      = "t3"
                , pre       = [0,0]
                , post      = [0,0]
                , nextState = "d"
                }])
            ]
        , initialState = "a"
        , finalState   = "d"
        , rigidCoords  = mempty
        , rigidValues  = mempty
        , initialConstrainedCoords = [0]
        , initialUnconstrainedCoords = [1]
        , finalConstrainedCoords = [1]
        , finalUnconstrainedCoords = [0]
        , initialVector = [(0,10)]
        , finalVector   = [(1,20)]
        , adjoinment    = Just []
        }

    , Component
        { dimension   = 2
        , states      = ["e"]
        , transitions = []
        , initialState = "e"
        , finalState   = "e"
        , rigidCoords  = mempty
        , rigidValues  = mempty
        , initialConstrainedCoords = []
        , initialUnconstrainedCoords = [0,1]
        , finalConstrainedCoords = []
        , finalUnconstrainedCoords = [0,1]
        , initialVector = []
        , finalVector   = []
        , adjoinment    = Nothing
        } 
    ]



{- | The same initial component as in ex2, but repeated in order to test that
   the combinations are correctly identified.
-}
ex3 :: GVASS
ex3 = GVASS
    [ Component
        { dimension   = 2
        , states      = ["a", "b", "c", "d"]
        , transitions =
            [ ("a", [Transition 
                { name      = "t0"
                , pre       = [0,0]
                , post      = [10,0]
                , nextState = "b"
                }, Transition 
                { name      = "t1"
                , pre       = [0,0]
                , post      = [0,10]
                , nextState = "c"
                }])
            , ("b", [Transition 
                { name      = "t2"
                , pre       = [0,0]
                , post      = [0,0]
                , nextState = "d"
                }])
            , ("c", [Transition 
                { name      = "t3"
                , pre       = [0,0]
                , post      = [0,0]
                , nextState = "d"
                }])
            ]
        , initialState = "a"
        , finalState   = "d"
        , rigidCoords  = mempty
        , rigidValues  = mempty
        , initialConstrainedCoords = [0]
        , initialUnconstrainedCoords = [1]
        , finalConstrainedCoords = [1]
        , finalUnconstrainedCoords = [0]
        , initialVector = [(0,100)]
        , finalVector   = [(1,200)]
        , adjoinment    = Just []
        }

    , Component
        { dimension   = 2
        , states      = ["a2", "b2", "c2", "d2"]
        , transitions =
            [ ("a2", [Transition 
                { name      = "t0"
                , pre       = [0,0]
                , post      = [10,0]
                , nextState = "b2"
                }, Transition 
                { name      = "t1"
                , pre       = [0,0]
                , post      = [0,10]
                , nextState = "c2"
                }])
            , ("b2", [Transition 
                { name      = "t2"
                , pre       = [0,0]
                , post      = [0,0]
                , nextState = "d2"
                }])
            , ("c2", [Transition 
                { name      = "t3"
                , pre       = [0,0]
                , post      = [0,0]
                , nextState = "d2"
                }])
            ]
        , initialState = "a2"
        , finalState   = "d2"
        , rigidCoords  = mempty
        , rigidValues  = mempty
        , initialConstrainedCoords = [0]
        , initialUnconstrainedCoords = [1]
        , finalConstrainedCoords = [1]
        , finalUnconstrainedCoords = [0]
        , initialVector = [(0,100)]
        , finalVector   = [(1,200)]
        , adjoinment    = Nothing
        }
    ]


-- | The example described by Ranko in his paper.
ex4 :: GVASS
ex4 = GVASS
    [ Component
    { dimension   = 3
    , states      = ["α", "a", "b", "b'","c", "c'", "d", "d'", "ω"]
    , transitions =
        [ ("α", 
          [ Transition 
            { name      = "t_incr"
            , pre       = [0,0,0]
            , post      = [1,1,0]
            , nextState = "a"
            }
          ]
        )
        ,  ("a", 
          [ Transition 
            { name      = "t_incr_2"
            , pre       = [0,0,0]
            , post      = [1,1,0]
            , nextState = "a"
            }
          , Transition 
            { name      = "t_ab"
            , pre       = [0,0,0]
            , post      = [0,0,0]
            , nextState = "b"
            }
          ]
        )
        ,  ("b", 
          [ Transition 
              { name      = "t_b_rep"
              , pre       = [1,0,0]
              , post      = [0,0,2]
              , nextState = "b"
              }
            , Transition 
              { name      = "t_bb'"
              , pre       = [0,0,0]
              , post      = [0,0,0]
              , nextState = "b'"
              }
          ]
        )
        ,  ("b'", 
          [ Transition 
              { name      = "t_b'_rep"
              , pre       = [0,0,1]
              , post      = [1,0,0]
              , nextState = "b'"
              }
            , Transition 
              { name      = "t_b'c"
              , pre       = [0,0,0]
              , post      = [0,0,0]
              , nextState = "c"
              }
          ]
        )
        ,  ("c", 
          [ Transition 
              { name      = "t_c_rep"
              , pre       = [2,0,0]
              , post      = [0,0,3]
              , nextState = "c"
              }
            , Transition 
              { name      = "t_cc'"
              , pre       = [0,0,0]
              , post      = [0,0,0]
              , nextState = "c'"
              }
          ]
        )
        ,  ("c'", 
          [ Transition 
              { name      = "t_c'_rep"
              , pre       = [0,0,1]
              , post      = [1,0,0]
              , nextState = "c'"
              }
            , Transition 
              { name      = "t_c'd"
              , pre       = [0,0,0]
              , post      = [0,0,0]
              , nextState = "d"
              }
          ]
        ),
        ("d", 
        [ Transition 
            { name      = "t_d_rep"
            , pre       = [3,0,0]
            , post      = [0,0,4]
            , nextState = "d"
            }
          , Transition 
            { name      = "t_dd'"
            , pre       = [0,0,0]
            , post      = [0,0,0]
            , nextState = "d'"
            }
        ]
        )
        ,  ("d'", 
          [ Transition 
            { name      = "t_d'_rep"
            , pre       = [0,0,1]
            , post      = [1,0,0]
            , nextState = "d'"
            }
          , Transition 
            { name      = "t_d'ω"
            , pre       = [0,0,0]
            , post      = [0,0,0]
            , nextState = "ω"
            }
          ]
        ) ,  ("ω", 
          [ Transition 
            { name      = "t_w_mul_sub"
            , pre       = [4,1,0]
            , post      = [0,0,0]
            , nextState = "ω"
            }
          , Transition 
            { name      = "t_w_extra_sub"
            , pre       = [1,0,0]
            , post      = [0,0,0]
            , nextState = "ω"
            }
          ]
        )
        ]
    , initialState = "α"
    , finalState   = "ω"
    , rigidCoords  = mempty
    , rigidValues  = mempty
    , initialConstrainedCoords = [0, 1, 2]
    , initialUnconstrainedCoords = mempty
    , finalConstrainedCoords = [0, 1, 2]
    , finalUnconstrainedCoords = mempty
    , initialVector = [(0,0), (1,0), (2,0)]
    , finalVector   = [(0,0), (1,0), (2,0)]
    , adjoinment    = Nothing
    }
    ]

-- | A very simple , parameterized example.
ex5 :: Integer -> GVASS
ex5 maxVal = GVASS
    [ Component
        { dimension   = 2
        , states      = ["a", "b"]
        , transitions =
            [ ("a", 
                [ Transition 
                { name      = "t0"
                , pre       = [0,1]
                , post      = [1,0]
                , nextState = "a"
                }, Transition 
                { name      = "t1"
                , pre       = [maxVal,0]
                , post      = [0, 0]
                , nextState = "b"
                }
                ])
            ]
        , initialState = "a"
        , finalState   = "b"
        , rigidCoords  = mempty
        , rigidValues  = mempty
        , initialConstrainedCoords = [0, 1]
        , initialUnconstrainedCoords = []
        , finalConstrainedCoords = [0, 1]
        , finalUnconstrainedCoords = []
        , initialVector = [(0,0), (1,maxVal)]
        , finalVector   = [(0,0), (1,0)]
        , adjoinment    = Just []
        }
    ]

-- Expected answer: no solutions
ex_ldn :: [([Integer], Integer)]
ex_ldn = 
    [ ([1,0,-1,0,1,0,0,0,0,0], 0)
    , ([0,1,0,-1,1,0,0,0,0,0], 0)
    , ([0,0,0,0,0,1,0,-1,0,-1],0)
    , ([0,0,0,0,0,0,1,0,-1,-1],0)
    , ([1,0,0,0,0,0,0,0,0,0],0)
    , ([0,1,0,0,0,0,0,0,0,0],0)
    , ([0,0,0,0,0,0,0,1,0,0],0)
    , ([0,0,0,0,0,0,0,0,1,0],0)
    , ([0,0,-1,0,0,1,0,0,0,0],0)
    , ([0,0,0,-1,0,0,1,0,0,0],1)
    ]