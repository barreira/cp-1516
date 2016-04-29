--
-- Teste unitário
--
   
g1 :: Graph Int
g1 = Graph {nodes = fromList [1],
            edges = fromList [Edge 1 1]
           }
 
-- Um exemplo de um teste unitário.
test_adj :: Test
test_adj = adj g1 1 ~?= fromList [Edge 1 1]
 
--
-- Tarefa 1
--
-- Defina testes unitários para todas as funções do módulo Graph,
-- tentando obter o máximo de cobertura de expressões, condições, etc.
--
 
g0 :: Graph Int
g0 = Graph {nodes = fromList [],
            edges = fromList []
           }
 
g2 :: Graph Int
g2 = Graph {nodes = fromList [2, 3],
            edges = fromList [Edge 2 3]
           }
 
g2t :: Graph Int
g2t = Graph {nodes = fromList [2, 3],
             edges = fromList [Edge 3 2]
            }
 
g1_2 :: Graph Int
g1_2 = Graph {nodes = fromList [1, 2, 3],
              edges = fromList [Edge 1 1, Edge 2 3]
             }
 
g3 :: Graph Int
g3 = Graph {nodes = fromList [1, 2, 3, 4, 5, 6],
            edges = fromList [Edge 1 2, Edge 1 5, Edge 2 3, Edge 2 4, Edge 4 6]
           }
 
f_bft :: Graph Int
f_bft = Graph {nodes = fromList [1, 2, 3, 4, 5, 6],
               edges = fromList [Edge 5 1, Edge 4 2, Edge 6 4]
              }
 
conjV :: Set Int  
conjV = fromList [1, 2, 3]
 
path0 :: Graph.Path Int
path0 = []
 
path1 :: Graph.Path Int
path1 = [Edge 1 2, Edge 2 4, Edge 4 6]
 
g4 :: Graph Int
g4 = Graph {nodes = fromList [1, 2, 3, 4, 5, 6],
            edges = fromList [Edge 1 2, Edge 2 3, Edge 2 4, Edge 3 5, Edge 4 6, Edge 5 6]
           }
 
path_g4 :: Graph.Path Int
path_g4 = [Edge 1 2, Edge 2 4, Edge 4 6]
 
g5 :: Graph Int
g5 = Graph {nodes = fromList [1, 2, 3, 4, 5, 6, 7],
            edges = fromList [Edge 1 2, Edge 1 3, Edge 2 4, Edge 2 5, Edge 5 6, Edge 3 7, Edge 4 7, Edge 6 7]
           }
 
topo_g5 :: [Set Int]
topo_g5 = [fromList [1], fromList [2,3], fromList [4,5], fromList [6], fromList [7]]
fromList [1,2,3,4,5,6,7]
 
 
test_swap :: Test
test_swap = swap (Edge 1 2) ~?= (Edge 2 1)
 
test_empty :: Test
test_empty = Graph.empty ~?= g0
 
test_isEmpty :: Test
test_isEmpty = isEmpty g1 ~?= False
 
test_isValid :: Test
test_isValid = isValid g1 ~?= True
 
test_isDAG :: Test
test_isDAG = isDAG g2 ~?= True
 
test_isForest :: Teste
test_isForest = isForest g2 ~?= True
 
test_isSubgraphOf :: Test
test_isSubgraphOf = isSubgraphOf g2 g1_2 ~?= True
 
test_transpose :: Test
test_transpose = transpose g2 ~?= g2t
 
test_union :: Test
test_union = Graph.union g1 g2 ~?= g1_2
 
test_bft :: Test
test_bft = bft g3 conjV ~?= f_bft
 
-- reachable TESTADA AO MESMO TEMPO QUE isDAG
 
test_isPathOf :: Test
test_isPathOf = TestList [isPathOf path1 g3 ~?= True, isPathOf path0 g1 ~?= True]
 
test_path :: Test
test_path = path g4 1 6 ~?= Just path_g4
 
test_topo :: Test
test_topo = topo g5 ~?= topo_g5
 
test_Eq :: Test
test_Eq = TestList [(Edge 1 1) == (Edge 1 1) ~?= True, (Edge 1 1) /= (Edge 1 1) ~?= False]
 
main = runTestTT $ TestList [test_adj, test_swap, test_empty, test_isEmpty, test_isValid, test_isDAG, test_isForest, test_isSubgraphOf, test_transpose, test_union, test_bft, test_isPathOf, test_path, test_topo, test_Eq]