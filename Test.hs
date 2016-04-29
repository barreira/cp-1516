--
-- Projecto CP 2015/16
--
-- O projecto consiste em desenvolver testes para o módulo Graph.hs
-- (para grafos orientados e não pesados).
-- Mais concretamente, o projecto consiste em 3 tarefas que são descritas abaixo.
-- O prazo para entrega é o dia 3 de Abril. Cada grupo deve enviar apenas
-- o módulo de testes (este módulo) por email para calculodeprogramas@gmail.com
-- O nome do ficheiro deve identificar os números dos 2 alunos do grupo (numero1_numero2.hs).
-- Certifiquem-se que o módulo de testes desenvolvido compila correctamente antes
-- de submeter. O módulo Graph.hs não deve ser alterado.
-- Os 2 alunos do grupo devem também indentificar-se nos comentários abaixo.
--
-- Aluno 1
-- Número: A61855
-- Nome: Ana Paula Carvalho
-- Curso: MIEI
--
-- Aluno 2
-- Número: A73831
-- Nome: João Pires Barreira
-- Curso: MIEI
--


module Main where

import Graph
import Test.HUnit hiding (path)
import Test.QuickCheck
import Data.Set as Set
import Data.Maybe

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

-- Definição de alguns grafos auxiliares
g2 :: Graph Int
g2 = Graph {nodes = fromList [1,2],
            edges = fromList [Edge 2 3]
           }

g3 :: Graph Int
g3 = Graph {nodes = fromList [1,2,3],
            edges = fromList [Edge 1 2, Edge 2 3, Edge 3 1]
           }

g4 :: Graph Int
g4 = Graph {nodes = fromList [1,2,3,4],
            edges = fromList [Edge 1 2, Edge 2 3, Edge 2 4]
           }    

g5 :: Graph Int
g5 = Graph {nodes = fromList [7,8,9],
            edges = fromList [Edge 7 8, Edge 8 9]
           }

g6 :: Graph Int
g6 = Graph {nodes = fromList [6,7,8,9],
            edges = fromList [Edge 6 7, Edge 7 8, Edge 8 9]
           }

g7 :: Graph Int
g7 = Graph {nodes = fromList [1,2,3],
            edges = fromList [Edge 1 2, Edge 2 3]
           }

g8 :: Graph Int
g8 = Graph {nodes = fromList [],
            edges = fromList []
           }

g9 :: Graph Int
g9 = Graph {nodes = fromList [7,8,9,10,11,12,13],
            edges = fromList [Edge 7 8, Edge 8 9, Edge 8 10, Edge 9 11, Edge 7 12, Edge 12 13]
           }

g10 :: Graph Int
g10 = Graph {nodes = fromList [7,8,9,10,11,12,13],
            edges = fromList [Edge 12 7, Edge 10 8, Edge 13 12]
           }

-- Deriving Eq
test_eq1 :: Test
test_eq1 = g1 == g1 ~?= True

test_eq2 :: Test
test_eq2 = g1 == g2 ~?= False

-- swap
test_swap :: Test
test_swap = swap (Edge 7 9) ~?= (Edge 9 7)

-- empty
test_empty :: Test
test_empty = Graph.empty ~?= g8

-- isEmpty
test_isEmpty1 :: Test
test_isEmpty1 = isEmpty g1 ~?= False

test_isEmpty2 :: Test
test_isEmpty2 = isEmpty (Graph {nodes = Set.empty, edges = Set.empty}) ~?= True

-- isValid
test_isValid1 :: Test
test_isValid1 = isValid g2 ~?= False

test_isValid2 :: Test
test_isValid2 = isValid g1 ~?= True

-- isDAG
test_isDAG1 :: Test
test_isDAG1 = isDAG g1 ~?= True

test_isDAG2 :: Test
test_isDAG2 = isDAG g3 ~?= False

-- isForest
test_isForest1 :: Test
test_isForest1 = isForest g4 ~?= False

test_isForest2 :: Test
test_isForest2 = isForest g5 ~?= True

-- isSubgraphOf
test_isSubgraphOf1 :: Test
test_isSubgraphOf1 = isSubgraphOf g6 g5 ~?= False

test_isSubgraphOf2 :: Test
test_isSubgraphOf2 = isSubgraphOf g5 g6 ~?= True

-- adj
test_adj1 :: Test
test_adj1 = adj g5 8 ~?= fromList [Edge 8 9]

test_adj2 :: Test
test_adj2 = adj g5 8 ~?= fromList [Edge 7 8]

-- transpose
test_transpose1 :: Test
test_transpose1 = transpose g5 ~?= Graph {nodes = fromList [7,8,9],
                                          edges = fromList [Edge 8 7, Edge 9 8]
                                         }

test_transpose2 :: Test
test_transpose2 = transpose g5 ~?= Graph {nodes = fromList [7,8,9],
                                         edges = fromList [Edge 7 8, Edge 9 8]
                                         }

-- union
test_union :: Test
test_union = Graph.union g7 g5 ~?= Graph {nodes = fromList [1,2,3,7,8,9],
                                          edges = fromList [Edge 1 2, Edge 2 3, Edge 7 8, Edge 8 9]
                                         }                                 

-- bft
test_bft :: Test
test_bft = bft g9 (fromList [7,8,9,11]) ~?= g10

-- reachable
test_reachable :: Test
test_reachable = reachable g5 7 ~?= fromList [7,8,9]

-- isPathOf
test_isPathOf :: Test
test_isPathOf = isPathOf [Edge 7 8, Edge 8 10] g9 ~?= True

-- path
test_path :: Test
test_path = path g9 7 10 ~?= Just [Edge 7 8, Edge 8 10]

-- topo
test_topo :: Test
test_topo = topo g9 ~?= [fromList [7], fromList [8,12], fromList [9,10,13], fromList [11]]

main = runTestTT $ TestList [test_adj, test_eq1, test_eq2, test_swap, test_empty, test_isEmpty1, test_isEmpty2, test_isValid1, test_isValid2, test_isDAG1, test_isDAG2, test_isForest1, test_isForest2, test_isSubgraphOf1, test_isSubgraphOf2, test_adj1, test_adj2, test_transpose1, test_transpose2, test_union, test_reachable, test_bft, test_isPathOf, test_path, test_topo]

--
-- Teste aleatório
--

--
-- Tarefa 2
--
-- A instância de Arbitrary para grafos definida abaixo gera grafos
-- com muito poucas arestas, como se pode constatar testando a
-- propriedade prop_valid.
-- Defina uma instância de Arbitrary menos enviesada.
-- Este problema ainda é mais grave nos geradores dag e forest que
-- têm como objectivo gerar, respectivamente, grafos que satisfazem
-- os predicados isDag e isForest. Estes geradores serão necessários
-- para testar propriedades sobre estas classes de grafos.
-- Melhore a implementação destes geradores por forma a serem menos enviesados.
--

-- Instância de Arbitrary para arestas
instance Arbitrary v => Arbitrary (Edge v) where
    arbitrary = do s <- arbitrary
                   t <- arbitrary
                   return $ Edge {source = s, target = t}

instance (Ord v, Arbitrary v) => Arbitrary (Graph v) where
    arbitrary = aux `suchThat` isValid
        where aux = do es <- arbitrary
                       return $ Graph {nodes = Set.map source es `Set.union` Set.map target es,
                                       edges = es
                                      }

prop_valid :: Graph Int -> Property
prop_valid g = collect (length (edges g)) $ isValid g

-- Gerador de DAGs
dag :: (Ord v, Arbitrary v) => Gen (DAG v)
dag = arbitrary `suchThat` isDAG

prop_dag :: Property
prop_dag = forAll (dag :: Gen (DAG Int)) $ \g -> collect (length (edges g)) $ isDAG g

-- Gerador de florestas
forest :: (Ord v, Arbitrary v) => Gen (Forest v)
forest = arbitrary `suchThat` isForest

prop_forest :: Property
prop_forest = forAll (forest :: Gen (Forest Int)) $ \g -> collect (length (edges g)) $ isForest g

--
-- Tarefa 3
--
-- Defina propriedades QuickCheck para testar todas as funções
-- do módulo Graph.
--

-- Exemplo de uma propriedade QuickCheck para testar a função adj          
prop_adj :: Graph Int -> Property
prop_adj g = forAll (elements $ elems $ nodes g) $ \v -> adj g v `isSubsetOf` edges g

-- swap
prop_swap :: Edge Int -> Bool
prop_swap e = source e == target (swap e)

-- empty
prop_empty :: Bool
prop_empty = Set.null (nodes Graph.empty)

-- isEmpty
prop_isEmpty :: Graph Int -> Bool
prop_isEmpty g = isEmpty g == Set.null (nodes g) || Set.null (edges g)

-- isValid
prop_isValid :: Graph Int -> Property
prop_isValid g = isValid g ==> (isValid $ transpose g)

-- isDag
prop_isDAG :: Graph Int -> Property
prop_isDAG g = isDAG g ==> (isDAG $ transpose g) && (isValid $ transpose g)

-- isForest
prop_isForest :: Graph Int -> Property
prop_isForest g = isForest g ==> isForest $ transpose $ transpose g

-- isSubgraphOf
prop_isSubgraphOf :: Graph Int -> Graph Int -> Property
prop_isSubgraphOf g1 g2 = isSubgraphOf g1 g2 ==> isSubgraphOf (transpose g1) (transpose g2)

-- adj
prop_adj2 :: Graph Int -> Int -> Property
prop_adj2 g v = (isEmpty g) ==> (toList (adj g v) == [])

-- transpose
prop_transpose :: Ord v => Graph v -> Bool
prop_transpose g = size (nodes g) == size (nodes (transpose g))

-- union
prop_union :: Ord v => Graph v -> Graph v -> Bool
prop_union g g' = nodes g `isSubsetOf` nodes (Graph.union g g') && nodes g' `isSubsetOf` nodes (Graph.union g g')

-- bft
prop_bft :: Graph Int -> Set Int -> Bool
prop_bft g s = all (\x -> x `member` nodes (bft g s)) s

-- reachable
prop_reachable :: Graph Int -> Int -> Bool
prop_reachable g v = v `member` (reachable g v)

-- isPathOf
prop_isPathOf :: Graph.Path Int -> Graph Int -> Bool
prop_isPathOf p g = if (isPathOf p g && p /= []) then not(isEmpty g)
                                                 else True

-- path
prop_path :: Graph Int -> Int -> Int -> Bool
prop_path g v v' = if (isJust(path g v v')) then v' `member` reachable g v
                                            else not(v' `member` reachable g v)
-- topo
prop_topo :: Graph Int -> Property
prop_topo g = isDAG g ==> if (isEmpty g)
                          then topo g == []
                          else not(and(Prelude.map Set.null (topo g)))