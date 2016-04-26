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
-- Número: A73831
-- Nome: João Pires Barreira
-- Curso: MIEI
--
-- Aluno 2
-- Número: A61855
-- Nome: Ana Paula Carvalho
-- Curso: MIEI
--


module Main where

import Graph
import Test.HUnit hiding (path)
import Test.QuickCheck
import Data.Set as Set

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

-- Deriving Eq
test_eq1 :: Test
test_eq1 = g1 == g1 ~?= True

test_eq2 :: Test
test_eq2 = g1 == g2 ~?= False

-- swap
test_swap :: Test
test_swap = swap (Edge 7 9) ~?= (Edge 9 7)

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

-- transpose

-- union

-- bft

-- reachable

-- isPathOf

-- path

-- topo

main = runTestTT $ TestList [test_adj]

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
        where aux = do ns <- arbitrary
                       es <- arbitrary
                       return $ Graph {nodes = fromList ns, edges = fromList es}
 
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
