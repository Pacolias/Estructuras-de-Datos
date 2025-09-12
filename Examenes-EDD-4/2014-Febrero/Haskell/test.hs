-- Main.hs
-- Prueba de SCCs para examen Febrero 2014

import SCCs
import DataStructures.Graph.DiGraph
import DataStructures.Graph.Graph

main :: IO ()
main = do
  let vs = ['A','B','C','D','E','F','G', 'H']
      es = [ 'A' :-> 'B'
           , 'B' :-> 'E'
           , 'E' :-> 'A'
           , 'E' :-> 'F'
           , 'B' :-> 'F'
           , 'F' :-> 'G'
           , 'G' :-> 'F'
           , 'C' :-> 'D'
           , 'C' :-> 'G'
           , 'D' :-> 'C'
           , 'D' :-> 'H'
           , 'H' :-> 'G'
           , 'H' :-> 'D'
           ]
      g = mkDiGraphEdges vs es

  putStrLn "DiGraph:"
  print g

  putStrLn "\nreverseDiGraph:"
  print (reverseDiGraph g)

  putStrLn "\nrestrictDiGraph [A,B,F,G,E]:"
  print (restrictDiGraph g ['A','B','F','G','E'])

  putStrLn "\nsccOf g 'A':"
  print (sccOf g 'A')

  putStrLn "\nsccOf g 'C':"
  print (sccOf g 'C')

  putStrLn "\nsccs g:"
  print (sccs g)