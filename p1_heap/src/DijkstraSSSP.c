#include <stdlib.h>
#include <stdio.h>
#include <BinaryHeap.h>
#include <FibonacciHeap.h>


/*
Algorithm Dijkstra(V, E, w, s)
Q := MakeQueue
dist[s] := 0
Insert(Q, s, 0)
for v ∈ V \ { s } do
{ s } do 
dist[v] := +∞
Insert(Q, v, +∞)
while Q ≠ ∅ do
v := DeleteMin(Q)
foreach u : (v, u) ∈ E do
if u ∈ Q and dist[v]+w(v, u) < dist[u] then
[ ] ( , )
[ ]
dist[u] := dist[v]+w(v, u)
DecreaseKey(u, dist[u])
*/