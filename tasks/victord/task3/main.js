'use strict';

const BinaryHeap = require('@tyriar/binary-heap'); // https://github.com/gwtw/js-binary-heap

const edges = [
  [1, 2, 7],
  [1, 3, 3],
  [2, 4, 8],
  [2, 5, 3],
  [3, 2, 1],
  [3, 4, 2],
  [3, 5, 1],
  [4, 6, 2],
  [4, 1, 2],
  [5, 4, 9],
  [5, 6, 4],
  [5, 8, 1],
  [6, 3, 4],
  [7, 8, 10],
  [9, 10, 3]
];

const graph = {};
edges.forEach(([vertex1, vertex2, weight]) => {
  if (!graph[vertex1]) {
    graph[vertex1] = {};
  }
  graph[vertex1][vertex2] = weight;
});

const vertexes = Object.keys(graph);

//      1    2    3    4    5    6    7    8    9    10
//      __________________________________________________
// 1   |0    4    3    5    4    7    -    5    -    -
// 2   |10   0    11   8    3    7    -    4    -    -
// 3   |4    1    0    2    1    4    -    2    -    -
// 4   |2    6    5    0    6    2    -    7    -    -
// 5   |11   9    8    9    0    4    -    1    -    -
// 6   |8    5    4    6    5    0    -    6    -    -
// 7   |-    -    -    -    -    -    0    10   -    -
// 8   |-    -    -    -    -    -    -    0    -    -
// 9   |-    -    -    -    -    -    -    -    0    3
// 10  |-    -    -    -    -    -    -    -    -    0

function dijkstra(startVertex) {
  const heap = new BinaryHeap();

  const distances = {};
  vertexes.forEach(vertex => {
    distances[vertex] = vertex === startVertex ? 0 : Infinity;
    heap.insert(distances[vertex], vertex);
  });

  for (const visited = new Set(); visited.size < vertexes.length;) {
    const nearestVertex = heap.extractMinimum().value;

    for (let neighbour in graph[nearestVertex]) {
      if (visited.has(neighbour)) { continue; }

      const distance = distances[nearestVertex] + graph[nearestVertex][neighbour];
      if (distance < distances[neighbour]) {
        distances[neighbour] = distance;
        heap.insert(distance, neighbour);
      }
    }

    visited.add(nearestVertex);
  }

  return distances;
}
test(dijkstra, 1000);

function bellmanFord(startVertex) {
  const distances = {};
  vertexes.forEach(vertex => {
    distances[vertex] = Infinity;
  });
  distances[startVertex] = 0;

  for (let i = 1; i < vertexes.length; i++) {
    let wasChanged = false;

    edges.forEach(([vertex1, vertex2, weight]) => {
      const distance = distances[vertex1] + weight;
      if (distance < distances[vertex2]) {
        distances[vertex2] = distance;
        wasChanged = true;
      }
    });

    if (!wasChanged) { break; }
  }

  return distances;
}
test(bellmanFord, 1000);

function test(func, times) {
  const label = `test ${func.name} ${times} times`;
  let distances;

  console.time(label);
  for (let i = 0; i < times; i++) {
    distances = {};
    vertexes.forEach(vertex => {
      distances[vertex] = func(vertex);
    });
  }
  console.timeEnd(label);

  plotDistances(distances);
}

function plotDistances(distances) {
  Object.entries(distances).forEach(([vertex, distances]) => {
    if (vertex === '1') {
      console.log('    |', Object.keys(distances).map(align).join(''));
      console.log('-----------------------------------')
    }
    console.log(align(vertex), '|', Object.values(distances).map(distance => align(distance === Infinity ? '-' : distance)).join(''));
  });
  console.log();
}

function align(str) {
  return (str + '   ').slice(0, 3);
}
