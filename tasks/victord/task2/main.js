const graph = {
  1: ['2', '3'],
  2: ['5', '6'],
  3: ['5'],
  4: [],
  5: ['4'],
  6: ['4', '5']
};
const vertexes = Object.keys(graph);

const order = [];
const isSorted = new Set();
const currentPath = [];

while (isSorted.size < vertexes.length) {
  if (currentPath.length === 0) {
    currentPath.push(vertexes.find(v => !isSorted.has(v)));
  }

  const notSortedNeighbors = graph[currentPath[0]].filter(n => !isSorted.has(n));
  if (notSortedNeighbors.length > 0) {
    currentPath.unshift(notSortedNeighbors[0]);
  } else {
    isSorted.add(currentPath[0]);
    order.push(currentPath[0]);
    currentPath.shift();
  }
}

console.log('order', order);

// @see search over a graph in the task1/main.js.
