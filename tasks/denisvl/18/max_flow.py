from pprint import pprint


def find_path(graph, source, sink):
    queue = [source]
    visited = [False] * len(graph)
    path = [None] * len(graph)
    while queue:
        v = queue.pop(0)
        for i in range(len(graph)):
            if not visited[i] and graph[v][i]:
                queue.append(i)
                visited[i] = True
                path[i] = v
    res = []
    curr = sink
    while path[curr] and curr != source:
        res.append((path[curr], graph[path[curr]][curr]))
        curr = path[curr]
    return res


def max_flow(graph, source, sink):
    res = 0
    while True:
        path = find_path(graph, source, sink)
        if not path:
            break

        min_edge = min(path, key=lambda x: x[1])[1]

        prev = sink
        for p in path:
            graph[p[0]][prev] -= min_edge
            graph[prev][p[0]] += min_edge
            prev = p[0]

        res += min_edge

    pprint(graph)
    return res

graph = [
    [0, 16, 13, 0, 0, 0],
    [0, 0, 10, 12, 0, 0],
    [0, 4, 0, 0, 14, 0],
    [0, 0, 9, 0, 0, 20],
    [0, 0, 0, 7, 0, 4],
    [0, 0, 0, 0, 0, 0]
]

print max_flow(graph, 0, 5)