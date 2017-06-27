from pprint import pprint


def find(v, parent):
    while v != parent[v]:
        parent[v] = parent[parent[v]]
        v = parent[v]
    return v


def union(r1, r2, parent, rank):
    if rank[r1] < rank[r2]:
        parent[r1] = r2
        rank[r2] += rank[r1]
    else:
        parent[r2] = r1
        rank[r1] += rank[r2]


def kruskal(n, edges):
    edges = sorted(edges, key=lambda x: x[2])
    res = []
    parent = [i for i in xrange(n)]
    rank = [1 for _ in xrange(n)]
    curr = 0
    while len(res) < n-1:
        v1, v2, w = edges[curr]
        r1 = find(v1, parent)
        r2 = find(v2, parent)
        if r1 != r2:
            res.append(edges[curr])
            union(r1, r2, parent, rank)
        curr += 1
    return res


n = 6
edges = [
    (0, 1, 16),
    (0, 2, 13),
    (1, 2, 10),
    (1, 3, 12),
    (2, 1, 4),
    (2, 4, 14),
    (3, 2, 9),
    (3, 5, 20),
    (4, 3, 7),
    (4, 5, 4)
]

pprint(kruskal(n, edges))