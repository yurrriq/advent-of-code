#! /usr/bin/env python

from collections import defaultdict

nodes = set()
edges = defaultdict(dict)
with open("./input/2015/day09.txt") as input:
    for line in input:
        source, _, destination, _, distance = line.rstrip().split(" ")
        nodes.add(source)
        nodes.add(destination)
        edges[source][destination] = distance
        edges[destination][source] = distance

sources = sorted(list(nodes))

C = [[0] + [1 for node in sources]]

for i, source in enumerate(sources):
    row = [1]
    destinations = edges[source].keys()
    for destination in sources:
        if destination in destinations:
            row.append(edges[source][destination])
        else:
            row.append(0)
    C.append(row)

print("{" + ",\n".join(["{" + ",".join(map(str, row)) + "}" for row in C]) + "}")
