# -*- coding: utf-8 -*-
from pprint import pprint
import sys


def ugliness(text, limit):
    lines = text.split("\n")
    return sum((limit - len(line))**3 for line in lines)


def align_text(lines, limit):
    words = sum([line.split(" ") for line in lines], [])
    n = len(words)

    extra = [[0]*n for _ in xrange(n)]
    for i in xrange(n):
        extra[i][i] = limit - len(words[i])
        for j in xrange(i+1, n):
            extra[i][j] = extra[i][j-1] - len(words[j]) - 1
    pprint(extra)
    res = [sys.maxint]*(n+1)
    res[0] = 0
    path = [0]*(n+1)
    for j in xrange(n):
        for i in xrange(j, -1, -1):
            price = sys.maxint if extra[i][j] < 0 else res[i] + extra[i][j]**3
            if res[j+1] > price:
                res[j+1] = price
                path[j] = i
    print path
    res_lines = []
    curr = n
    while curr > 0:
        res_lines.append(' '.join(words[path[curr-1]:curr]))
        curr = path[curr-1]
    res_lines.reverse()
    return "\n".join(res_lines)


limit = 61
text = u"""Необходимо написать функцию, которая получает на вход размер
рюкзака (целое число) и набор
предметов, каждый из которых имеет определенную ценность и
вес (целое число),
и выдает сочетание
предметов, которое максимизирует общую ценность рюкзака,
укладываясь
в ограничение по весу. Каждый из предметов
может быть помещен в рюкзак 1 или более раз."""
print text
print "Ugliness: ", ugliness(text, limit)

lines = text.split(u"\n")
aligned_text = align_text(lines, limit)
print aligned_text
print "Ugliness: ", ugliness(aligned_text, limit)

