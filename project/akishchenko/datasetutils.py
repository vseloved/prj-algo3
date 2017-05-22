import csv
import os
from collections import defaultdict

def get_filepaths(directory):
    file_paths = []  # List which will store all of the full filepaths.

    # Walk the tree.
    for root, directories, files in os.walk(directory):
        for filename in files:
            # Join the two strings in order to form the full filepath.
            filepath = os.path.join(root, filename)
            file_paths.append(filepath)  # Add it to the list.

    return file_paths  # Self-explanatory.

def get_hamming_distance(a, b):
    """calculate hamming (bit) distance between two int64 bits list"""
    diff = 0
    for i in range(len(a)):
        if a[i] != b[i]: diff += 1
    return diff / 64.0

class RawToResults:

    class RawNode:
        def __init__(self, name, hashRes, assign=-1):
            self.name = name
            self.hashRes = hashRes
            self.assign = assign
            self.bits = list(bin(hashRes)[2:].zfill(64))

        def __str__(self):
            return "(n:{0})".format(self.name)

    def __init__(self, fn, fn_out, distThreshold = 0.15, verboseError = False):
        self.fn = fn
        self.fn_out = fn_out
        self.rawList = []
        self.distThreshold = distThreshold
        self.verboseError = verboseError

        with open(fn) as csvfile:
            reader = csv.DictReader(csvfile)
            for row in reader:
                node = RawToResults.RawNode(row['file'], int(row['hash']))
                self.rawList.append(node)

        print("\ttotal nodes: " ,len(self.rawList))
        self.calculate()
        print("\tcalculated nodes, save to file...")
        self.save_to_file()
        print("\tsaved")

    def calculate(self):
        self.errors = 0
        for i in range(len(self.rawList) - 1):
            if i % 1000 == 0: print("\talready calculated: {0}, errors: {1}".format(i, self.errors))

            for k in range(i, len(self.rawList)):
                n1 = self.rawList[i]
                n2 = self.rawList[k]
                #skip already similar images
                if n1.assign > 0 and n2.assign > 0 and n1.assign == n2.assign: continue

                dist = get_hamming_distance(n1.bits, n2.bits)
                if dist <= self.distThreshold:
                    if n1.assign > 0 and n2.assign > 0:
                        if self.verboseError:
                            print('[error] similar: {0} and {1} but another assign...'.format(n1, n2))
                        self.errors += 1
                    else:
                        if n1.assign < 0: n1.assign = n1.hashRes
                        n2.assign = n1.assign

    def save_to_file(self):
        with open(self.fn_out, 'w') as out_file:
            out_file.write('"file","cluster"\n')
            for n in self.rawList:
                out_file.write('"{0}",{1}\n'.format(n.name, n.assign))


class CsvStats:
    def __init__(self, fn):
        self.fn = fn
        with open(fn) as csvfile:
            reader = csv.DictReader(csvfile)
            self.total = 0
            self.uniqueGroup = {}
            for row in reader:
                self.total += 1
                cluster = row['cluster']
                if cluster not in self.uniqueGroup:
                    self.uniqueGroup[cluster] = 1
                else:
                    self.uniqueGroup[cluster] += 1

    def __str__(self):
        valMin = 1 << 32
        valMax = -1
        avg = 0
        for k, v in self.uniqueGroup.items():
            valMin = min(valMin, v)
            valMax = max(valMax, v)
            avg += v

        avg /= len(self.uniqueGroup)
        return "file: {0}\n\t total files: {1}, unique groups: {2}\n\t in group, min: {3}, max: {4}, avg: {5:.2f}\n".format(self.fn, self.total, len(self.uniqueGroup), valMin, valMax, avg)


if __name__ == '__main__':
    fname = 'results/correct.result.csv'
    stats = CsvStats(fname)
    print(stats)
