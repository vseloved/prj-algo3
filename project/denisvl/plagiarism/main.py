import os
import logging
from collections import defaultdict
import psutil
from rabinhash import RabinHash

logging.basicConfig(level=logging.DEBUG, format='%(asctime)s %(levelname)-6s %(message)s')

SOURCE_PATH = 'pan-plagiarism-corpus-2011/source-document/'
SOURCE_FILES = ['part13/source-document06022.txt', 'part13/source-document06021.txt', 'part13/source-document06020.txt']

SUSPICIOUS_PATH = 'pan-plagiarism-corpus-2011/suspicious-document/'
SUSPICIOUS_FILES = ['part1/suspicious-document00007.txt']

WINDOW_SIZE = 64


def get_memory_usage():
    process = psutil.Process(os.getpid())
    return process.memory_info().rss

memory_usage = get_memory_usage()

source_hashes = []
for source in SOURCE_FILES:
    logging.info("Reading %s", source)
    with open(os.path.join(SOURCE_PATH, source), 'r') as f:
        text = f.read()
    logging.info("Source len = %d", len(text))
    rhash = RabinHash(text, WINDOW_SIZE)
    hashes = defaultdict(list)
    for h in rhash:
        hashes[h[0]].append(h[1])
    source_hashes.append(hashes)
    logging.info("Source hashes len: %d", len(source_hashes))

logging.info("Memory usage: %d ", get_memory_usage()-memory_usage)

for suspicoius in SUSPICIOUS_FILES:
    logging.info("Reading %s", suspicoius)
    with open(os.path.join(SUSPICIOUS_PATH, suspicoius), 'r') as f:
        text = f.read()
    logging.info("Suspicious len = %d", len(text))
    rhash = RabinHash(text, WINDOW_SIZE)
    res = 0
    for h in rhash:
        found = False
        for file_index, file_name in enumerate(SOURCE_FILES):
            if h[0] in source_hashes[file_index]:
                res += 1
                #logging.debug("Possible plagiarism: \n%s", text[h[1]:h[1]+WINDOW_SIZE])
    logging.info("Possible plagiarisms count: %d", res)

