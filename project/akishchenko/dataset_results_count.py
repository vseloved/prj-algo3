from datasetutils import CsvStats
from datasetutils import get_filepaths

if __name__ == '__main__':
    files = get_filepaths("results/")
    for f in files:
        if f.endswith("result.csv"):
            stats = CsvStats(f)
            print(stats)

