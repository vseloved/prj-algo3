from datasetutils import get_filepaths
from datasetutils import RawToResults

if __name__ == '__main__':
    files = get_filepaths('results/')
    for f in files:
        if f.endswith('.raw.csv'):
            print('raw file {0} convert to result file'.format(f))
            RawToResults(f)
