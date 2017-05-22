import os.path

from datasetutils import get_filepaths
from datasetutils import RawToResults

if __name__ == '__main__':
    files = get_filepaths('results/')
    for f in files:
        if f.endswith('.raw.csv'):
            f_out = f.replace('raw', 'result')
            if os.path.isfile(f_out):
                print('[cancel] already exist:' ,f_out)
                continue

            print('[process] raw file {0} is converting to {1} result file'.format(f, f_out))
            RawToResults(f, f_out, verboseError=False)
