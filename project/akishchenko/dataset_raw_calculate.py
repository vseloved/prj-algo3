import os.path
from datasetutils import get_filepaths
from perceptualhash import PerceptualFastAvg

def calculate_fast_avg(files):
    print("start calculating: ~{0} images files".format(len(files)))
    fn = 'results/fastavg.raw.csv'
    if os.path.isfile(fn):
        print("[cancel] already exist:" ,fn)
        return

    print('[start]' ,fn)
    with open(fn, 'w') as out_file:
        out_file.write('"file","hash"\n')
        total = 0
        for f in files:
            if f.endswith('.jpg'):
                total += 1
                pfa = PerceptualFastAvg(f)
                res = pfa.hashRes
                out_file.write(str(pfa))
                if total % 1000 == 0:
                    print("\talready calculated:" ,total)

        print("[done]total: {0} in {1}".format(total, fn))

if __name__ == '__main__':
    files = get_filepaths('images/')
    calculate_fast_avg(files)
