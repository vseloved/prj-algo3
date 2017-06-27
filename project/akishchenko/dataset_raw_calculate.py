import os.path

import PIL.Image as Image

from datasetutils import get_filepaths
from perceptualhash import PerceptualFastAvg

def calculate_fast_avg(files, fname, method):
    print("start calculating: ~{0} images files".format(len(files)))
    fn = 'results/' + fname + '.raw.csv'
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
                pfa = method(f)
                res = pfa.hashRes
                out_file.write(str(pfa))
                if total % 1000 == 0:
                    print("\talready calculated:" ,total)

        print("[done]total: {0} in {1}".format(total, fn))

def get_fast_avg_default(fn):
    return PerceptualFastAvg(fn, Image.NEAREST, 8)

def get_fast_avg_bicubic(fn):
    return PerceptualFastAvg(fn, Image.BICUBIC, 8)

def get_fast_avg_lanczos(fn):
    return PerceptualFastAvg(fn, Image.LANCZOS, 8)

if __name__ == '__main__':
    files = get_filepaths('images/')
    calculate_fast_avg(files, 'fastavg_8x8_nearest', get_fast_avg_default)
    calculate_fast_avg(files, 'fastavg_8x8_bicubic', get_fast_avg_bicubic)
    calculate_fast_avg(files, 'fastavg_8x8_lanczos', get_fast_avg_lanczos)
