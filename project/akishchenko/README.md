# Image duplicate finder
Utils and algorithms to find image duplicates

## Python versions
Should work on python3 version >= 3.2  
HIGH recomendation use **PyPy3** or calculation will be so slow... :(

I've tried on PyPy for windows
```
Python 3.2.5 (b2091e973da6, Oct 19 2014, 21:25:51)
PyPy 2.4.0 with MSC v.1500 32 bit] on win32
```

## General workflow
- download image dataset to /images (manual or auto) (i've created zip with all lun.ua images on my google drive)
- run dataset_raw_calculate (calculate images hashes and dump to /results (*.raw.csv)
- run dataset_raw_to_result for image grouping (/results/*.raw.csv -> /results/*.result.csv)
- run results_process for showing results

## Result examples
```
file: results/correct.result.csv
         total files: 23513, unique groups: 500
         in group, min: 21, max: 119, avg: 47.03

file: results/fastavg.result.csv
         total files: 23513, unique groups: 921
         in group, min: 1, max: 599, avg: 25.53
```

### Images folder
There are should be all *.jpg files  
Look *readme* in images folder  
*lun_image_clusters_training_set.csv* --> file from lun.ua team

### Results folder
- *.raw.csv files
- *.result.csv files