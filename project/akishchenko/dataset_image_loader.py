#load lun.ua dataset images from csv file
import csv
import requests
import shutil

def get_file_name(url, cluster, folder='images2'):
    split = url.split('/')
    return "{0}/({1})_{2}".format(folder, cluster, split[-1])

def load_image(url, cluster):
    fname = get_file_name(url, cluster)
    response = requests.get(url, stream=True)
    with open(fname, 'wb') as out_file:
        shutil.copyfileobj(response.raw, out_file)
    del response
    print('downloaded and saved:' , fname)

def load_csv(fname):
    with open(fname) as csvfile:
        reader = csv.DictReader(csvfile)
        count = 0
        for row in reader:
            print("[{0}] load: {1} {2}".format(count, row['image_url'], row['cluster_id']))
            load_image(row['image_url'], row['cluster_id'])
            count += 1

if __name__ == '__main__':
    fname = 'images/lun_image_clusters_training_set.csv'
    load_csv(fname)


