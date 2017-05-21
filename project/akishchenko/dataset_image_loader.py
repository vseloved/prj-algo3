#load lun.ua dataset images from csv file
import csv
import requests
from PIL import Image
from io import BytesIO

def get_file_name(url, cluster, folder='images'):
    split = url.split('/')
    return "{0}/({1})_{2}".format(folder, cluster, split[-1])

def load_image(url, cluster):
    response = requests.get(url)
    img = Image.open(BytesIO(response.content))
    fname = get_file_name(url, cluster)
    img.save(fname)
    print('downloaded and saved:' , fname)

def load_csv(fname):
    with open(fname) as csvfile:
        reader = csv.DictReader(csvfile)
        for row in reader:
            print(row['image_url'], row['cluster_id'])
            load_image(row['image_url'], row['cluster_id'])

if __name__ == '__main__':
    fname = 'images/lun_image_clusters_training_set.csv'
    load_csv(fname)


