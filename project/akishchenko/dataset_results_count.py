import os
from datasetutils import CsvStats

def get_filepaths(directory):
    file_paths = []  # List which will store all of the full filepaths.

    # Walk the tree.
    for root, directories, files in os.walk(directory):
        for filename in files:
            # Join the two strings in order to form the full filepath.
            filepath = os.path.join(root, filename)
            file_paths.append(filepath)  # Add it to the list.

    return file_paths  # Self-explanatory.

if __name__ == '__main__':
    files = get_filepaths("results/")
    for f in files:
        if f.endswith("result.csv"):
            stats = CsvStats(f)
            print(stats)

