#simple check script
#ensure that opencv and numpy / matplotlib installed
import cv2
import numpy as np
from matplotlib import pyplot as plt

if __name__ == "__main__":
    img = cv2.imread('lena_test_image.png', 0)
    plt.imshow(img, cmap="gray")
    plt.show()
