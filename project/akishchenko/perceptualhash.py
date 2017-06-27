import PIL.Image as Image
from datasetutils import convert_bits_to_int

class PerceptualFastAvg:
    """
    Fast (but not accurate) perceptual hash calculating
    base on average color
    steps:
        1. reduce size
        2. reduce color
        3. average the colors
        4. compute the bits
        5. construct the hash
    """

    def __init__(self, fn, resample=Image.NEAREST, size=8, calculate=True):
        """ default size 8 (8x8 square, total 64 pixels)"""
        self.SIZE = size
        self.TOTAL = size * size
        self.resample = resample
        self.fn = fn
        self.avg = -1
        self.hashRes = -1

        if calculate: self.calculate()

    def reduce_size(self, img):
        return img.resize((self.SIZE, self.SIZE), self.resample)

    def calculate_avg(self, img):
        """Calculate average color value (gray)"""
        self.avg = 0
        for i in range(self.SIZE):
            for k in range(self.SIZE):
                self.avg += img.getpixel((i, k))

        self.avg /= self.TOTAL

    def compute_bits(self, img):
        """compute 64 bits from average color"""
        bits = [0] * self.TOTAL
        index = 0
        for i in range(self.SIZE):
            for k in range(self.SIZE):
                color = img.getpixel((i, k))
                bits[index] = 0 if color <= self.avg else 1
                index += 1

        self.hashRes = convert_bits_to_int(bits)

    def calculate(self):
        """load image and calculate hash"""
        im = Image.open(self.fn)
        res = self.reduce_size(im)
        res = res.convert(mode="L")

        self.calculate_avg(res)
        self.compute_bits(res)

        return self.hashRes

    def __str__(self):
        return "\"{0}\",{1}\n".format(self.fn, self.hashRes)

if __name__ == '__main__':
    fn = "lena_test_image.png"
    pfa = PerceptualFastAvg(fn)
    print(pfa)
