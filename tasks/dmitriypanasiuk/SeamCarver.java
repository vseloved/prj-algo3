package dmitriypanasiuk;

import dmitriypanasiuk.StopWatch;

import java.awt.*;

public class SeamCarver {
    private int[][] colorsRGB;
    private int width;
    private int height;

    public SeamCarver(Picture picture) {
        if (picture == null) throw new NullPointerException();

        this.width = picture.width();
        this.height = picture.height();
        this.colorsRGB = new int[this.width][this.height];

        for (int i = 0; i < this.width; i++) {
            for (int j = 0; j < this.height; j++) {
                colorsRGB[i][j] = rgbFromInt(picture.get(i, j).getRed(), picture.get(i, j).getGreen(),
                        picture.get(i, j).getBlue());
            }
        }
    }

    public Picture picture() {
        Picture newPicture = new Picture(this.width, this.height);
        for (int i = 0; i < this.width; i++) {
            for (int j = 0; j < this.height; j++) {
                newPicture.set(i, j, new Color(getRed(colorsRGB[i][j]), getGreen(colorsRGB[i][j]),
                        getBlue(colorsRGB[i][j])));
            }
        }

        return newPicture;
    }

    public int width() {
        return this.width;
    }

    public int height() {
        return this.height;
    }

    public double energy(int x, int y) {
        if (x < 0 || x >= this.width || y < 0 || y >= this.height) {
            throw new IndexOutOfBoundsException();
        }
        if (x == 0 || y == 0 || x == this.width - 1 || y == this.height - 1) {
            return  1000.0;
        }
        return Math.sqrt(gradientX(x, y) + gradientY(x, y));
    }

    private double gradientX(int x, int y) {
        double dR = Math.abs(getRed(colorsRGB[x - 1][y]) - getRed(colorsRGB[x + 1][y]));
        double dG = Math.abs(getGreen(colorsRGB[x - 1][y]) - getGreen(colorsRGB[x + 1][y]));
        double dB = Math.abs(getBlue(colorsRGB[x - 1][y]) - getBlue(colorsRGB[x + 1][y]));

        return dR * dR + dG * dG + dB * dB;
    }

    private double gradientY(int x, int y) {
        double dR = Math.abs(getRed(colorsRGB[x][y - 1]) - getRed(colorsRGB[x][y + 1]));
        double dG = Math.abs(getGreen(colorsRGB[x][y - 1]) - getGreen(colorsRGB[x][y + 1]));
        double dB = Math.abs(getBlue(colorsRGB[x][y - 1]) - getBlue(colorsRGB[x][y + 1]));

        return dR * dR + dG * dG + dB * dB;
    }

    public int[] findHorizontalSeam() {
        double[][] paths = new double[width()][height()];
        double min = Double.POSITIVE_INFINITY;
        int indexMin = 0;
        int[] result = new int[width()];

        if (height() == 1) {
            return result;
        }
        for (int x = 0; x < width(); x++) {
            for (int y = 0; y < height(); y++) {
                double currentEnergy = energy(x, y);
                if (x == 0) {
                    paths[x][y] = currentEnergy;
                } else {
                    if (y == 0) {
                        paths[x][y] = currentEnergy + Math.min(paths[x - 1][y], paths[x - 1][y + 1]);
                    } else if (y == height() - 1) {
                        paths[x][y] = currentEnergy + Math.min(paths[x - 1][y], paths[x - 1][y - 1]);
                    } else {
                        paths[x][y] = currentEnergy + smallestOfThree(paths[x - 1][y - 1], paths[x - 1][y],
                                paths[x - 1][y + 1]);
                    }
                }
            }
        }

        for (int y = 0; y < height(); y++) {
            if (paths[width() - 1][y] < min) {
                min = paths[width() - 1][y];
                indexMin = y;
            }
        }
        result[width() - 1] = indexMin;
        for (int x = width() - 2; x >= 0; x--) {
            int currentMin = result[x + 1];
            if (currentMin == 0) {
                result[x] = indexYOfSmallestElement(paths, x, currentMin, currentMin + 1);
            } else if (currentMin == height() - 1) {
                result[x] = indexYOfSmallestElement(paths, x, currentMin - 1, currentMin);
            } else {
                result[x] = indexYOfSmallestElement(paths, x, currentMin - 1, currentMin, currentMin + 1);
            }
        }

        return result;
    }

    public int[] findVerticalSeam() {
        double[][] paths = new double[width()][height()];
        double min = Double.POSITIVE_INFINITY;
        int indexMin = 0;
        int[] result = new int[height()];

        if (width() == 1) {
            return result;
        }
        for (int y = 0; y < height(); y++) {
            for (int x = 0; x < width(); x++) {
                double currentEnergy = energy(x, y);
                if (y == 0) {
                    paths[x][y] = currentEnergy;
                } else {
                    if (x == 0) {
                        paths[x][y] = currentEnergy + Math.min(paths[x][y - 1], paths[x + 1][y - 1]);
                    } else if (x == width() - 1) {
                        paths[x][y] = currentEnergy + Math.min(paths[x][y - 1], paths[x - 1][y - 1]);
                    } else {
                        paths[x][y] = currentEnergy + smallestOfThree(paths[x - 1][y - 1], paths[x][y - 1],
                                paths[x + 1][y - 1]);
                    }
                }
            }
        }

        for (int x = 0; x < width(); x++) {
            if (paths[x][height() - 1] < min) {
                min = paths[x][height() - 1];
                indexMin = x;
            }
        }
        result[height() - 1] = indexMin;
        for (int y = height() - 2; y >= 0; y--) {
            int currentMin = result[y + 1];
            if (currentMin == 0) {
                result[y] = indexXOfSmallestElement(paths, y, currentMin, currentMin + 1);
            } else if (currentMin == width() - 1) {
                result[y] = indexXOfSmallestElement(paths, y, currentMin - 1, currentMin);
            } else {
                result[y] = indexXOfSmallestElement(paths, y, currentMin - 1, currentMin, currentMin + 1);
            }
        }

        return result;
    }

    private int indexYOfSmallestElement(double[][] paths, int x, int... elems) {
        double min = Double.POSITIVE_INFINITY;
        int minIndex = elems[0];

        for (int i : elems) {
            if (paths[x][i] < min) {
                min = paths[x][i];
                minIndex = i;
            }
        }

        return minIndex;
    }

    private int indexXOfSmallestElement(double[][] paths, int y, int... elems) {
        double min = Double.POSITIVE_INFINITY;
        int minIndex = elems[0];

        for (int i : elems) {
            if (paths[i][y] < min) {
                min = paths[i][y];
                minIndex = i;
            }
        }

        return minIndex;
    }

    public void removeHorizontalSeam(int[] seam) {
        if (seam == null) {
            throw new NullPointerException();
        }
        assureValidHorizontalSeam(seam);
        int[][] newColorsRGB = new int[this.width()][this.height() - 1];

        for (int x = 0; x < width(); x++) {
            int currentSeam = seam[x];
            int counter = 0;
            for (int y = 0; y < height(); y++) {
                if (y != currentSeam) {
                    newColorsRGB[x][counter] = colorsRGB[x][y];
                    counter++;
                }
            }
        }

        this.colorsRGB = newColorsRGB;
        this.height -= 1;
    }

    public void removeVerticalSeam(int[] seam) {
        if (seam == null) {
            throw new NullPointerException();
        }
        assureValidVerticalSeam(seam);
        int[][] newColorsRGB = new int[this.width() - 1][this.height()];

        for (int y = 0; y < height(); y++) {
            int currentSeam = seam[y];
            int counter = 0;
            for (int x = 0; x < width(); x++) {
                if (x != currentSeam) {
                    newColorsRGB[counter][y] = colorsRGB[x][y];
                    counter++;
                }
            }
        }
        this.colorsRGB = newColorsRGB;
        this.width -= 1;
    }

    public void addVerticalSeam(int[] seam) {
        if (seam == null) {
            throw new NullPointerException();
        }
        assureValidVerticalSeam(seam);
        int[][] newColorsRGB = new int[this.width() + 1][this.height()];

        for (int y = 0; y < height(); y++) {
            int currentSeam = seam[y];
            int counter = 0;
            for (int x = 0; x < width(); x++) {
                if (x != currentSeam) {
                    newColorsRGB[counter][y] = colorsRGB[x][y];
                    counter++;
                } else {
                    int newR = (getRed(colorsRGB[x][y]) + getRed(colorsRGB[x-1][y])) / 2;
                    int newG = (getGreen(colorsRGB[x][y]) + getGreen(colorsRGB[x-1][y])) / 2;
                    int newB = (getBlue(colorsRGB[x][y]) + getBlue(colorsRGB[x-1][y])) / 2;
                    newColorsRGB[counter][y] = rgbFromInt(newR, newG, newB);
                    counter++;
                    newColorsRGB[counter][y] = colorsRGB[x][y];
                    counter++;
                }
            }
        }
        this.colorsRGB = newColorsRGB;
        this.width += 1;
    }

    public void addHorizontalSeam(int[] seam) {

    }

    private void assureValidVerticalSeam(int[] seam) {
        if (seam.length != height()) {
            throw new IllegalArgumentException();
        }
        for (int i = 0; i < seam.length - 1; i++) {
            if (seam[i] < 0 || seam[i] > width() - 1 || Math.abs(seam[i] - seam[i + 1]) > 1) {
                throw new IllegalArgumentException();
            }
        }
        if (seam[seam.length - 1] < 0 || seam[seam.length - 1] > width() - 1) {
            throw new IllegalArgumentException();
        }
    }

    private void assureValidHorizontalSeam(int[] seam) {
        if (seam.length != width()) {
            throw new IllegalArgumentException();
        }
        for (int i = 0; i < seam.length - 1; i++) {
            if (seam[i] < 0 || seam[i] > height() - 1 || Math.abs(seam[i] - seam[i + 1]) > 1) {
                throw new IllegalArgumentException();
            }
        }
        if (seam[seam.length - 1] < 0 || seam[seam.length - 1] > height() - 1) {
            throw new IllegalArgumentException();
        }
    }

    private double smallestOfThree(double a, double b, double c) {
        return Math.min(a, Math.min(b, c));
    }

    private int rgbFromInt(int red, int green, int blue) {
        return  ((red & 0xFF) << 16) | ((green & 0xFF) << 8)  | ((blue & 0xFF));
    }

    private int getRed(int rgb) {
        return (rgb >> 16) & 0xFF;
    }

    private int getGreen(int rgb) {
        return (rgb >> 8) & 0xFF;
    }

    private int getBlue(int rgb) {
        return rgb & 0xFF;
    }

    public static void main(String[] args) {
        String filename = "ship.jpg";
        final Picture p = new Picture(filename);

        SeamCarver s = new SeamCarver(p);
        StopWatch clock = new StopWatch();
        for (int i = 0; i < 200; i++) {
            s.removeVerticalSeam(s.findVerticalSeam());
            s.removeHorizontalSeam(s.findHorizontalSeam());
            //s.addVerticalSeam(s.findVerticalSeam());
        }
        System.out.println(clock.elapsedTime());
        s.picture().save("new.png");
    }
}
