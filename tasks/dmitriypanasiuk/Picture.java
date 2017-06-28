package dmitriypanasiuk;

import javax.imageio.ImageIO;
import java.awt.*;
import java.awt.image.BufferedImage;
import java.io.File;
import java.io.IOException;
import java.net.URL;

public class Picture {
    private BufferedImage image;
    private String filename;
    private boolean isOriginUpperLeft = true;
    private final int width, height;

    public Picture(String filename) {
        if (filename == null) throw new IllegalArgumentException("filename is null");

        this.filename = filename;
        try {
            File file = new File(filename);
            if (file.isFile()) {
                image = ImageIO.read(file);
            } else {
                URL url = getClass().getResource(filename);
                if (url == null) {
                    url = new URL(filename);
                }
                image = ImageIO.read(url);
            }

            if (image == null) {
                throw new IllegalArgumentException("could not read image file: " + filename);
            }

            width  = image.getWidth(null);
            height = image.getHeight(null);
        }
        catch (IOException ioe) {
            throw new IllegalArgumentException("could not open image file: " + filename, ioe);
        }
    }

    public Picture(int width, int height) {
        if (width  < 0) throw new IllegalArgumentException("width must be nonnegative");
        if (height < 0) throw new IllegalArgumentException("height must be nonnegative");
        this.width  = width;
        this.height = height;
        image = new BufferedImage(width, height, BufferedImage.TYPE_INT_RGB);
        filename = width + "-by-" + height;
    }

    public BufferedImage getImage() {
        return image;
    }

    public void save(String filename) {
        if (filename == null) throw new IllegalArgumentException("argument to save() is null");
        save(new File(filename));
    }

    public void save(File file) {
        if (file == null) throw new IllegalArgumentException("argument to save() is null");
        filename = file.getName();
        String suffix = filename.substring(filename.lastIndexOf('.') + 1);
        if ("jpg".equalsIgnoreCase(suffix) || "png".equalsIgnoreCase(suffix)) {
            try {
                ImageIO.write(image, suffix, file);
            }
            catch (IOException e) {
                e.printStackTrace();
            }
        }
        else {
            System.out.println("Error: filename must end in .jpg or .png");
        }
    }

    public int height() {
        return height;
    }

    public int width() {
        return width;
    }

    public Color get(int col, int row) {
        validateCol(col);
        validateRow(row);
        if (isOriginUpperLeft) return new Color(image.getRGB(col, row));
        else                   return new Color(image.getRGB(col, height - row - 1));
    }

    public void set(int col, int row, Color color) {
        validateCol(col);
        validateRow(row);
        if (color == null) throw new IllegalArgumentException("color argument is null");
        if (isOriginUpperLeft) image.setRGB(col, row, color.getRGB());
        else                   image.setRGB(col, height - row - 1, color.getRGB());
    }

    private void validateRow(int row) {
        if (row < 0 || row >= height())
            throw new IndexOutOfBoundsException("row must be between 0 and " + (height() - 1) + ": " + row);
    }

    private void validateCol(int col) {
        if (col < 0 || col >= width())
            throw new IndexOutOfBoundsException("col must be between 0 and " + (width() - 1) + ": " + col);
    }
}
