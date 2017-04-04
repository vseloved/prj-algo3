import javax.swing.*;

public class SeamCarver {
    public static void main(String[] args) {
        String filename = "HJocean.png";
        final Picture p = new Picture(filename);

        final JFrame frame = new JFrame();
        /*JMenuBar menuBar = new JMenuBar();
        JMenu menu = new JMenu("File");
        menuBar.add(menu);
        JMenuItem menuItem1 = new JMenuItem(" Save...   ");
        menuItem1.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {

                    FileDialog chooser = new FileDialog(frame,
                            "Use a .png or .jpg extension", FileDialog.SAVE);
                    chooser.setVisible(true);
                    if (chooser.getFile() != null) {
                        p.save(chooser.getDirectory() + File.separator + chooser.getFile());
                    }
                }
        });
        menuItem1.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_S,
                Toolkit.getDefaultToolkit().getMenuShortcutKeyMask()));
        menu.add(menuItem1);
        frame.setJMenuBar(menuBar);*/


        ImageIcon icon = new ImageIcon(p.getImage());

        frame.setContentPane(new JLabel(icon));
        // f.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        frame.setDefaultCloseOperation(JFrame.DISPOSE_ON_CLOSE);
        frame.setTitle(filename);
        frame.setResizable(true);
        frame.pack();
        frame.setVisible(true);


        frame.repaint();
    }
}
