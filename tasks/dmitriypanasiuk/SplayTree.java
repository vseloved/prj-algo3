package dmitriypanasiuk;

import java.util.ArrayDeque;
import java.util.Queue;

public class SplayTree {
    private Node root;
    private static final String NEWLINE = System.getProperty("line.separator");
    private final Node EMPTY_NODE = new Node(0, "");

    private class Node {
        private int key;
        private String value;
        private Node left, right;

        public Node(int key, String value) {
            this.key = key;
            this.value = value;
        }
    }

    public void insert(int key, String value) {
        Node newNode = new Node(key, value);
        if (root == null) {
            root = newNode;
            return;
        }
        insert(root, newNode);
    }

    private void insert(Node root, Node node) {
        if (root.key > node.key) {
            if (root.left == null) root.left = node;
            else insert(root.left, node);
        }
        if (root.key < node.key) {
            if (root.right == null) root.right = node;
            else insert(root.right, node);
        }
    }

    private Node Zig(Node root) {
        Node right = root.left.right;
        root.left.right = root;
        Node newRoot = root.left;
        root.left = right;
        return newRoot;
    }

    private Node Zag(Node root) {
        Node left = root.right.left;
        root.right.left = root;
        Node newRoot = root.right;
        root.right = left;
        return newRoot;
    }

    @Override
    public String toString() {
        Queue<Node> queue = new ArrayDeque<>();
        StringBuilder sb = new StringBuilder();
        queue.add(root);
        queue.add(EMPTY_NODE);
        while (!queue.isEmpty()) {
            Node n = queue.poll();
            if (n == EMPTY_NODE) {
                sb.append(NEWLINE);
                continue;
            }
            sb.append(n.value + " ");
            if (n.left != null) queue.add(n.left);
            if (n.right != null) queue.add(n.right);
            if (queue.peek() == EMPTY_NODE) {
                queue.add(EMPTY_NODE);
            }
        }
        return sb.toString();
    }

    private static void inorderTraversal(Node root) {
        if (root.left!= null) inorderTraversal(root.left);
        System.out.print(root.value + " ");
        if (root.right!= null) inorderTraversal(root.right);
    }

    public static void main(String[] args) {
        SplayTree tree = new SplayTree();
        tree.insert(5, "5");
        tree.insert(7, "7");
        tree.insert(2, "2");
        tree.insert(3, "3");
        tree.insert(1, "1");
        System.out.println(tree);
        tree.root = tree.Zig(tree.root);
        System.out.println(tree);
        tree.root = tree.Zag(tree.root);
        System.out.println(tree);
    }
}
