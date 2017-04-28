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

        Node(int key, String value) {
            this.key = key;
            this.value = value;
        }
    }

    public String get(int key) {
        this.root = splay(this.root, key);
        if (this.root.key == key) return this.root.value;
        return null;
    }

    public void remove(int key) {
        this.root = splay(this.root, key);
        if (this.root.key == key) {
            if (this.root.left == null) {
                this.root = this.root.right;
            } else {
                Node x = root.right;
                root = root.left;
                splay(root, key);
                root.right = x;
            }
        }
    }

    public void insert(int key, String value) {
        Node newNode = new Node(key, value);
        if (root == null) {
            root = newNode;
            return;
        }
        insert(root, newNode);
        this.root = splay(this.root, key);
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

    private Node splay(Node root, int key) {
        if (root == null) return null;
        if (root.key > key) {
            if (root.left == null) return root;
            if (root.left.key == key) {
                return Zig(root);
            } else if (root.left.key > key) {
                root.left.left = splay(root.left.left, key);
                root = Zig(root);

            } else {
                root.left.right = splay(root.left.right, key);
                if (root.left.right != null) {
                    root.left = Zag(root.left);
                }
            }
            if (root.left == null) return root;
            else return Zig(root);
        } else if (root.key < key){
            if (root.right == null) return root;
            if (root.right.key == key) {
                return Zag(root);
            } else if (root.right.key > key) {
                root.right.left = splay(root.right.left, key);
                if (root.right.left != null) {
                    root.right = Zig(root.right);
                }
            } else {
                root.right.right = splay(root.right.right, key);
                root = Zag(root);
            }
            if (root.right == null) return root;
            else return Zag(root);
        }
        else return root;
    }

    private Node Zig(Node root) {
        Node x = root.left;
        root.left = x.right;
        x.right = root;
        return x;
    }

    private Node Zag(Node root) {
        Node x = root.right;
        root.right = x.left;
        x.left = root;
        return x;
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
        tree.insert(0, "0");
        tree.insert(2, "2");
        tree.insert(1, "1");
        tree.insert(4, "4");
        tree.insert(3, "3");
        tree.insert(7, "7");
        tree.insert(5, "5");
    }
}
