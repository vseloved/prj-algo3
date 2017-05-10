class Node:
    def __init__(self, key, left=None, right=None):
        self.key = key
        self.left = left
        self.right = right


class SplayTree:
    def __init__(self):
        self.root = None

    def find(self, key):
        cur = self.root
        while cur:
            if cur.key == key:
                # do splay
                return cur
            else:
                cur = cur.left if cur.key > key else cur.right
        return None

    def rotate_right(self, node):
        t = node.left
        node.left = t.right
        t.right = node
        return t

    def rotate_left(self, node):
        t = node.right
        node.right = t.left
        t.left = node
        return t

    def splay(self, root, key):
        if root.key == key:
            return root

        if root.key > key:
            if root.left.key > key:
                root.left.left = self.splay(root.left.left, key)
                root = self.rotate_right(root)
            elif root.left.key < key:
                root.left.right = self.splay(root.left.right, key)
                root.left = self.rotate_left(root.left)
            root = self.rotate_right(root)
        else:
            if root.right.key > key:
                root.right.left = self.splay(root.right.left, key)
                root.right = self.rotate_right(root.right)
            elif root.right.key < key:
                root.right.right = self.splay(root.right.right, key)
                root = self.rotate_left(root)
            root = self.rotate_left(root)
        return root

    def insert(self, key):
        cur = self.root
        prev = None
        while cur:
            prev = cur
            cur = cur.left if cur.key > key else cur.right
        node = Node(key)
        if not prev:
            self.root = node
        elif prev.key > key:
            prev.left = node
        else:
            prev.right = node
        self.root = self.splay(self.root, key)

    def __str__(self):
        stack = [self.root]
        keys = []
        while stack:
            node = stack.pop()
            if node is None:
                continue
            keys.append(node.key)
            stack.append(node.right)
            stack.append(node.left)
        return " ".join(map(str, keys))


tree = SplayTree()
tree.insert(50)
tree.insert(20)
tree.insert(10)
tree.insert(25)
tree.insert(21)
# print tree
