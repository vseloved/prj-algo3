package dmitriypanasiuk;

import java.util.NoSuchElementException;

public class MinHeap {
    private int maxN;
    private int n;
    private int[] pq;
    private int[] qp;
    private Double[] keys;

    public MinHeap(int maxN) {
        this.maxN = maxN;
        n = 0;
        keys = new Double[maxN + 1];
        pq = new int[maxN + 1];
        qp = new int[maxN + 1];
        for (int i = 0; i <= maxN; i++) {
            qp[i] = -1;
        }
    }

    public boolean isEmpty() {
        return n == 0;
    }

    public boolean contains(int i) {
        return qp[i] != -1;
    }

    public void insert(int i, Double key) {
        if (contains(i)) throw new IllegalArgumentException("index is already in heap");
        n++;
        qp[i] = n;
        pq[n] = i;
        keys[i] = key;
        swim(n);
    }

    public int delMin() {
        if (n == 0) throw new NoSuchElementException("Heap is empty");
        int min = pq[1];
        exch(1, n--);
        sink(1);
        qp[min] = -1;
        keys[min] = null;
        pq[n + 1] = -1;        // not needed
        return min;
    }

    public void decreaseKey(int i, Double key) {
        if (!contains(i)) throw new NoSuchElementException("index is not in the heap");
        keys[i] = key;
        swim(qp[i]);
    }

    private boolean greater(int i, int j) {
        return keys[pq[i]] > keys[pq[j]];
    }

    private void exch(int i, int j) {
        int swap = pq[i];
        pq[i] = pq[j];
        pq[j] = swap;
        qp[pq[i]] = i;
        qp[pq[j]] = j;
    }

    private void swim(int k) {
        while (k > 1 && greater(k / 2, k)) {
            exch(k, k / 2);
            k = k / 2;
        }
    }

    private void sink(int k) {
        while (2 * k <= n) {
            int j = 2 * k;
            if (j < n && greater(j, j + 1)) j++;
            if (!greater(k, j)) break;
            exch(k, j);
            k = j;
        }
    }
}
