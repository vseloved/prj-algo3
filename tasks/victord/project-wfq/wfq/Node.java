package wfq;

import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicReference;

public class Node {
    int value;
    int enqTid;

    AtomicReference<Node> next;
    AtomicInteger deqTid;

    Node(int value, int enqThreadId) {
        this.value = value;
        this.enqTid = enqThreadId;

        next = new AtomicReference<Node>(null);
        deqTid = new AtomicInteger(-1);
    }
}