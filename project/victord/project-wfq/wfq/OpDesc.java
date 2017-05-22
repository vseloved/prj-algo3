package wfq;

public class OpDesc {
    long phase;
    boolean pending;
    boolean enqueue;
    Node node;

    OpDesc(long phase, boolean pending, boolean enqueue, Node node) {
        this.phase = phase;
        this.pending = pending;
        this.enqueue = enqueue;
        this.node = node;
    }
}
