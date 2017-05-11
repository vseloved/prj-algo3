package wfq;

import java.util.concurrent.atomic.AtomicReference;
import java.util.concurrent.atomic.AtomicReferenceArray;

public class WFQueue {
    AtomicReference<Node> head, tail;
    AtomicReferenceArray<OpDesc> state;

    public WFQueue(int numThreads) {
        Node sentinel = new Node(-1, -1);

        head = new AtomicReference<Node>(sentinel);
        tail = new AtomicReference<Node>(sentinel);
        state = new AtomicReferenceArray<OpDesc>(numThreads);

        for (int i = 0; i < state.length(); i++) {
            state.set(i, new OpDesc(-1, false, true, null));
        }
    }

    void help(long phase) {
        for (int i = 0; i < state.length(); i++) {
            OpDesc desc = state.get(i);

            if (desc.pending && desc.phase <= phase) {
                if (desc.enqueue) {
                    help_enq(i, phase);
                } else {
                    help_deq(i, phase);
                }
            }
        }
    }

    long maxPhase() {
        long maxPhase = -1;
        for (int i = 0; i < state.length(); i++) {
            long phase = state.get(i).phase;
            if (phase > maxPhase) {
                maxPhase = phase;
            }
        }

        return maxPhase;
    }

    boolean isStillPending(int tid, long ph) {
        return state.get(tid).pending && state.get(tid).phase <= ph;
    }

    void help_enq(int tid, long phase) {
        while (isStillPending(tid, phase)) {
            Node last = tail.get();
            Node next = last.next.get();
            if (last == tail.get()) {
                if (next == null) { // enqueue can be applied
                    if (isStillPending(tid, phase)) {
                        if (last.next.compareAndSet(next, state.get(tid).node)) {
                            help_finish_enq();
                            return;
                        }
                    }
                } else {
                    // some enqueue is in progress
                    // help it first, then retry
                    help_finish_enq();
                }
            }
        }
    }

    void help_finish_enq() {
        Node last = tail.get();
        Node next = last.next.get();

        if (next != null) {
            int tid = next.enqTid; // read enqTid of the last element
            OpDesc curDesc = state.get(tid);

            if (last == tail.get() && state.get(tid).node == next) {
                OpDesc newDesc = new OpDesc(state.get(tid).phase, false, true, next);
                state.compareAndSet(tid, curDesc, newDesc);
                tail.compareAndSet(last, next);
            }
        }
    }

    public void enq(int tid, int value) {
        long phase = maxPhase() + 1;
        state.set(tid, new OpDesc(phase, true, true, new Node(value, tid)));
        help(phase);
        help_finish_enq();
    }

    public int deq(int tid) throws EmptyException {
        long phase = maxPhase() + 1;
        state.set(tid, new OpDesc(phase, true, false, null));
        help(phase);
        help_finish_deq();
        Node node = state.get(tid).node;
        if (node == null) {
            throw new EmptyException();
        }
        return node.next.get().value;
    }

    void help_deq(int tid, long phase) {
        while (isStillPending(tid, phase)) {
            Node first = head.get();
            Node last = tail.get();
            Node next = first.next.get();

            if (first == head.get()) {
                if (first == last) { // queue might be empty
                    if (next == null) { // queue is empty
                        OpDesc curDesc = state.get(tid);

                        if (last == tail.get() && isStillPending(tid, phase)) {
                            OpDesc newDesc = new OpDesc(state.get(tid).phase, false, false, null);
                            state.compareAndSet(tid, curDesc, newDesc);
                        }
                    } else {
                        // some enqueue is in progress
                        // help it first, then retry
                        help_finish_enq();
                    }
                } else { // queue is not empty
                    OpDesc curDesc = state.get(tid);
                    Node node = curDesc.node;

                    if (!isStillPending(tid, phase)) break;
                    if (first == head.get() && node != first) {
                        OpDesc newDesc = new OpDesc(state.get(tid).phase, true, false, first);

                        if (!state.compareAndSet(tid, curDesc, newDesc)) {
                            continue;
                        }
                    }
                    first.deqTid.compareAndSet(-1, tid);
                    help_finish_deq();
                }
            }
        }
    }

    void help_finish_deq() {
        Node first = head.get();
        Node next = first.next.get();

        int tid = first.deqTid.get(); // read deqTid of the first element
        if (tid != -1) {
            OpDesc curDesc = state.get(tid);

            if (first == head.get() && next != null) {
                OpDesc newDesc = new OpDesc(state.get(tid).phase, false, false, state.get(tid).node);
                state.compareAndSet(tid, curDesc, newDesc);
                head.compareAndSet(first, next);
            }
        }
    }
}