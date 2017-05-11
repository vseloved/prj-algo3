///usr/bin/env true; javac main.java && java Main; exit $?;

import java.util.concurrent.atomic.AtomicLong;

import wfq.WFQueue;
import wfq.EmptyException;

class Main {
    static int numProducers = 10;
    static int numConsumers = 10;
    static int numThreads = numProducers + numConsumers;
    static int numIntPairs = 1_000_000;

    public static void main(String[] args) {
        WFQueue queue = new WFQueue(numThreads);

        for (int i = 0; i < numProducers; i++) {
            int tid = i;

            new Thread(() -> {
                System.out.println("Producer #" + tid + " started!");

                for (int j = 0; j < numIntPairs; j++) {
                    queue.enq(tid, j);
                    queue.enq(tid, -j);
                }
            }).start();
        }

        AtomicLong result = new AtomicLong();

        for (int i = numProducers; i < numProducers + numConsumers; i++) {
            int tid = i;

            new Thread(() -> {
                System.out.println("Consumer #" + tid + " started!");

                long count = 0;

                for (int j = 0; j < 2; j++) {
                    for (int k = 0; k < numIntPairs; k++) {
                        try {
                            count += queue.deq(tid);
                        } catch (EmptyException ex) {
                            k++;
                        }
                    }
                }

                System.out.println("Consumer #" + tid + " finished: result + " + count + " = " + result.addAndGet(count));
            }).start();
        }

        Runtime.getRuntime().addShutdownHook(new Thread(() -> {
            System.out.println("result = " + result.get());
        }));
    }
}