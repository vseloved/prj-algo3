package dmitriypanasiuk;

import java.lang.management.ManagementFactory;
import java.lang.management.ThreadMXBean;

public class StopWatchCPU {
    private static final double NANOSECONDS_PER_SECOND = 1000_000_000;

    private final ThreadMXBean threadTimer;
    private final long start;

    public StopWatchCPU() {
        threadTimer = ManagementFactory.getThreadMXBean();
        start = threadTimer.getCurrentThreadCpuTime();
    }

    public double elapsedTime() {
        long now = threadTimer.getCurrentThreadCpuTime();
        return (now - start) / NANOSECONDS_PER_SECOND;
    }
}
