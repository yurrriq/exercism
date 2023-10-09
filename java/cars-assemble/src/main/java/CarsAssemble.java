import java.lang.Math;

public class CarsAssemble {
    private static final int SPEED_OFF = 0;
    private static final int SPEED_MAXIMUM = 10;
    private static final int CARS_PER_HOUR = 221;

    private static double successRate(int speed) {
        if (speed < SPEED_OFF)
            throw new IllegalArgumentException("Negative speed");
        if (speed > SPEED_MAXIMUM)
            throw new IllegalArgumentException("Speed exceeds maximum");

        if (speed <= 4) return 1.0;
        if (speed <= 8) return 0.9;
        if (speed == 9) return 0.8;

        return 0.77;
    }

    public static double productionRatePerHour(int speed) {
        return speed * CARS_PER_HOUR * CarsAssemble.successRate(speed);
    }

    public static int workingItemsPerMinute(int speed) {
        return (int) Math.floor(CarsAssemble.productionRatePerHour(speed) / 60);
    }
}
