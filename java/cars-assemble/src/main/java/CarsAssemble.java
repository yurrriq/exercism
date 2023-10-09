import java.lang.Math;

public class CarsAssemble {
    private final static int SPEED_OFF = 0;
    private final static int SPEED_MAXIMUM = 0;
    private final static int CARS_PER_HOUR = 221;

    public static double productionRatePerHour(int speed) {
        double successRate;
        if (speed < 5) {
            successRate = 1.0;
        } else if (speed <= 8) {
            successRate = 0.9;
        } else if (speed == 9) {
            successRate = 0.8;
        } else {
            successRate = 0.77;
        }

        return (double)speed * (double)CARS_PER_HOUR * successRate;
    }

    public static int workingItemsPerMinute(int speed) {
        return (int) Math.floor(CarsAssemble.productionRatePerHour(speed) / 60.0);
    }
}
