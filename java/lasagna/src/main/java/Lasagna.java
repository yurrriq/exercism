public class Lasagna {
    public static int expectedMinutesInOven() {
        return 40;
    }

    public int remainingMinutesInOven(int actualMinutesInOven) {
        return this.expectedMinutesInOven() - actualMinutesInOven;
    }

    public static int preparationTimeInMinutes(int numberOfLayers) {
        return 2 * numberOfLayers;
    }

    public int totalTimeInMinutes(int numberOfLayers, int actualMinutesInOven) {
        return this.preparationTimeInMinutes(numberOfLayers) + actualMinutesInOven;
    }
}
