import java.lang.Math;

public class SalaryCalculator {
    private static final double baseSalary = 1000, maxTotalCompensation = 2000;

    private static final int daysSkippedAllowance = 5;
    private static final double truancyPenalty = 0.15;

    private static final int productsSoldBonusThreshold = 20;
    private static final int topSellerMultiplier = 13;
    private static final int normalSellerMultiplier = 10;

    public static double multiplierPerDaysSkipped(int daysSkipped) {
        return 1 - (daysSkipped > daysSkippedAllowance ? truancyPenalty : 0);
    }

    public static int multiplierPerProductsSold(int productsSold) {
        return productsSold > productsSoldBonusThreshold ? topSellerMultiplier : normalSellerMultiplier;
    }

    public static double bonusForProductSold(int productsSold) {
        return productsSold * SalaryCalculator.multiplierPerProductsSold(productsSold);
    }

    public static double finalSalary(int daysSkipped, int productsSold) {
        double salary, bonus;
        salary = baseSalary * SalaryCalculator.multiplierPerDaysSkipped(daysSkipped);
        bonus = bonusForProductSold(productsSold);
        return Math.min(maxTotalCompensation, salary + bonus);
    }
}
