import java.lang.Math;

public class SalaryCalculator {
    private static final double SALARY_BASE = 1000, SALARY_MAX = 2000;

    public static double multiplierPerDaysSkipped(int daysSkipped) {
        return daysSkipped > 5 ? 0.85 : 1;
    }

    public static int multiplierPerProductsSold(int productsSold) {
        return productsSold > 20 ? 13 : 10;
    }

    public static double bonusForProductSold(int productsSold) {
        return productsSold * SalaryCalculator.multiplierPerProductsSold(productsSold);
    }

    public static double finalSalary(int daysSkipped, int productsSold) {
        double salary, bonus;
        salary = SALARY_BASE * SalaryCalculator.multiplierPerDaysSkipped(daysSkipped);
        bonus = bonusForProductSold(productsSold);
        return Math.min(SALARY_MAX, salary + bonus);
    } 
}
