import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class LogLevels {

    public static String message(String logLine) throws IllegalArgumentException {
        Pattern pattern = Pattern.compile("^\\[(?:INFO|WARNING|ERROR)\\]: (?<message>.+)");
        Matcher matcher = pattern.matcher(logLine);
        if (matcher.find()) {
            return matcher.group("message").trim();
        } else {
            throw new IllegalArgumentException("Bad logLine");
        }
    }

    public static String logLevel(String logLine) throws IllegalArgumentException {
        Pattern pattern = Pattern.compile("^\\[(?<level>(?:INFO|WARNING|ERROR))\\]: .+");
        Matcher matcher = pattern.matcher(logLine);
        if (matcher.find()) {
            return matcher.group("level").toLowerCase();
        } else {
            throw new IllegalArgumentException("Bad logLine");
        }
    }

    public static String reformat(String logLine)  throws IllegalArgumentException {
        String message = LogLevels.message(logLine);
        String logLevel = LogLevels.logLevel(logLine);
        return String.format("%s (%s)", message, logLevel);
    }

}
