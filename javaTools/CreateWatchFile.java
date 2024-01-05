package javaTools;

import java.io.BufferedWriter;
import java.io.FileWriter;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.List;
import java.util.Random;
import java.time.LocalTime;
import java.time.format.DateTimeFormatter;
// No changes needed for this code block.

public class CreateWatchFile {
    
    public static void main(String[] args) {
        String accountNumbersFile = "rawData\\custFile.tsv";
        String contentIdsFile = "rawData\\titleFile.tsv";
        String outputFile = "rawData\\watchFile.tsv";
        int numberOfLines = 5000000;

        List<String> accountNumbers = readColumnFromTSV(accountNumbersFile, 0);
        List<String> contentIds = readColumnFromTSV(contentIdsFile, 0);

        try (BufferedWriter writer = new BufferedWriter(new FileWriter(outputFile))) {

            String header = "accountNumber\tcontentId\twatchPercent\tstartTime\tmediaType";
            writer.write(header);
            writer.newLine();

            Random random = new Random();
            for (int i = 0; i < numberOfLines; i++) {
                String accountNumber = getRandomElement(accountNumbers, random);
                String contentId = getRandomElement(contentIds, random);
                int watchPercent = random.nextInt(100) + 1; // Generate a random number between 1 and 100
                int startTimeSeconds = random.nextInt(86399) + 1; // Generate a random number between 1 and 3600
                String mediaType = getRandomMediaType(random);

                LocalTime startTime = LocalTime.ofSecondOfDay(startTimeSeconds);
                String formattedStartTime = startTime.format(DateTimeFormatter.ofPattern("HH:mm:ss"));

                writer.write(accountNumber + "\t" + contentId + "\t" + watchPercent + "\t" + formattedStartTime + "\t" + mediaType);
                writer.newLine();
            }
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    private static String getRandomMediaType(Random random) {
        String[] mediaTypes = {"TV", "Web", "App"};
        int index = random.nextInt(mediaTypes.length);
        return mediaTypes[index];
    }

    private static List<String> readColumnFromTSV(String filePath, int columnIndex) {
        List<String> columnData = new ArrayList<>();
        try {
            List<String> lines = Files.readAllLines(Paths.get(filePath));
            for (String line : lines) {
                String[] columns = line.split("\t");
                if (columns.length > columnIndex) {
                    columnData.add(columns[columnIndex]);
                }
            }
        } catch (IOException e) {
            e.printStackTrace();
        }
        return columnData;
    }

    private static <T> T getRandomElement(List<T> list, Random random) {
        int index = random.nextInt(list.size());
        return list.get(index);
    }
}

