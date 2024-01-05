package javaTools;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.Random;
import java.time.LocalDate;
import java.util.concurrent.ThreadLocalRandom;

public class CreateCustFile {
    public static void main(String[] args) {
        String[][] firstNames = readFirstNamesFromFile("C:\\Users\\xeraso\\git\\AIFlix\\rawData\\firstNames.csv");
        String[] lastNames = readLastNamesFromFile("C:\\Users\\xeraso\\git\\AIFlix\\rawData\\surnames.csv");

        try (BufferedWriter writer = new BufferedWriter(new FileWriter("rawData\\custFile.tsv"))) {

            String header = "accountNumber\tfirstName\tlastName\tgender\tsubscriptionDate\temailAddress\tsubscriptionProgram";
            writer.write(header);
            writer.newLine();

            Random random = new Random();
            for (int i = 0; i < 1000000; i++) {
                String accountNumber = String.format("%010d", i+1);
                String[] firstNameWithGender = firstNames[random.nextInt(firstNames.length)];
                String firstName = firstNameWithGender[0];
                String gender = firstNameWithGender[1];
                String lastName = lastNames[random.nextInt(lastNames.length)];
                String subscriptionDate = generateSubscriptionDate();
                String emailAddress = firstName.toLowerCase() + "." + lastName.toLowerCase() + "@domain.com";
                String subscriptionProgram = generateSubscriptionProgram();
                String line = accountNumber + "\t" + firstName + "\t" + lastName + "\t" + gender + "\t" +
                        subscriptionDate + "\t" + emailAddress + "\t" + subscriptionProgram;
                writer.write(line);
                writer.newLine();
            }
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    private static String[][] readFirstNamesFromFile(String filePath) {
        List<String[]> firstNamesList = new ArrayList<>();

        try (BufferedReader reader = new BufferedReader(new FileReader(filePath))) {
            String line;
            while ((line = reader.readLine()) != null) {
                String[] nameAndGender = line.split(",");
                firstNamesList.add(nameAndGender);
            }
        } catch (IOException e) {
            e.printStackTrace();
        }

        return firstNamesList.toArray(new String[0][0]);
    }

    private static String[] readLastNamesFromFile(String filePath) {
        List<String> lastNamesList = new ArrayList<>();

        try (BufferedReader reader = new BufferedReader(new FileReader(filePath))) {
            String line;
            while ((line = reader.readLine()) != null) {
                lastNamesList.add(line);
            }
        } catch (IOException e) {
            e.printStackTrace();
        }

        return lastNamesList.toArray(new String[0]);
    }

    private static String generateSubscriptionDate() {
        LocalDate startDate = LocalDate.of(2020, 1, 1);
        LocalDate endDate = LocalDate.now();
        long startEpochDay = startDate.toEpochDay();
        long endEpochDay = endDate.toEpochDay();
        long randomEpochDay = ThreadLocalRandom.current().nextLong(startEpochDay, endEpochDay + 1);
        LocalDate randomDate = LocalDate.ofEpochDay(randomEpochDay);
        return randomDate.toString();
    }

    private static String generateSubscriptionProgram() {
        String[] subscriptionPrograms = {"standard", "extended", "premium"};
        Random random = new Random();
        int index = random.nextInt(subscriptionPrograms.length);
        return subscriptionPrograms[index];
    }

}

