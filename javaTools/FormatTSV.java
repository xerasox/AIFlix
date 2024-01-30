package javaTools;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.FileInputStream;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStreamReader;
import java.nio.charset.StandardCharsets;

public class FormatTSV {
   
    public static void main(String[] args) {
        String inputFile = "rawData\\titleFile.tsv"; // Remplacez par le chemin de votre fichier TSV
        String outputFile = "C:\\Users\\xeraso\\Desktop\\output.txt"; // Remplacez par le chemin de votre fichier de sortie
        String copyTable = "C:\\Users\\xeraso\\Desktop\\copy.txt"; // Remplacez par le chemin de votre fichier de sortie

        convertirTSVEnEspaces(inputFile, outputFile, copyTable);
    }

    private static void convertirTSVEnEspaces(String inputFile, String outputFile, String copyTable) {
        

                TailleMaxColonnesTSV tailleMaxColonnesTSV = new TailleMaxColonnesTSV(inputFile);
                int nombreEspaces = 0;
                int[] taillesMax = tailleMaxColonnesTSV.getTaillesMax();
                int[] taillesTransfMax = tailleMaxColonnesTSV.getTaillesMax();

                // Afficher les tailles maximales de chaque colonne
                for (int i = 0; i < taillesMax.length; i++) {
                        int multipleDeCinq = taillesMax[i];
                
                        // Tant que le reste de la division par 5 n'est pas égal à 0, incrémenter l'entier
                        while (multipleDeCinq % 5 != 0) {
                            multipleDeCinq++;
                        }
                        taillesTransfMax[i] = multipleDeCinq;
                    }

            String ligne;
            // Modifier la ligne de lecture du fichier en utilisant l'encodage UTF-8
            try (BufferedReader br = new BufferedReader(new InputStreamReader(new FileInputStream(inputFile), StandardCharsets.UTF_8));
                BufferedWriter bw = new BufferedWriter(new FileWriter(outputFile));
                BufferedWriter bt = new BufferedWriter(new FileWriter(copyTable))) {
                // Reste du code...
            int j = 1;
            while ((ligne = br.readLine()) != null) {

                // Diviser la ligne en colonnes en utilisant la tabulation comme séparateur
                    String[] colonnes = ligne.split("\t");

                if ( j != 1) {

                    // Construire une nouvelle ligne avec les colonnes séparées par le nombre d'espaces spécifié
                    StringBuilder nouvelleLigne = new StringBuilder();

                    for (int i = 0; i < colonnes.length; i++) {
                        String maChaine = colonnes[i];

                        if (colonnes[i].equals("\\N")) {
                            maChaine = "null";
                        }

                        // Mesurer la taille de la chaîne
                        int taille = maChaine.length();

                        if (taille > 100) {
                            taille = 100;
                            maChaine = maChaine.substring(0, 100);
                        }

                        nombreEspaces = taillesTransfMax[i] - taille;

                        nouvelleLigne.append(maChaine).append(" ".repeat(nombreEspaces));
                    }

                    // Écrire la nouvelle ligne dans le fichier de sortie
                    bw.write(nouvelleLigne.toString());
                    bw.newLine();
                } 
                else 
                {
                    
                    bt.write("01 TITRE-PRINCIPAL.");
                    bt.newLine();
                    int total = 0;
                    String maChaine ="";
                    int nbEspaces = 0;
                    int taille = 0;

                    for (int i = 0; i < taillesMax.length; i++) {

                    StringBuilder maxCopy = new StringBuilder();
                    
                    total = total + taillesMax[i];

                    maChaine = colonnes[i];

                    // Mesurer la taille de la chaîne
                    taille = maChaine.length();

                    //Nb d'espaces
                    nbEspaces = 20 - taille;
                    
                    maxCopy.append("  ");
                    maxCopy.append("05 ");
                    maxCopy.append(maChaine.toString());
                    maxCopy.append(" ".repeat(nbEspaces));
                    maxCopy.append("PIC X(");
                    maxCopy.append(taillesTransfMax[i]);
                    maxCopy.append(")");
                    bt.write(maxCopy.toString());
                    bt.newLine();
                    }

                    StringBuilder fillerCopy = new StringBuilder();
                    int tailleCopy = 500;
                    int tailleFiller = tailleCopy - total;
                    maChaine = "FILLER";

                    // Mesurer la taille de la chaîne
                    taille = maChaine.length();

                    //Nb d'espaces
                    nbEspaces = 20 - taille;

                    fillerCopy.append("  ");
                    fillerCopy.append("05 ");
                    fillerCopy.append(maChaine.toString());
                    fillerCopy.append(" ".repeat(nbEspaces));
                    fillerCopy.append("PIC X(");
                    fillerCopy.append(tailleFiller);
                    fillerCopy.append(")");
                    bt.write(fillerCopy.toString());
                    bt.newLine();

                }
                j++;   
            } 

            System.out.println("Conversion terminée avec succès.");

        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
