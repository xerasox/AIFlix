package javaTools;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;

public class FormatTSV {
   
    public static void main(String[] args) {
        String inputFile = "C:\\Users\\xeraso\\Downloads\\title.basics.tsv\\data3.tsv"; // Remplacez par le chemin de votre fichier TSV
        String outputFile = "C:\\Users\\xeraso\\Desktop\\output.txt"; // Remplacez par le chemin de votre fichier de sortie
        String copyTable = "C:\\Users\\xeraso\\Desktop\\copyTable.txt"; // Remplacez par le chemin de votre fichier de sortie

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
            
            try (BufferedReader br = new BufferedReader(new FileReader(inputFile));
             BufferedWriter bw = new BufferedWriter(new FileWriter(outputFile));
             BufferedWriter bt = new BufferedWriter(new FileWriter(copyTable))) {

            String ligne;
            int j = 1;
            while ((ligne = br.readLine()) != null) {

                // Diviser la ligne en colonnes en utilisant la tabulation comme séparateur
                    String[] colonnes = ligne.split("\t");

                if ( j != 1) {

                    // Construire une nouvelle ligne avec les colonnes séparées par le nombre d'espaces spécifié
                    StringBuilder nouvelleLigne = new StringBuilder();

                    for (int i = 0; i< colonnes.length; i++) {

                        String maChaine = colonnes[i];

                        // Mesurer la taille de la chaîne
                        int taille = maChaine.length();
                        
                        nombreEspaces=taillesTransfMax[i]-taille; 
                        
                        if (colonnes[i].equals("\\N")) {

                            maChaine = "null";

                        }          

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

                    for (int i = 0; i < taillesMax.length; i++) {

                    StringBuilder maxCopy = new StringBuilder();

                     String maChaine = colonnes[i];

                    // Mesurer la taille de la chaîne
                    int taille = maChaine.length();

                    //Nb d'espaces
                    int nbEspaces = 20 - taille;
                    
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
                    
                }
                j++;   
            } 

            System.out.println("Conversion terminée avec succès.");

        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
