package javaTools;

import java.io.BufferedReader;
import java.io.FileInputStream;
import java.io.IOException; // Add missing import statement
import java.io.InputStreamReader;
import java.nio.charset.StandardCharsets;

public class TailleMaxColonnesTSV {
    private int[] taillesMax; // Tableau pour stocker la taille maximale de chaque colonne

    public TailleMaxColonnesTSV(String cheminFichier) {
        calculerTaillesMax(cheminFichier);
    }

    public int[] getTaillesMax() {
        return taillesMax.clone(); // Utiliser clone() pour éviter la modification externe du tableau
    }

    private void calculerTaillesMax(String cheminFichier) {
        try (BufferedReader br = new BufferedReader(new InputStreamReader(new FileInputStream(cheminFichier), StandardCharsets.UTF_8))) {
            String ligne;

            // Initialiser les tailles maximales à zéro
            taillesMax = null;
            boolean premiereOccurrence = true;

            // Lire le fichier ligne par ligne
            while ((ligne = br.readLine()) != null) {
                
                if (premiereOccurrence) {
                    premiereOccurrence = false;
                    continue; // Ignorer la première occurrence
                }

                // Diviser la ligne en colonnes en utilisant la tabulation comme séparateur
                String[] colonnes = ligne.split("\t");

                // Mettre à jour les tailles maximales pour chaque colonne
                if (taillesMax == null) {
                    // Si c'est la première ligne, initialiser le tableau des tailles maximales
                    taillesMax = new int[colonnes.length];
                    for (int i = 0; i < colonnes.length; i++) {
                        taillesMax[i] = 0;
                    }
                }

                for (int i = 0; i < colonnes.length; i++) {
                    int tailleColonne = colonnes[i].length();
                    if (tailleColonne > taillesMax[i]) {
                        taillesMax[i] = tailleColonne;
                    }
                    if (taillesMax[i] > 100) {
                        taillesMax[i] = 100;
                    }
                }
            }
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
