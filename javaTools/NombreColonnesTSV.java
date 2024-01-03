package javaTools;

import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;

public class NombreColonnesTSV {
    private int nombreColonnes; // Ajout d'un champ pour stocker le nombre de colonnes

    public NombreColonnesTSV(String cheminFichier) {
        calculerNombreColonnes(cheminFichier);
    }

    public int getNombreColonnes() {
        return nombreColonnes;
    }

    private void calculerNombreColonnes(String cheminFichier) {
        try (BufferedReader br = new BufferedReader(new FileReader(cheminFichier))) {
            String premiereLigne = br.readLine();

            // Vérifier si la première ligne n'est pas vide
            if (premiereLigne != null) {
                // Compter le nombre de colonnes en utilisant la tabulation comme séparateur
                nombreColonnes = compteColonnes(premiereLigne, '\t');
            } else {
                System.out.println("Le fichier TSV est vide.");
            }

        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    private int compteColonnes(String ligne, char separateur) {
        int nombreColonnes = 1; // Initialiser à 1 car il y a au moins une colonne

        // Parcourir la ligne pour compter le nombre de colonnes
        for (int i = 0; i < ligne.length(); i++) {
            if (ligne.charAt(i) == separateur) {
                nombreColonnes++;
            }
        }

        return nombreColonnes;
    }
}
