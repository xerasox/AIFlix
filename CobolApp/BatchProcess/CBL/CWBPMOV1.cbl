       IDENTIFICATION DIVISION.
      *------------------------------------------------------------------------
      * Programme COBOL de traitement de fichiers QSAM
      *------------------------------------------------------------------------
       PROGRAM-ID. CWBPMOV1.
       
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
      * Déclaration des fichiers
           SELECT TITLE-FILE    ASSIGN TO TITFILE.
           SELECT REPORT-FILE   ASSIGN TO RPTFILE.
       DATA DIVISION.
       FILE SECTION.
       FD  TITLE-FILE
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORDING MODE IS F.
       01  EMPLOYEE-RECORD            PIC X(1000).
       FD  REPORT-FILE
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORDING MODE IS F.
       01  REPORT-RECORD              PIC X(80).
      
       WORKING-STORAGE SECTION.
       
      * Title
       01 Title-Work-Area.
           05 TCONST            PIC X(10).
           05 TITLE-TYPE        PIC X(12).
           05 PRIMARY-TITLE     PIC X(242).
           05 ORIGINAL-TITLE    PIC X(242).
           05 IS-ADULT          PIC X(8).
           05 START-YEAR        PIC X(10).
           05 END-YEAR          PIC X(8).
           05 RUNTIME-MINUTES   PIC X(14).
           05 GENRES            PIC X(32).
           05 FILLER            PIC X(422). 
           
      * 
       01 Reporting-Line PIC X(80) Value SPACES.
      
      * Variables de travail
       01 End-Of-File           PIC X VALUE 'N'.
       01 Reporting-Data        PIC X(80).
       
      
      * Val Num
       01 Num-Start-Year        PIC 9(10) COMP-3.
       01 Num-End-Year          PIC 9(10) COMP-3.
       01 Num-Runtime-Minutes   PIC 9(10) COMP-3.

      
      * Min et Max
       01 Min-Start-Year        PIC 9(10) COMP-3 VALUE 9999999999.
       01 Min-End-Year          PIC 9(10) COMP-3 VALUE 9999999999.
       01 Min-Runtime-Minutes   PIC 9(10) COMP-3 VALUE 9999999999.
       01 Max-Start-Year        PIC 9(10) COMP-3 VALUE 0.
       01 Max-End-Year          PIC 9(10) COMP-3 VALUE 0.
       01 Max-Runtime-Minutes   PIC 9(14) COMP-3 VALUE 0.
       
      * Comptage décennies
       01 Year                PIC X(4).
       01 Decade              PIC X(3).
       01 CompteDecennies.
           05 Compte1880s       PIC 9(4) VALUE 0.
           05 Compte1890s       PIC 9(4) VALUE 0.
           05 Compte1900s       PIC 9(4) VALUE 0.
           05 Compte1910s       PIC 9(4) VALUE 0.
           05 Compte1920s       PIC 9(4) VALUE 0.
           05 Compte1930s       PIC 9(4) VALUE 0.
           05 Compte1940s       PIC 9(4) VALUE 0.
           05 Compte1950s       PIC 9(4) VALUE 0.
           05 Compte1960s       PIC 9(4) VALUE 0.
           05 Compte1970s       PIC 9(4) VALUE 0.
           05 Compte1980s       PIC 9(4) VALUE 0.
           05 Compte1990s       PIC 9(4) VALUE 0.
           05 Compte2000s       PIC 9(4) VALUE 0.
           05 Compte2010s       PIC 9(4) VALUE 0.
           
       01 Screen-Data.
           05 Screen-Header.
              10 FILLER           PIC X(15) VALUE 'QSAM Example'.
           05 FILLER               PIC X(65) VALUE SPACES.
       
       PROCEDURE DIVISION.
      *------------------------------------------------------------------------
      * Programme principal
      *------------------------------------------------------------------------
       
       1000-Principal-Process.
           PERFORM 9000-Initialization
           PERFORM 9100-Open-Files
           PERFORM 2000-Secondary-Process
           PERFORM 9900-Close-Files
           PERFORM 9999-Finalize
           STOP RUN.

       2000-Secondary-Process.
           PERFORM UNTIL End-Of-File = 'Y'
              READ TITLE-FILE into Title-Work-Area
                 AT END
                    MOVE 'Y' TO End-Of-File
                 NOT AT END
                    PERFORM 3000-Process-Record
                    PERFORM 3200-Decade-Count
              END-READ
           END-PERFORM.
       
       3000-Process-Record.
       
           Move 0 to Num-Start-Year
           Move 0 to Num-End-Year
           Move 0 to Num-Runtime-Minutes
       
           IF START-YEAR NOT EQUAL '\N        '
           Compute Num-Start-Year = FUNCTION NUMVAL(START-YEAR)
           END-IF.
           
           IF END-YEAR NOT EQUAL '\N      '
           Compute Num-End-Year = FUNCTION NUMVAL(END-YEAR)
           END-IF.
           
           IF RUNTIME-MINUTES NOT EQUAL '\N            '
           Compute Num-Runtime-Minutes 
           = FUNCTION NUMVAL(RUNTIME-MINUTES)
           END-IF.
       
      * Vérification et mise à jour du minimum START-YEAR
           IF Num-Start-Year NOT EQUAL 0
              AND Num-Start-Year < Min-Start-Year
              MOVE Num-Start-Year TO Min-Start-Year
           END-IF.
       
      * Vérification et mise à jour du minimum END-YEAR
           IF Num-End-Year NOT EQUAL 0
              AND Num-End-Year < Min-End-Year
              MOVE Num-End-Year TO Min-End-Year
           END-IF.
       
      * Vérification et mise à jour du minimum RUNTIME-MINUTES
           IF Num-Runtime-Minutes NOT EQUAL 0
              AND Num-Runtime-Minutes < Min-Runtime-Minutes
              MOVE Num-Runtime-Minutes TO Min-Runtime-Minutes
           END-IF.

      * Vérification et mise à jour du maximum START-YEAR
           IF Num-Start-Year NOT EQUAL 0
              AND Num-Start-Year > Max-Start-Year
              MOVE Num-Start-Year TO Max-Start-Year
           END-IF.
      
      * Vérification et mise à jour du maximum END-YEAR
           IF Num-End-Year NOT EQUAL 0
              AND Num-End-Year > Max-End-Year
              MOVE Num-End-Year TO Max-End-Year
           END-IF.
      
      * Vérification et mise à jour du maximum RUNTIME-MINUTES
           IF Num-Runtime-Minutes NOT EQUAL 0
              AND Num-Runtime-Minutes > Max-Runtime-Minutes
              MOVE Num-Runtime-Minutes TO Max-Runtime-Minutes
           END-IF.
       
      *    WRITE REPORT-RECORD from Reporting-Line
      *    DISPLAY 'Record processed: ' Reporting-Line.
      *    
       3200-Decade-Count.
       
           MOVE START-YEAR(1:3) to Decade
           
           Evaluate Decade 
           When '188' ADD 1 TO Compte1880s
           When '189' ADD 1 TO Compte1890s
           When '190' ADD 1 TO Compte1900s
           When '191' ADD 1 TO Compte1910s
           When '192' ADD 1 TO Compte1920s
           When '193' ADD 1 TO Compte1930s
           When '194' ADD 1 TO Compte1940s
           When '195' ADD 1 TO Compte1950s
           When '196' ADD 1 TO Compte1960s
           When '197' ADD 1 TO Compte1970s
           When '198' ADD 1 TO Compte1980s
           When '199' ADD 1 TO Compte1990s
           When '200' ADD 1 TO Compte2000s
           When '201' ADD 1 TO Compte2010s
           End-evaluate.
       
       9000-Initialization.
           DISPLAY Screen-Header.
           MOVE SPACES TO Reporting-Line.
       
       9100-Open-Files.
           OPEN INPUT TITLE-FILE
           OPEN OUTPUT REPORT-FILE.
       
       9900-Close-Files.
           CLOSE TITLE-FILE
           CLOSE REPORT-FILE.
       
       9999-Finalize.
           DISPLAY 'Program completed.'.
      * Affichage des résultats
           DISPLAY 'Minimum START-YEAR: ' Min-Start-Year
           DISPLAY 'Minimum END-YEAR: ' Min-End-Year
           DISPLAY 'Minimum RUNTIME-MINUTES: ' Min-Runtime-Minutes
           DISPLAY 'Maximum START-YEAR: ' Max-Start-Year
           DISPLAY 'Maximum END-YEAR: ' Max-End-Year
           DISPLAY 'Maximum RUNTIME-MINUTES: ' Max-Runtime-Minutes.
           
           DISPLAY "Compteur des années 1880s : " Compte1880s
           DISPLAY "Compteur des années 1890s : " Compte1890s
           DISPLAY "Compteur des années 1900s : " Compte1900s
           DISPLAY "Compteur des années 1910s : " Compte1910s
           DISPLAY "Compteur des années 1920s : " Compte1920s
           DISPLAY "Compteur des années 1930s : " Compte1930s
           DISPLAY "Compteur des années 1940s : " Compte1940s
           DISPLAY "Compteur des années 1950s : " Compte1950s
           DISPLAY "Compteur des années 1960s : " Compte1960s
           DISPLAY "Compteur des années 1970s : " Compte1970s
           DISPLAY "Compteur des années 1980s : " Compte1980s
           DISPLAY "Compteur des années 1990s : " Compte1990s
           DISPLAY "Compteur des années 2000s : " Compte2000s
           DISPLAY "Compteur des années 2010s : " Compte2010s

           
           EXIT PROGRAM.
