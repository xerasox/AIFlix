       IDENTIFICATION DIVISION.
      *------------------------------------------------------------------------
      * Programme COBOL de traitement de fichiers QSAM
      *------------------------------------------------------------------------
       PROGRAM-ID. CWBPAIF1.
       
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
      * Déclaration des fichiers
           SELECT CUSTOMER-FILE    ASSIGN TO CUSTFILE
               FILE STATUS IS STATUS-CUSTOMER-FILE.
           SELECT WATCH-FILE       ASSIGN TO WATCFILE
               FILE STATUS IS STATUS-WATCH-FILE.
           SELECT MERGED-FILE      ASSIGN TO MERGFILE
               FILE STATUS IS STATUS-MERGED-FILE.
       DATA DIVISION.
       FILE SECTION.
       FD  CUSTOMER-FILE
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORDING MODE IS F.
       01  CUSTOMER-RECORD            PIC X(500).
       FD  WATCH-FILE
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORDING MODE IS F.
       01  WATCH-RECORD               PIC X(200).
       FD  MERGED-FILE
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORDING MODE IS F.
       01  MERGED-RECORD              PIC X(700).
       
       WORKING-STORAGE SECTION.
 
      * Customer-File
       01 WS-Customer-File.
        05 accountNumber              PIC X(10).
        05 firstName                  PIC X(15).
        05 lastName                   PIC X(15).
        05 gender                     PIC X(10).
        05 birthDate                  PIC X(10).
        05 subscriptionDate           PIC X(10).
        05 emailAddress               PIC X(40).
        05 subscriptionProgram        PIC X(10).
        05 FILLER                     PIC X(393).
 
      * Watch-File
       01 WS-Watch-File.
        05 accountNumber              PIC X(10).
        05 contentId                  PIC X(10).
        05 watchPercent               PIC X(5).
        05 startTime                  PIC X(10).
        05 mediaType                  PIC X(5).
        05 FILLER                     PIC X(166).
 
      * Merged-File
       01 WS-Merged-File.
        05 accountNumber              PIC X(10).
        05 firstName                  PIC X(15).
        05 lastName                   PIC X(15).
        05 gender                     PIC X(10).
        05 birthDate                  PIC X(10).
        05 subscriptionDate           PIC X(10).
        05 emailAddress               PIC X(40).
        05 subscriptionProgram        PIC X(10).
        05 FILLER                     PIC X(393).
        05 contentId                  PIC X(10).
        05 watchPercent               PIC X(5).
        05 startTime                  PIC X(10).
        05 mediaType                  PIC X(5).
        05 FILLER                     PIC X(176).
           
      * 
       01 Reporting-Line              PIC X(80) Value SPACES.
       
      * Status
        01 FILE-STATUS.
           05 STATUS-CUSTOMER-FILE           PIC XX.  
           05 STATUS-WATCH-FILE              PIC XX.  
           05 STATUS-MERGED-FILE             PIC XX.  
 
 
      * Variables de travail
       01 End-Of-File-C                      PIC X VALUE 'N'.
       01 End-Of-File-W                      PIC X VALUE 'N'.
 
      * Ecran 
       01 Reporting-Data                    PIC X(80).
       
       
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
           
              PERFORM 2100-Process-Record
                UNTIL End-Of-File-C = 'Y' AND End-Of-File-W = 'Y'.
 
       2100-Process-Record.
 
              IF accountNumber of WS-Customer-File  = 
                 accountNumber of WS-Watch-File 
                PERFORM 3000-Retrieve-Data
                PERFORM 9210-Process-Watch-Record
                PERFORM 9200-Process-Customer-Record
              END-IF.
              
              IF accountNumber of WS-Customer-File < 
                 accountNumber of WS-Watch-File
                PERFORM 3000-Retrieve-Data
                PERFORM 9210-Process-Watch-Record
              END-IF.
              
              IF accountNumber of WS-Customer-File > 
                 accountNumber of WS-Watch-File
                PERFORM 3000-Retrieve-Data
                PERFORM 9200-Process-Customer-Record
              END-IF.
       
       3000-Retrieve-Data.
              MOVE WS-Customer-File TO WS-Merged-File
              MOVE WS-Watch-File TO WS-Merged-File(500)
              PERFORM 9300-Write-Merged-Record
              .
 
 
       9000-Initialization.
           MOVE SPACES TO Reporting-Line.
       
       9100-Open-Files.
 
           OPEN INPUT CUSTOMER-FILE
           IF STATUS-CUSTOMER-FILE NOT = '00'
               DISPLAY 'Error while opening CUSTOMER-FILE - FS : ' 
                          STATUS-CUSTOMER-FILE
           END-IF
           OPEN INPUT WATCH-FILE
           IF STATUS-WATCH-FILE NOT = '00'
               DISPLAY 'Error while opening WATCH-FILE - FS : ' 
                          STATUS-WATCH-FILE
           END-IF
           OPEN OUTPUT MERGED-FILE
           IF STATUS-MERGED-FILE NOT = '00'
               DISPLAY 'Error while opening MERGED-FILE - FS : ' 
                          STATUS-MERGED-FILE
           END-IF.
 
       9200-Process-Customer-Record.
           READ CUSTOMER-FILE
               AT END MOVE 'Y' TO End-Of-File-C
               DISPLAY 'End of CUSTOMER-FILE'
           END-READ
           IF STATUS-CUSTOMER-FILE NOT = '00'
               MOVE CUSTOMER-RECORD TO WS-Customer-File 
               DISPLAY 'Error while reading Customer-File - FS : ' 
                          STATUS-CUSTOMER-FILE
           END-IF.
 
       9210-Process-Watch-Record.
           READ WATCH-FILE
               AT END MOVE 'Y' TO End-Of-File-W
           END-READ
           IF STATUS-WATCH-FILE  NOT = '00'
               MOVE WATCH-RECORD TO WS-Watch-File 
               DISPLAY 'Error while reading Watch-File - FS : ' 
                          STATUS-WATCH-FILE 
           END-IF.
 
        9300-Write-Merged-Record.
           WRITE MERGED-FILE FROM WS-Merged-File 
               INVALID KEY
                   DISPLAY 'Error while writing to MERGED-FILE - FS : '
                           STATUS-MERGED-FILE 
           END-WRITE.
           
       9900-Close-Files.
           CLOSE CUSTOMER-FILE
           IF STATUS-CUSTOMER-FILE NOT = '00'
               DISPLAY 'Error while closing CUSTOMER-FILE - FS : ' 
                          STATUS-CUSTOMER-FILE
           END-IF
           CLOSE WATCH-FILE
           IF STATUS-WATCH-FILE NOT = '00'
               DISPLAY 'Error while closing WATCH-FILE - FS : ' 
                          STATUS-WATCH-FILE
           END-IF
           CLOSE MERGED-FILE
           IF STATUS-MERGED-FILE NOT = '00'
               DISPLAY 'Error while closing MERGED-FILE - FS : ' 
                          STATUS-MERGED-FILE
           END-IF.
 
       
       9999-Finalize.
           
 
           EXIT PROGRAM.
