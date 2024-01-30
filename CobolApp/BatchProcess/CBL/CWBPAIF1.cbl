       IDENTIFICATION DIVISION.
      *-----------------------------------------------------------------
      * 
      * This program is responsible for performing batch processing in 
      * the AIFlix application.
      * It contains the main logic for processing data in batch mode.
      *
      * 
      *-----------------------------------------------------------------
      * Program Name : AIFlix-Batch-Process

       PROGRAM-ID. CWBPAIF1.
       
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

      *-----------------------------------------------------------------  
      * File Control

           SELECT CUSTOMER-FILE    ASSIGN TO CUSTFILE
               FILE STATUS IS STATUS-CUSTOMER-FILE.

           SELECT WATCH-FILE       ASSIGN TO WATCFILE
               FILE STATUS IS STATUS-WATCH-FILE.

           SELECT MERGED-FILE      ASSIGN TO MERGFILE
               FILE STATUS IS STATUS-MERGED-FILE.
               
       DATA DIVISION.

      *-----------------------------------------------------------------
      * File Section 
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

      *-----------------------------------------------------------------
      * Working-Storage Section

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
        05 FILLER                     PIC X(380).

      * Watch-File
       01 WS-Watch-File.
        05 accountNumber              PIC X(10).
        05 contentId                  PIC X(10).
        05 watchPercent               PIC X(5).
        05 startTime                  PIC X(10).
        05 mediaType                  PIC X(5).
        05 FILLER                     PIC X(160).

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
        05 FILLER                     PIC X(380).
        05 contentId                  PIC X(10).
        05 watchPercent               PIC X(5).
        05 startTime                  PIC X(10).
        05 mediaType                  PIC X(5).
        05 FILLER                     PIC X(170).
           
      * Reporting 
       01 Reporting-Line              PIC X(80) Value SPACES.
      
      * Status
        01 FILE-STATUS.
           05 STATUS-CUSTOMER-FILE           PIC XX.  
           05 STATUS-WATCH-FILE              PIC XX.  
           05 STATUS-MERGED-FILE             PIC XX.  


      * End-Of-Files
       01 End-Of-File-C                      PIC X VALUE 'N'.
       01 End-Of-File-W                      PIC X VALUE 'N'.

      * Ecran 
       01 Reporting-Data                    PIC X(80).
       
       
       PROCEDURE DIVISION.
      *-----------------------------------------------------------------
      * Principal Process
      *-----------------------------------------------------------------
       
       1000-Principal-Process.

           PERFORM 9000-Initialization
           PERFORM 9100-Open-Files
           PERFORM 2000-Secondary-Process
           PERFORM 9900-Close-Files
           PERFORM 9999-Finalize.
      
      *-----------------------------------------------------------------
      * Secondary Process
      *-----------------------------------------------------------------

       2000-Secondary-Process.

      * First Read
           PERFORM 9210-Process-Watch-Record 

      * Process Records
           PERFORM 2100-Process-Record
             UNTIL End-Of-File-C = 'Y' AND End-Of-File-W = 'Y'.

      *-----------------------------------------------------------------
      * Process Record
      *-----------------------------------------------------------------

       2100-Process-Record.
           
           PERFORM 9200-Process-Customer-Record
           PERFORM 2200-Process-Merged-Record
             UNTIL End-Of-File-C = 'Y' AND End-Of-File-W = 'Y'
             OR accountNumber of WS-Customer-File 
             < accountNumber of WS-Watch-File .
             
      *-----------------------------------------------------------------
      * Process Merged Record
      *-----------------------------------------------------------------

       2200-Process-Merged-Record.

           IF accountNumber of WS-Customer-File < 
               accountNumber of WS-Watch-File
              PERFORM 3100-Unmatched-Customer-Record
           END-IF.

           IF accountNumber of WS-Customer-File  = 
              accountNumber of WS-Watch-File 
             PERFORM 3000-Retrieve-Data
             PERFORM 9210-Process-Watch-Record
           END-IF.
           
           IF accountNumber of WS-Customer-File > 
              accountNumber of WS-Watch-File
             PERFORM 9210-Process-Watch-Record
           END-IF.
      *-----------------------------------------------------------------
      * Retrieve Data   
      *-----------------------------------------------------------------

       3000-Retrieve-Data.

      * Initialize
           MOVE SPACES TO WS-Merged-File 
      
      * Copy Data
           MOVE accountNumber of WS-Customer-File 
                TO accountNumber of WS-Merged-File
           MOVE firstName of WS-Customer-File
                TO firstName of WS-Merged-File
           MOVE lastName of WS-Customer-File
                TO lastName of WS-Merged-File
           MOVE birthDate of WS-Customer-File 
                TO birthDate of WS-Merged-File
           MOVE subscriptionDate of WS-Customer-File 
               TO subscriptionDate of WS-Merged-File
           MOVE emailAddress of WS-Customer-File 
               TO emailAddress of WS-Merged-File
           MOVE subscriptionProgram of WS-Customer-File 
               TO subscriptionProgram of WS-Merged-File
           MOVE contentId of WS-Watch-File 
               TO contentId of WS-Merged-File
           MOVE watchPercent of WS-Watch-File 
               TO watchPercent of WS-Merged-File
           MOVE startTime of WS-Watch-File 
               TO startTime of WS-Merged-File
           MOVE mediaType of WS-Watch-File 
               TO mediaType of WS-Merged-File

           PERFORM 9300-Write-Merged-Record.

      *-----------------------------------------------------------------
      * Unmatched Customer Record
      *-----------------------------------------------------------------

       3100-Unmatched-Customer-Record.    

           DISPLAY 'Unmatched Customer Record : ' 
                   accountNumber of WS-Customer-File.

      *-----------------------------------------------------------------
      * Initialization
      *-----------------------------------------------------------------

       9000-Initialization.
 
           MOVE SPACES TO Reporting-Line.     

      *-----------------------------------------------------------------
      * Open Files
      *-----------------------------------------------------------------

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

      *-----------------------------------------------------------------
      * Process Customer Record
      *-----------------------------------------------------------------

       9200-Process-Customer-Record.

           IF End-Of-File-C = 'N'
               READ CUSTOMER-FILE
                   AT END MOVE 'Y' TO End-Of-File-C
                   DISPLAY 'End of CUSTOMER-FILE'
                   MOVE HIGH-VALUES TO CUSTOMER-RECORD
               END-READ
               MOVE CUSTOMER-RECORD TO WS-Customer-File 
               IF STATUS-CUSTOMER-FILE NOT = '00' AND '10'
                   DISPLAY 'Error while reading Customer-File - FS : ' 
                              STATUS-CUSTOMER-FILE
                END-IF
           END-IF.
      
      *-----------------------------------------------------------------
      * Process Watch Record
      *-----------------------------------------------------------------

       9210-Process-Watch-Record.

           IF End-Of-File-W = 'N'
               READ WATCH-FILE
                   AT END MOVE 'Y' TO End-Of-File-W
                   DISPLAY 'End of WATCH-FILE'
                   MOVE HIGH-VALUES TO WATCH-RECORD 
               END-READ
               MOVE WATCH-RECORD TO WS-Watch-File 
               IF STATUS-WATCH-FILE  NOT = '00' AND '10'
                   DISPLAY 'Error while reading Watch-File - FS : ' 
                              STATUS-WATCH-FILE 
               END-IF
           END-IF.

      *-----------------------------------------------------------------
      * Write Merged Record
      *-----------------------------------------------------------------

        9300-Write-Merged-Record.

           IF STATUS-MERGED-FILE = '00'
               WRITE MERGED-RECORD FROM WS-Merged-File 
                   INVALID KEY
                   DISPLAY 'Error while writing to MERGED-FILE - FS : '
                          STATUS-MERGED-FILE 
               END-WRITE
           END-IF.

      *-----------------------------------------------------------------
      * Close Files
      *-----------------------------------------------------------------

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

      *-----------------------------------------------------------------
      * Finalize
      *-----------------------------------------------------------------

       9999-Finalize.

      * End of Program     
              STOP RUN.
