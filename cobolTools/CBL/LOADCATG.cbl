       IDENTIFICATION DIVISION.
      *-----------------------------------------------------------------
      * 
      * 
      * 
      *-----------------------------------------------------------------
      * Program Name : LoadCatg

       PROGRAM-ID. LOADCATG.
       
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

      *-----------------------------------------------------------------  
      * File Control

           SELECT TITLE-FILE     ASSIGN TO TITLFILE
               FILE STATUS IS STATUS-TITLE-FILE.

           SELECT ERROR-FILE     ASSIGN TO ERROFILE
               FILE STATUS IS STATUS-ERROR-FILE.

       DATA DIVISION.

      *-----------------------------------------------------------------
      * File Section 
       FILE SECTION.

       FD  TITLE-FILE
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORDING MODE IS F.
       01  TITLE-RECORD               PIC X(500).

       FD  ERROR-FILE
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORDING MODE IS F.
       01  ERROR-RECORD             PIC X(500).

      *-----------------------------------------------------------------
      * Working-Storage Section

       WORKING-STORAGE SECTION.
      
      * Title-File
       01 WS-Title-File.
        05 contentId                   PIC X(10).
        05 titleType                   PIC X(15).
        05 primaryTitle                PIC X(100).
        05 originalTitle               PIC X(100).
        05 isAdult                     PIC X(5).
        05 startYear                   PIC 9(4).
        05 FILLER                      PIC X(1).
        05 endYear                     PIC 9(4).
        05 FILLER                      PIC X(1).
        05 runtimeMinutes              PIC X(5).
        05 genres                      PIC X(35).
        05 FILLER                      PIC X(220).

      * Error-File
       01 WS-Error-File.
        05 contentId                   PIC X(10).
        05 titleType                   PIC X(15).
        05 primaryTitle                PIC X(100).
        05 originalTitle               PIC X(100).
        05 isAdult                     PIC X(5).
        05 startYear                   PIC 9(4).
        05 FILLER                      PIC X(1).
        05 endYear                     PIC 9(4).
        05 FILLER                      PIC X(1).
        05 runtimeMinutes              PIC X(5).
        05 genres                      PIC X(35).
        05 FILLER                      PIC X(220).

      * Status
       01 FILE-STATUS. 
        05 STATUS-TITLE-FILE             PIC XX.
        05 STATUS-ERROR-FILE             PIC XX.

      * End-Of-Files
       01 End-Of-File-T                  PIC X VALUE 'N'.
       
      *Working Variables

       01 WS-WORKING-VARIABLES.
        05 WS-GENRES                     PIC X(35).
        05 WS-GENRE1                     PIC X(15).
        05 WS-GENRE2                     PIC X(15).
        05 WS-GENRE3                     PIC X(15).
        05 WS-GENRE-COUNT                PIC 9(2).
        05 WS-RUNTIME-MINUTES            PIC X(5).
        05 WS-LENGTH                     PIC 9.
        05 WS-TEMP                       PIC 9.

      * SQLCA

           EXEC SQL INCLUDE SQLCA END-EXEC.

      ******************************************************************
      * DCLGEN TABLE(SALESSUP.AIF_CATALOG)                             *
      *        LIBRARY(MVSXYE.CATALOG.COBOL)                           *
      *        LANGUAGE(COBOL)                                         *
      *        QUOTE                                                   *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE AIF_CATALOG TABLE
           ( CONTENT_ID                     CHAR(10) NOT NULL,
             TITLE_TYPE                     VARCHAR(15),
             PRIMARY_TITLE                  VARCHAR(100),
             ORIGINAL_TITLE                 VARCHAR(100),
             IS_ADULT                       CHAR(1),
             START_YEAR                     DECIMAL(4, 0),
             END_YEAR                       DECIMAL(4, 0),
             RUNTIME_MINUTES                DECIMAL(5, 0),
             GENRE1                         VARCHAR(15),
             GENRE2                         VARCHAR(15),
             GENRE3                         VARCHAR(15)
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE SALESSUP.AIF_CATALOG               *
      ******************************************************************
       01  DCLAIF-CATALOG.
           10 CONTENT-ID           PIC X(10).
           10 TITLE-TYPE.
              49 TITLE-TYPE-LEN    PIC S9(4) USAGE COMP.
              49 TITLE-TYPE-TEXT   PIC X(15).
           10 PRIMARY-TITLE.
              49 PRIMARY-TITLE-LEN
                 PIC S9(4) USAGE COMP.
              49 PRIMARY-TITLE-TEXT
                 PIC X(100).
           10 ORIGINAL-TITLE.
              49 ORIGINAL-TITLE-LEN
                 PIC S9(4) USAGE COMP.
              49 ORIGINAL-TITLE-TEXT
                 PIC X(100).
           10 IS-ADULT             PIC X(1).
           10 START-YEAR           PIC S9(4)V USAGE COMP-3.
           10 END-YEAR             PIC S9(4)V USAGE COMP-3.
           10 RUNTIME-MINUTES      PIC S9(5)V USAGE COMP-3.
           10 GENRE1.
              49 GENRE1-LEN        PIC S9(4) USAGE COMP.
              49 GENRE1-TEXT       PIC X(15).
           10 GENRE2.
              49 GENRE2-LEN        PIC S9(4) USAGE COMP.
              49 GENRE2-TEXT       PIC X(15).
           10 GENRE3.
              49 GENRE3-LEN        PIC S9(4) USAGE COMP.
              49 GENRE3-TEXT       PIC X(15).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 11      *
      ******************************************************************

      * Null
         01 NULL-IND-SY            PIC S9(4) USAGE COMP.
         01 NULL-IND-EY            PIC S9(4) USAGE COMP.
         01 NULL-IND-RM            PIC S9(4) USAGE COMP.
         01 NULL-IND-GENRE1        PIC S9(4) USAGE COMP.

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


      * Process Records
           PERFORM 2100-Process-Record
             UNTIL End-Of-File-T = 'Y'.

      *-----------------------------------------------------------------
      * Process Record
      *-----------------------------------------------------------------

       2100-Process-Record.

           PERFORM 9200-Read-Title-Record
     
           IF contentId of WS-Title-File NOT = HIGH-VALUES
             MOVE SPACES TO WS-GENRE1 
             MOVE SPACES TO WS-GENRE2 
             MOVE SPACES TO WS-GENRE3 
             MOVE contentId of WS-Title-File TO CONTENT-ID 
             MOVE titleType of WS-Title-File TO TITLE-TYPE-TEXT 
             MOVE LENGTH OF TITLE-TYPE-TEXT TO TITLE-TYPE-LEN 
             MOVE primaryTitle of WS-Title-File TO PRIMARY-TITLE-TEXT
             MOVE LENGTH OF PRIMARY-TITLE-TEXT TO PRIMARY-TITLE-LEN
             MOVE originalTitle of WS-Title-File TO ORIGINAL-TITLE-TEXT 
             MOVE LENGTH OF ORIGINAL-TITLE-TEXT TO ORIGINAL-TITLE-LEN 
             MOVE isAdult of WS-Title-File TO IS-ADULT 

             IF startYear of WS-Title-File = 'null' 
                MOVE 0 TO START-YEAR
                MOVE -1 TO NULL-IND-SY 
             ELSE
                MOVE startYear of WS-Title-File TO START-YEAR 
                MOVE 0 TO NULL-IND-SY 
             END-IF
             
             IF endYear of WS-Title-File = 'null' 
                MOVE 0 TO END-YEAR
                MOVE -1 TO NULL-IND-EY 
             ELSE
                MOVE endYear of WS-Title-File TO END-YEAR 
                MOVE 0 TO NULL-IND-EY 
             END-IF

             IF runtimeMinutes of WS-Title-File NOT = 'null'
                MOVE 0 TO WS-LENGTH
                MOVE runtimeMinutes of WS-Title-File 
                TO WS-RUNTIME-MINUTES
                PERFORM VARYING WS-TEMP FROM 1 BY 1 
                   UNTIL WS-TEMP > LENGTH OF WS-RUNTIME-MINUTES
                   IF WS-RUNTIME-MINUTES (WS-TEMP:1) NOT = SPACE
                      ADD 1 TO WS-LENGTH
                   END-IF
                END-PERFORM
                MOVE WS-RUNTIME-MINUTES (1:WS-LENGTH)
                TO RUNTIME-MINUTES
                MOVE 0 TO NULL-IND-RM
             ELSE
                MOVE 0 TO RUNTIME-MINUTES
                MOVE -1 TO NULL-IND-RM
             END-IF

             MOVE genres of WS-Title-File TO WS-GENRES
            
             IF isAdult of WS-Title-File = '0' OR '1'
                PERFORM 3000-Split-Genres
                EXEC SQL INSERT INTO AIF_CATALOG
                            (
                               CONTENT_ID,
                               TITLE_TYPE, 
                               PRIMARY_TITLE, 
                               ORIGINAL_TITLE, 
                               IS_ADULT, 
                               START_YEAR, 
                               END_YEAR, 
                               RUNTIME_MINUTES,
                               GENRE1, 
                               GENRE2,    
                               GENRE3
                            )
                     VALUES (
                               :CONTENT-ID,
                               :TITLE-TYPE,
                               :PRIMARY-TITLE,
                               :ORIGINAL-TITLE,
                               :IS-ADULT,
                               :START-YEAR:NULL-IND-SY,
                               :END-YEAR:NULL-IND-EY,
                               :RUNTIME-MINUTES:NULL-IND-RM,
                               :GENRE1:NULL-IND-GENRE1,
                               :GENRE2,
                               :GENRE3
                            ) 
                END-EXEC 
                     IF SQLCODE NOT = 0  
                             MOVE TITLE-RECORD TO WS-Error-File
                             PERFORM 9300-Write-Error-Record
                     END-IF
               ELSE
                MOVE TITLE-RECORD TO WS-Error-File
                PERFORM 9300-Write-Error-Record
               END-IF
           END-IF.

      *-----------------------------------------------------------------
      * Split Genres
      *-----------------------------------------------------------------

       3000-Split-Genres.

           
           UNSTRING WS-GENRES DELIMITED BY ',' 
           INTO WS-GENRE1 WS-GENRE2 WS-GENRE3
           COUNT IN WS-GENRE-COUNT.

           IF WS-GENRE1 = 'null'
              MOVE SPACES TO GENRE1-TEXT
              MOVE -1 TO NULL-IND-GENRE1
           ELSE
              MOVE LENGTH OF WS-GENRE1 TO GENRE1-LEN
              MOVE WS-GENRE1(1:GENRE1-LEN ) TO GENRE1-TEXT
              MOVE 0 TO NULL-IND-GENRE1
           END-IF
           MOVE LENGTH OF WS-GENRE2 TO GENRE2-LEN
           MOVE WS-GENRE2(1:GENRE2-LEN ) TO GENRE2-TEXT
           MOVE LENGTH OF WS-GENRE3 TO GENRE3-LEN
           MOVE WS-GENRE3(1:GENRE3-LEN ) TO GENRE3-TEXT.

      *-----------------------------------------------------------------
      * Initialization
      *-----------------------------------------------------------------

       9000-Initialization.

           INITIALIZE WS-Title-File 
           INITIALIZE WS-Error-File.
 
      *-----------------------------------------------------------------
      * Open Files
      *-----------------------------------------------------------------

       9100-Open-Files.

           OPEN INPUT TITLE-FILE
           IF STATUS-TITLE-FILE NOT = '00'
               DISPLAY 'Error while opening TITLE-FILE - FS : ' 
                          STATUS-TITLE-FILE
           END-IF

           OPEN OUTPUT ERROR-FILE
           IF STATUS-ERROR-FILE NOT = '00'
               DISPLAY 'Error while opening ERROR-FILE - FS : ' 
                          STATUS-ERROR-FILE
           END-IF.
      
      *-----------------------------------------------------------------
      * Process Title Record
      *-----------------------------------------------------------------

       9200-Read-Title-Record.
           
           IF End-Of-File-T = 'N'
               READ TITLE-FILE
                   AT END MOVE 'Y' TO End-Of-File-T
                   DISPLAY 'End of TITLE-FILE'
                   MOVE HIGH-VALUES TO TITLE-RECORD 
               END-READ
               MOVE TITLE-RECORD TO WS-Title-File 
               IF STATUS-TITLE-FILE  NOT = '00' AND '10'
                   DISPLAY 'Error while reading Title-File - FS : ' 
                              STATUS-TITLE-FILE 
               END-IF
           END-IF.

      *-----------------------------------------------------------------
      * Write Costumer Statistics
      *-----------------------------------------------------------------

        9300-Write-Error-Record.
           
           IF STATUS-ERROR-FILE  = '00'
               WRITE ERROR-RECORD FROM WS-Error-File 
                   INVALID KEY
                   DISPLAY 'Error while writing to CSTSTAT-FILE - FS : '
                          STATUS-ERROR-FILE 
               END-WRITE
           END-IF.

      *-----------------------------------------------------------------
      * Close Files
      *-----------------------------------------------------------------

       9900-Close-Files.

           CLOSE TITLE-FILE
           IF STATUS-TITLE-FILE NOT = '00'
             DISPLAY 'Error while closing TITLE-FILE - FS : ' 
                           STATUS-TITLE-FILE
           END-IF.

           CLOSE ERROR-FILE
           IF STATUS-ERROR-FILE NOT = '00'
             DISPLAY 'Error while closing CSTSTAT-FILE - FS : ' 
                           STATUS-ERROR-FILE
           END-IF.

      *-----------------------------------------------------------------
      * Finalize
      *-----------------------------------------------------------------

       9999-Finalize.

      * End of Program     
            STOP RUN.
