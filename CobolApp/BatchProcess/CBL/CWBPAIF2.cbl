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

       PROGRAM-ID. CWBPAIF2.
       
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

      *-----------------------------------------------------------------  
      * File Control

           SELECT MERGED-FILE    ASSIGN TO MERGFILE
               FILE STATUS IS STATUS-MERGED-FILE.

           SELECT TITLE-FILE       ASSIGN TO TITLFILE
               FILE STATUS IS STATUS-TITLE-FILE.

           SELECT CSTSTAT-FILE      ASSIGN TO CSTSFILE
               FILE STATUS IS STATUS-CSTSTAT-FILE.

           SELECT TTLSTAT-FILE      ASSIGN TO TTLSFILE
               FILE STATUS IS STATUS-TTLSTAT-FILE.

           SELECT ALLSTAT-FILE      ASSIGN TO ALLSFILE
               FILE STATUS IS STATUS-ALLSTAT-FILE.

       DATA DIVISION.

      *-----------------------------------------------------------------
      * File Section 
       FILE SECTION.
       FD  MERGED-FILE
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORDING MODE IS F.
       01  MERGED-RECORD              PIC X(700).

       FD  TITLE-FILE
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORDING MODE IS F.
       01  TITLE-RECORD               PIC X(500).

       FD  CSTSTAT-FILE
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORDING MODE IS F.
       01  CSTSTAT-RECORD             PIC X(200).

       FD  TTLSTAT-FILE
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORDING MODE IS F.
       01  TTLSTAT-RECORD             PIC X(200).

       FD  ALLSTAT-FILE
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORDING MODE IS F.
       01  ALLSTAT-RECORD             PIC X(80).

      *-----------------------------------------------------------------
      * Working-Storage Section

       WORKING-STORAGE SECTION.

      * Merged-File
       01 WS-Merged-File.
        05 accountNumber               PIC X(10).
        05 firstName                   PIC X(15).
        05 lastName                    PIC X(15).
        05 gender                      PIC X(10).
        05 birthDate                   PIC X(10).
        05 subscriptionDate            PIC X(10).
        05 emailAddress                PIC X(40).
        05 subscriptionProgram         PIC X(10).
        05 FILLER                      PIC X(380).
        05 contentId                   PIC X(10).
        05 watchPercent                PIC X(5).
        05 startTime                   PIC X(10).
        05 mediaType                   PIC X(5).
        05 FILLER                      PIC X(170).
      
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

      * Customer-Stat-File
       01 WS-Customer-Stat-File.
        05 accountNumber               PIC X(10).
        05 Watch-Number                PIC X(10).
        05 Watch-Time-Spent            PIC X(10).
        05 Watch-Timelapse             PIC X(10).
        05 FILLER                      PIC X(160).

      * Title-Stat-File
       01 WS-Title-Stat-File.
        05 contentId                   PIC X(10).
        05 Watch-Number                PIC X(10).
        05 Watch-Time-Spent            PIC X(10).
        05 Watch-Timelapse             PIC X(10).
        05 FILLER                      PIC X(160).

      * Allstat-File
       01 WS-All-Stat-File             PIC X(80).
      * Separation Lines
       01 Separation-Lines.
        05 Blank-Line                  PIC X(80) Value SPACES.
        05 Dash-Line                   PIC X(80) Value ALL '-'.
      * Time-Line
       01 Time-Line.
        05 Date-of-Run.
         10 Date-of-Run-YYYY           PIC 9(4).
         10 FILLER                     PIC X(1) VALUE '-'.
         10 Date-of-Run-MM             PIC 9(2).
         10 FILLER                     PIC X(1) VALUE '-'.
         10 Date-of-Run-DD             PIC 9(2).
        05 FILLER                      PIC X(60) Value SPACES.
        05 Time-of-Run.
           10 Time-of-Run-HH             PIC 9(2).
           10 FILLER                     PIC X(1) VALUE ':'.
           10 Time-of-Run-MIN            PIC 9(2).
           10 FILLER                     PIC X(1) VALUE ':'.
           10 Time-of-Run-SS             PIC 9(2).
      * Header-Lines
       01 Header-Line.
        05 FILLER                 PIC X(29) Value SPACES.
        05 FILLER                 PIC X(23)
              Value 'AIFlix Daily Statistics'.
        05 FILLER                 PIC X(28) Value SPACES. 
       01 Header-General-Line.
        05 FILLER                 PIC X(10) Value SPACES.
        05 FILLER                 PIC X(21)
              Value 'General Statistics : '.
        05 FILLER                 PIC X(39) Value SPACES.
       01 Header-Content-Line.
        05 FILLER                 PIC X(10) Value SPACES.
        05 FILLER                 PIC X(21)
              Value 'Content Statistics : '.
        05 FILLER                 PIC X(39) Value SPACES.
       01 Header-Subscription-Line.
        05 FILLER                 PIC X(10) Value SPACES.
        05 FILLER                 PIC X(26)
              Value 'Subscription Statistics : '.
        05 FILLER                 PIC X(34) Value SPACES.
       01 Header-Customer-Line.
        05 FILLER                 PIC X(10) Value SPACES.
        05 FILLER                 PIC X(23)
              Value 'Customer Statistics : '.
        05 FILLER                 PIC X(37) Value SPACES.
       01 Header-Views-Line.
        05 FILLER                 PIC X(10) Value SPACES.
        05 FILLER                 PIC X(20)
              Value 'Views Statistics : '.
        05 FILLER                 PIC X(40) Value SPACES.
      * General Statistics
       01 Total-Viewer-Line.
        05 FILLER                 PIC X(10) Value SPACES.
        05 FILLER                 PIC X(45)
              Value 'Total Number of Viewers : '.
        05 Total-Viewer           PIC ZZZBZZZBZZ9.
        05 FILLER                 PIC X(14) Value SPACES.
       01 Total-Content-Line.
        05 FILLER                 PIC X(10) Value SPACES.
        05 FILLER                 PIC X(45)
              Value 'Total Number of Contents : '.
        05 Total-Content          PIC ZZZBZZZBZZ9.
        05 FILLER                 PIC X(14) Value SPACES.
       01 Total-Subscription-Line.
        05 FILLER                 PIC X(10) Value SPACES.
        05 FILLER                 PIC X(45)
              Value 'Total Number of Subscriptions : '.
        05 Total-Subscription     PIC ZZZBZZZBZZ9.
        05 FILLER                 PIC X(14) Value SPACES.
       01 Total-Views-Line.
        05 FILLER                 PIC X(10) Value SPACES.
        05 FILLER                 PIC X(45)
              Value 'Total Number of Views : '.
        05 Total-Views            PIC ZZZBZZZBZZ9.
        05 FILLER                 PIC X(14) Value SPACES.
       01 Total-Watch-Time-Line.
        05 FILLER                 PIC X(10) Value SPACES.
        05 FILLER                 PIC X(45)
              Value 'Total Watch Time (min): '.
        05 Total-Watch-Time       PIC ZZZBZZZBZZZBZZ9.
        05 FILLER                 PIC X(14) Value SPACES.
      * Views Statistics
       01 Total-Views-Web-Line.
        05 FILLER                 PIC X(10) Value SPACES.
        05 FILLER                 PIC X(45)
              Value 'Total Views on the Web : '.
        05 Total-Views-Web        PIC ZZZBZZZBZZ9.
        05 FILLER                 PIC X(14) Value SPACES.
       01 Total-Views-App-Line.
        05 FILLER                 PIC X(10) Value SPACES.
        05 FILLER                 PIC X(45)
              Value 'Total Views on the App : '.
        05 Total-Views-App        PIC ZZZBZZZBZZ9.
        05 FILLER                 PIC X(14) Value SPACES.
       01 Total-Views-TV-Line.
        05 FILLER                 PIC X(10) Value SPACES.
        05 FILLER                 PIC X(45)
              Value 'Total Views on the TV : '.
        05 Total-Views-TV         PIC ZZZBZZZBZZ9.
        05 FILLER                 PIC X(14) Value SPACES.
       01 Max-Views-Content-Line.
        05 FILLER                 PIC X(10) Value SPACES.
        05 FILLER                 PIC X(45)
              Value 'Max Views on a Content : '.
        05 Max-Views-Content      PIC ZZZBZZZBZZ9.
        05 FILLER                 PIC X(14) Value SPACES.
       01 Min-Views-Content-Line.
        05 FILLER                 PIC X(10) Value SPACES.
        05 FILLER                 PIC X(45)
              Value 'Min Views on a Content : '.
        05 Min-Views-Content      PIC ZZZBZZZBZZ9.
        05 FILLER                 PIC X(14) Value SPACES.
       01 Avg-Views-Content-Line.
        05 FILLER                 PIC X(10) Value SPACES.
        05 FILLER                 PIC X(45)
              Value 'Avg Views on a Content : '.
        05 Avg-Views-Content      PIC ZZZBZZZBZZ9.
        05 FILLER                 PIC X(14) Value SPACES.
       01 Max-Views-Customer-Line.
        05 FILLER                 PIC X(10) Value SPACES.
        05 FILLER                 PIC X(45)
              Value 'Max Views for a Customer : '.
        05 Max-Views-Customer     PIC ZZZBZZZBZZ9.
        05 FILLER                 PIC X(14) Value SPACES.
       01 Min-Views-Customer-Line.  
        05 FILLER                 PIC X(10) Value SPACES.
        05 FILLER                 PIC X(45)
              Value 'Min Views for a Customer : '.
        05 Min-Views-Customer     PIC ZZZBZZZBZZ9.
        05 FILLER                 PIC X(14) Value SPACES.
       01 Avg-Views-Customer-Line.
        05 FILLER                 PIC X(10) Value SPACES.
        05 FILLER                 PIC X(45)
              Value 'Avg Views for a Customer : '.
        05 Avg-Views-Customer     PIC ZZZBZZZBZZ9.
        05 FILLER                 PIC X(14) Value SPACES.
       01 Max-View-Content-Time-Line.
        05 FILLER                 PIC X(10) Value SPACES.
        05 FILLER                 PIC X(45)
              Value 'Max View Time on a Content : '.
        05 Max-View-Content-Time  PIC ZZZBZZZBZZZBZZ9.
        05 FILLER                 PIC X(10) Value SPACES.
       01 Min-View-Content-Time-Line.
        05 FILLER                 PIC X(10) Value SPACES.
        05 FILLER                 PIC X(45)
              Value 'Min View Time on a Content : '.
        05 Min-View-Content-Time  PIC ZZZBZZZBZZZBZZ9.
        05 FILLER                 PIC X(10) Value SPACES.
       01 Avg-View-Content-Time-Line.    
        05 FILLER                 PIC X(10) Value SPACES.
        05 FILLER                 PIC X(45)
              Value 'Avg View Time on a Content : '.
        05 Avg-View-Content-Time  PIC ZZZBZZZBZZZBZZ9.
        05 FILLER                 PIC X(10) Value SPACES.
       01 Max-View-Customer-Time-Line.
        05 FILLER                 PIC X(10) Value SPACES.
        05 FILLER                 PIC X(45)
              Value 'Max View Time for a Customer : '.
        05 Max-View-Customer-Time  PIC ZZZBZZZBZZZBZZ9.
        05 FILLER                 PIC X(10) Value SPACES.
       01 Min-View-Customer-Time-Line.
        05 FILLER                 PIC X(10) Value SPACES.
        05 FILLER                 PIC X(45)
              Value 'Min View Time for a Customer : '.
        05 Min-View-Customer-Time  PIC ZZZBZZZBZZZBZZ9.
        05 FILLER                 PIC X(10) Value SPACES.
       01 Avg-View-Customer-Time-Line.
        05 FILLER                 PIC X(10) Value SPACES.
        05 FILLER                 PIC X(45)
              Value 'Avg View Time for a Customer : '.
        05 Avg-View-Customer-Time  PIC ZZZBZZZBZZZBZZ9.
        05 FILLER                 PIC X(10) Value SPACES.
      * Subscription Statistics
      * Subscription Program
       01 Total-Sub-Program-Std-Line.
        05 FILLER                 PIC X(10) Value SPACES.
        05 FILLER                 PIC X(45)
              Value 'Total Number of Standard Subscriptions : '. 
        05 Total-Sub-Program-Stantard PIC ZZZBZZZBZZ9.
        05 FILLER                 PIC X(14) Value SPACES.
       01 Total-Sub-Program-Ext-Line.    
        05 FILLER                 PIC X(10) Value SPACES.
        05 FILLER                 PIC X(45)
              Value 'Total Number of Extended Subscriptions : '.
        05 Total-Sub-Program-Extended PIC ZZZBZZZBZZ9.
        05 FILLER                 PIC X(14) Value SPACES.  
       01 Total-Sub-Program-Prm-Line.    
        05 FILLER                 PIC X(10) Value SPACES.
        05 FILLER                 PIC X(45)
              Value 'Total Number of Premium Subscriptions : '.
        05 Total-Sub-Program-Premium PIC ZZZBZZZBZZ9.
        05 FILLER                 PIC X(14) Value SPACES. 
      * Customer Age  
       01 Max-Age-Cust-Standard-Line.
        05 FILLER                 PIC X(10) Value SPACES.
        05 FILLER                 PIC X(45)
              Value 'Max Age of Standard Subscribers : '.
        05 Max-Age-Cust-Standard  PIC ZZ9.
        05 FILLER                 PIC X(22) Value SPACES.
       01 Min-Age-Cust-Standard-Line.
        05 FILLER                 PIC X(10) Value SPACES.
        05 FILLER                 PIC X(45)
              Value 'Min Age of Standard Subscribers : '.
        05 Min-Age-Cust-Standard  PIC ZZ9.
        05 FILLER                 PIC X(22) Value SPACES.
       01 Avg-Age-Cust-Standard-Line. 
        05 FILLER                 PIC X(10) Value SPACES.
        05 FILLER                 PIC X(45)
              Value 'Avg Age of Standard Subscribers : '.
        05 Avg-Age-Cust-Standard  PIC ZZ9.
        05 FILLER                 PIC X(22) Value SPACES.
       01 Max-Age-Cust-Extended-Line.
        05 FILLER                 PIC X(10) Value SPACES.
        05 FILLER                 PIC X(45)
              Value 'Max Age of Extended Subscribers : '.
        05 Max-Age-Cust-Extended  PIC ZZ9.
        05 FILLER                 PIC X(22) Value SPACES.
       01 Min-Age-Cust-Extended-Line.
        05 FILLER                 PIC X(10) Value SPACES.
        05 FILLER                 PIC X(45)
              Value 'Min Age of Extended Subscribers : '.
        05 Min-Age-Cust-Extended  PIC ZZ9.
        05 FILLER                 PIC X(22) Value SPACES.
       01 Avg-Age-Cust-Extended-Line. 
        05 FILLER                 PIC X(10) Value SPACES.
        05 FILLER                 PIC X(45)
              Value 'Avg Age of Extended Subscribers : '.
        05 Avg-Age-Cust-Extended  PIC ZZ9.
        05 FILLER                 PIC X(22) Value SPACES.
       01 Max-Age-Cust-Premium-Line.
        05 FILLER                 PIC X(10) Value SPACES.
        05 FILLER                 PIC X(45)
              Value 'Max Age of Premium Subscribers : '.
        05 Max-Age-Cust-Premium   PIC ZZ9.
        05 FILLER                 PIC X(22) Value SPACES.
       01 Min-Age-Cust-Premium-Line.  
        05 FILLER                 PIC X(10) Value SPACES.
        05 FILLER                 PIC X(45)
              Value 'Min Age of Premium Subscribers : '.
        05 Min-Age-Cust-Premium   PIC ZZ9.
        05 FILLER                 PIC X(22) Value SPACES.
       01 Avg-Age-Cust-Premium-Line.
        05 FILLER                 PIC X(10) Value SPACES.
        05 FILLER                 PIC X(45)
              Value 'Avg Age of Premium Subscribers : '.
        05 Avg-Age-Cust-Premium   PIC ZZ9.
        05 FILLER                 PIC X(22) Value SPACES.
       01 Max-Age-Cust-Total-Line.
        05 FILLER                 PIC X(10) Value SPACES.
        05 FILLER                 PIC X(45)
              Value 'Max Age of All Subscribers : '.
        05 Max-Age-Cust-Total     PIC ZZ9.
        05 FILLER                 PIC X(22) Value SPACES.
       01 Min-Age-Cust-Total-Line.
        05 FILLER                 PIC X(10) Value SPACES.
        05 FILLER                 PIC X(45)
              Value 'Min Age of All Subscribers : '.
        05 Min-Age-Cust-Total     PIC ZZ9.
        05 FILLER                 PIC X(22) Value SPACES.
       01 Avg-Age-Cust-Total-Line. 
        05 FILLER                 PIC X(10) Value SPACES.
        05 FILLER                 PIC X(45)
              Value 'Avg Age of All Subscribers : '.
        05 Avg-Age-Cust-Total     PIC ZZ9.
        05 FILLER                 PIC X(22) Value SPACES.
      * Subscription Age
       01 Max-Age-Sub-Standard-Line.
        05 FILLER                 PIC X(10) Value SPACES.
        05 FILLER                 PIC X(45)
              Value 'Max Age of Standard Subscriptions : '.
        05 Max-Age-Sub-Standard   PIC ZZ9.
        05 FILLER                 PIC X(22) Value SPACES.   
       01 Min-Age-Sub-Standard-Line.
        05 FILLER                 PIC X(10) Value SPACES.
        05 FILLER                 PIC X(45)
              Value 'Min Age of Standard Subscriptions : '.
        05 Min-Age-Sub-Standard   PIC ZZ9.
        05 FILLER                 PIC X(22) Value SPACES.
       01 Avg-Age-Sub-Standard-Line.
        05 FILLER                 PIC X(10) Value SPACES.
        05 FILLER                 PIC X(45)
              Value 'Avg Age of Standard Subscriptions : '.
        05 Avg-Age-Sub-Standard   PIC ZZ9.
        05 FILLER                 PIC X(22) Value SPACES.
       01 Max-Age-Sub-Extended-Line.
        05 FILLER                 PIC X(10) Value SPACES.
        05 FILLER                 PIC X(45)
              Value 'Max Age of Extended Subscriptions : '.
        05 Max-Age-Sub-Extended   PIC ZZ9.
        05 FILLER                 PIC X(22) Value SPACES.
       01 Min-Age-Sub-Extended-Line.
        05 FILLER                 PIC X(10) Value SPACES.
        05 FILLER                 PIC X(45)
              Value 'Min Age of Extended Subscriptions : '.
        05 Min-Age-Sub-Extended   PIC ZZ9.
        05 FILLER                 PIC X(22) Value SPACES.
       01 Avg-Age-Sub-Extended-Line.
        05 FILLER                 PIC X(10) Value SPACES.
        05 FILLER                 PIC X(45)
              Value 'Avg Age of Extended Subscriptions : '.
        05 Avg-Age-Sub-Extended   PIC ZZ9.
        05 FILLER                 PIC X(22) Value SPACES.
       01 Max-Age-Sub-Premium-Line.
        05 FILLER                 PIC X(10) Value SPACES.
        05 FILLER                 PIC X(45)
              Value 'Max Age of Premium Subscriptions : '.
        05 Max-Age-Sub-Premium    PIC ZZ9.
        05 FILLER                 PIC X(22) Value SPACES.
       01 Min-Age-Sub-Premium-Line.
        05 FILLER                 PIC X(10) Value SPACES.
        05 FILLER                 PIC X(45)
              Value 'Min Age of Premium Subscriptions : '.
        05 Min-Age-Sub-Premium    PIC ZZ9.
        05 FILLER                 PIC X(22) Value SPACES.
       01 Avg-Age-Sub-Premium-Line.
        05 FILLER                 PIC X(10) Value SPACES.
        05 FILLER                 PIC X(45)
              Value 'Avg Age of Premium Subscriptions : '.
        05 Avg-Age-Sub-Premium    PIC ZZ9.
        05 FILLER                 PIC X(22) Value SPACES.
       01 Max-Age-Sub-Total-Line.
        05 FILLER                 PIC X(10) Value SPACES.
        05 FILLER                 PIC X(45)
              Value 'Max Age of All Subscriptions : '.
        05 Max-Age-Sub-Total      PIC ZZ9.
        05 FILLER                 PIC X(22) Value SPACES.
       01 Min-Age-Sub-Total-Line.
        05 FILLER                 PIC X(10) Value SPACES.
        05 FILLER                 PIC X(45)
              Value 'Min Age of All Subscriptions : '.
        05 Min-Age-Sub-Total      PIC ZZ9.
        05 FILLER                 PIC X(22) Value SPACES.
       01 Avg-Age-Sub-Total-Line.
        05 FILLER                 PIC X(10) Value SPACES.
        05 FILLER                 PIC X(45)
              Value 'Avg Age of All Subscriptions : '.
        05 Avg-Age-Sub-Total      PIC ZZ9.
        05 FILLER                 PIC X(22) Value SPACES.
      * Content Statistics
          
       01 Total-Content-1880s-Line.
        05 FILLER                 PIC X(10) Value SPACES.
        05 FILLER                 PIC X(45)
              Value 'Total Number of 1880s Content : '.
        05 Total-Content-1880s    PIC ZZZBZZZBZZ9.
        05 FILLER                 PIC X(14) Value SPACES.
       01 Total-Content-1890s-Line.      
        05 FILLER                 PIC X(10) Value SPACES.
        05 FILLER                 PIC X(45)
              Value 'Total Number of 1890s Content : '.
        05 Total-Content-1890s    PIC ZZZBZZZBZZ9.
        05 FILLER                 PIC X(14) Value SPACES.
       01 Total-Content-1900s-Line.      
        05 FILLER                 PIC X(10) Value SPACES.
        05 FILLER                 PIC X(45)
              Value 'Total Number of 1900s Content : '.
        05 Total-Content-1900s    PIC ZZZBZZZBZZ9.
        05 FILLER                 PIC X(14) Value SPACES.
       01 Total-Content-1910s-Line.      
        05 FILLER                 PIC X(10) Value SPACES.
        05 FILLER                 PIC X(45)
              Value 'Total Number of 1910s Content : '.
        05 Total-Content-1910s    PIC ZZZBZZZBZZ9.
        05 FILLER                 PIC X(14) Value SPACES.
       01 Total-Content-1920s-Line.      
        05 FILLER                 PIC X(10) Value SPACES.
        05 FILLER                 PIC X(45)
              Value 'Total Number of 1920s Content : '.
        05 Total-Content-1920s    PIC ZZZBZZZBZZ9.
        05 FILLER                 PIC X(14) Value SPACES.
       01 Total-Content-1930s-Line.      
        05 FILLER                 PIC X(10) Value SPACES.
        05 FILLER                 PIC X(45)
              Value 'Total Number of 1930s Content : '.
        05 Total-Content-1930s    PIC ZZZBZZZBZZ9.
        05 FILLER                 PIC X(14) Value SPACES.
       01 Total-Content-1940s-Line.      
        05 FILLER                 PIC X(10) Value SPACES.
        05 FILLER                 PIC X(45)
              Value 'Total Number of 1940s Content : '.
        05 Total-Content-1940s    PIC ZZZBZZZBZZ9.
        05 FILLER                 PIC X(14) Value SPACES.
       01 Total-Content-1950s-Line.      
        05 FILLER                 PIC X(10) Value SPACES.
        05 FILLER                 PIC X(45)
              Value 'Total Number of 1950s Content : '.
        05 Total-Content-1950s    PIC ZZZBZZZBZZ9.
        05 FILLER                 PIC X(14) Value SPACES.
       01 Total-Content-1960s-Line.      
        05 FILLER                 PIC X(10) Value SPACES.
        05 FILLER                 PIC X(45)
              Value 'Total Number of 1960s Content : '.
        05 Total-Content-1960s    PIC ZZZBZZZBZZ9.
        05 FILLER                 PIC X(14) Value SPACES.
       01 Total-Content-1970s-Line.      
        05 FILLER                 PIC X(10) Value SPACES.
        05 FILLER                 PIC X(45)
              Value 'Total Number of 1970s Content : '.
        05 Total-Content-1970s    PIC ZZZBZZZBZZ9.
        05 FILLER                 PIC X(14) Value SPACES.
       01 Total-Content-1980s-Line.      
        05 FILLER                 PIC X(10) Value SPACES.
        05 FILLER                 PIC X(45)
              Value 'Total Number of 1980s Content : '.
        05 Total-Content-1980s    PIC ZZZBZZZBZZ9.
        05 FILLER                 PIC X(14) Value SPACES.
       01 Total-Content-1990s-Line.      
        05 FILLER                 PIC X(10) Value SPACES.
        05 FILLER                 PIC X(45)
              Value 'Total Number of 1990s Content : '.
        05 Total-Content-1990s    PIC ZZZBZZZBZZ9.
        05 FILLER                 PIC X(14) Value SPACES.
       01 Total-Content-2000s-Line.      
        05 FILLER                 PIC X(10) Value SPACES.
        05 FILLER                 PIC X(45)
              Value 'Total Number of 2000s Content : '.
        05 Total-Content-2000s    PIC ZZZBZZZBZZ9.
        05 FILLER                 PIC X(14) Value SPACES.
       01 Total-Content-2010s-Line.      
        05 FILLER                 PIC X(10) Value SPACES.
        05 FILLER                 PIC X(45)
              Value 'Total Number of 2010s Content : '.
        05 Total-Content-2010s    PIC ZZZBZZZBZZ9.
        05 FILLER                 PIC X(14) Value SPACES.
       01 Total-Content-2020s-Line.      
        05 FILLER                 PIC X(10) Value SPACES.
        05 FILLER                 PIC X(45)
              Value 'Total Number of 2020s Content : '.
        05 Total-Content-2020s    PIC ZZZBZZZBZZ9.
        05 FILLER                 PIC X(14) Value SPACES.
       01 Total-Content-Adult-Line.
        05 FILLER                 PIC X(10) Value SPACES.
        05 FILLER                 PIC X(45)
              Value 'Total Number of Adult Content : '.
        05 Total-Content-Adult    PIC ZZZBZZZBZZ9.
        05 FILLER                 PIC X(14) Value SPACES.
       01 Total-Content-Non-Adult-Line.
        05 FILLER                 PIC X(10) Value SPACES.
        05 FILLER                 PIC X(45)
              Value 'Total Number of Non-Adult Content : '.
        05 Total-Content-Non-Adult PIC ZZZBZZZBZZ9.
        05 FILLER                 PIC X(14) Value SPACES.  
       01 Min-Age-Content-Line.
        05 FILLER                 PIC X(10) Value SPACES.
        05 FILLER                 PIC X(45)
              Value 'Min Age of All Content : '.
        05 Min-Age-Content        PIC X(4).
        05 FILLER                 PIC X(21) Value SPACES.
       01 Max-Age-Content-Line.
        05 FILLER                 PIC X(10) Value SPACES.
        05 FILLER                 PIC X(45)
              Value 'Max Age of All Content : '.
        05 Max-Age-Content        PIC X(4).
        05 FILLER                 PIC X(21) Value SPACES.


      * Status
       01 FILE-STATUS.
        05 STATUS-MERGED-FILE            PIC XX.  
        05 STATUS-TITLE-FILE             PIC XX.
        05 STATUS-CSTSTAT-FILE           PIC XX.
        05 STATUS-TTLSTAT-FILE           PIC XX.
        05 STATUS-ALLSTAT-FILE           PIC XX.

      * Working variables
       01 WS-Working-Variables.
        05 WS-Current-Date.
         10 WS-Current-YYYY              PIC 9(4) VALUE ZERO.
         10 WS-Current-MM                PIC 9(2) VALUE ZERO.
         10 WS-Current-DD                PIC 9(2) VALUE ZERO. 
         10 WS-Current-HH                PIC 9(2) VALUE ZERO.
         10 WS-Current-MIN               PIC 9(2) VALUE ZERO.
         10 WS-Current-SS                PIC 9(2) VALUE ZERO.
         10 WS-Current-THH               PIC 9(2) VALUE ZERO.
         10 WS-Current-TT                PIC 9(2) VALUE ZERO.
        05 WS-BirthDate.
         10 WS-BirthDate-YYYY            PIC 9(4) VALUE ZERO.
         10 FILLER                       PIC X(1) VALUE '-'.
         10 WS-BirthDate-MM              PIC 9(2) VALUE ZERO.
         10 FILLER                       PIC X(1) VALUE '-'.
         10 WS-BirthDate-DD              PIC 9(2) VALUE ZERO.
        05 WS-Subscription-Date.
         10 WS-Subscription-Date-YYYY    PIC 9(4) VALUE ZERO.
         10 FILLER                       PIC X(1) VALUE '-'.
         10 WS-Subscription-Date-MM      PIC 9(2) VALUE ZERO.
         10 FILLER                       PIC X(1) VALUE '-'.
         10 WS-Subscription-Date-DD      PIC 9(2) VALUE ZERO.
        05 WS-watchPercent               PIC 9(5) VALUE ZERO.
        05 WS-runtimeMinutes             PIC 9(5) VALUE ZERO.
        05 WS-Age-Cust-TMP               PIC 9(3) VALUE ZERO.
        05 WS-Age-Sub-TMP                PIC 9(3) VALUE ZERO.
        05 WS-startYear-TMP              PIC 9(4) VALUE ZERO.
        05 WS-Decade                     PIC X(3) VALUE SPACES.
        05 WS-Watch-Time-TMP             PIC 9(10) VALUE ZERO.
        05 WS-Content-Views              PIC 9(10) VALUE ZERO.
        05 WS-Customer-Views             PIC 9(10) VALUE ZERO.
        05 WS-acct-Number-TMP            PIC 9(10) VALUE ZERO.
        05 WS-Total-Content-1880s        PIC 9(11) VALUE ZERO.
        05 WS-Total-Content-1890s        PIC 9(11) VALUE ZERO.
        05 WS-Total-Content-1900s        PIC 9(11) VALUE ZERO.
        05 WS-Total-Content-1910s        PIC 9(11) VALUE ZERO.
        05 WS-Total-Content-1920s        PIC 9(11) VALUE ZERO.
        05 WS-Total-Content-1930s        PIC 9(11) VALUE ZERO.
        05 WS-Total-Content-1940s        PIC 9(11) VALUE ZERO.
        05 WS-Total-Content-1950s        PIC 9(11) VALUE ZERO.
        05 WS-Total-Content-1960s        PIC 9(11) VALUE ZERO.
        05 WS-Total-Content-1970s        PIC 9(11) VALUE ZERO.
        05 WS-Total-Content-1980s        PIC 9(11) VALUE ZERO.
        05 WS-Total-Content-1990s        PIC 9(11) VALUE ZERO.
        05 WS-Total-Content-2000s        PIC 9(11) VALUE ZERO.
        05 WS-Total-Content-2010s        PIC 9(11) VALUE ZERO.
        05 WS-Total-Content-2020s        PIC 9(11) VALUE ZERO.
        05 WS-Total-Viewer               PIC 9(11) VALUE ZERO.
        05 WS-Total-Content              PIC 9(11) VALUE ZERO.
        05 WS-Total-Subscription         PIC 9(11) VALUE ZERO.
        05 WS-Total-Views                PIC 9(11) VALUE ZERO.
        05 WS-Total-Watch-Time           PIC 9(11) VALUE ZERO.
        05 WS-Total-Views-Web            PIC 9(11) VALUE ZERO.
        05 WS-Total-Views-App            PIC 9(11) VALUE ZERO.
        05 WS-Total-Views-TV             PIC 9(11) VALUE ZERO.
        05 WS-Total-Sub-Program-Std      PIC 9(11) VALUE ZERO.
        05 WS-Total-Sub-Program-Ext      PIC 9(11) VALUE ZERO.
        05 WS-Total-Sub-Program-Prm      PIC 9(11) VALUE ZERO.
        05 WS-Total-Content-Watch-Time   PIC 9(11) VALUE ZERO.
        05 WS-Max-Views-Content          PIC 9(11) VALUE ZERO.
        05 WS-Min-Views-Content          PIC 9(11) VALUE 99999999999.
        05 WS-Avg-Views-Content          PIC 9(11) VALUE ZERO.
        05 WS-Sum-Views-Content          PIC 9(11) VALUE ZERO.
        05 WS-Max-Views-Customer         PIC 9(11) VALUE ZERO.
        05 WS-Min-Views-Customer         PIC 9(11) VALUE 99999999999.
        05 WS-Avg-Views-Customer         PIC 9(11) VALUE ZERO.
        05 WS-Sum-Views-Customer         PIC 9(11) VALUE ZERO.
        05 WS-Max-View-Content-Time      PIC 9(11) VALUE ZERO.
        05 WS-Min-View-Content-Time      PIC 9(11) VALUE 99999999999.
        05 WS-Avg-View-Content-Time      PIC 9(11) VALUE ZERO.
        05 WS-Sum-View-Content-Time      PIC 9(11) VALUE ZERO.
        05 WS-Max-View-Customer-Time     PIC 9(11) VALUE ZERO.
        05 WS-Min-View-Customer-Time     PIC 9(11) VALUE 99999999999.
        05 WS-Avg-View-Customer-Time     PIC 9(11) VALUE ZERO.
        05 WS-Sum-View-Customer-Time     PIC 9(11) VALUE ZERO.
        05 WS-Max-Age-Cust-Standard      PIC 9(03) VALUE ZERO.
        05 WS-Min-Age-Cust-Standard      PIC 9(03) VALUE 999.
        05 WS-Avg-Age-Cust-Standard      PIC 9(03) VALUE ZERO.
        05 WS-Sum-Age-Cust-Standard      PIC 9(11) VALUE ZERO.
        05 WS-Max-Age-Cust-Extended      PIC 9(03) VALUE ZERO.
        05 WS-Min-Age-Cust-Extended      PIC 9(03) VALUE 999.
        05 WS-Avg-Age-Cust-Extended      PIC 9(03) VALUE ZERO.
        05 WS-Sum-Age-Cust-Extended      PIC 9(11) VALUE ZERO.
        05 WS-Max-Age-Cust-Premium       PIC 9(03) VALUE ZERO.
        05 WS-Min-Age-Cust-Premium       PIC 9(03) VALUE 999.
        05 WS-Avg-Age-Cust-Premium       PIC 9(03) VALUE ZERO.
        05 WS-Sum-Age-Cust-Premium       PIC 9(11) VALUE ZERO.
        05 WS-Max-Age-Cust-Total         PIC 9(03) VALUE ZERO.
        05 WS-Min-Age-Cust-Total         PIC 9(03) VALUE 999.
        05 WS-Avg-Age-Cust-Total         PIC 9(03) VALUE ZERO.
        05 WS-Sum-Age-Cust-Total         PIC 9(11) VALUE ZERO.
        05 WS-Max-Age-Sub-Standard       PIC 9(03) VALUE ZERO.
        05 WS-Min-Age-Sub-Standard       PIC 9(03) VALUE 999.
        05 WS-Avg-Age-Sub-Standard       PIC 9(03) VALUE ZERO.
        05 WS-Sum-Age-Sub-Standard       PIC 9(03) VALUE ZERO.
        05 WS-Max-Age-Sub-Extended       PIC 9(03) VALUE ZERO.
        05 WS-Min-Age-Sub-Extended       PIC 9(03) VALUE 999.
        05 WS-Avg-Age-Sub-Extended       PIC 9(03) VALUE ZERO.
        05 WS-Sum-Age-Sub-Extended       PIC 9(11) VALUE ZERO.
        05 WS-Max-Age-Sub-Premium        PIC 9(03) VALUE ZERO.
        05 WS-Min-Age-Sub-Premium        PIC 9(03) VALUE 999.
        05 WS-Avg-Age-Sub-Premium        PIC 9(03) VALUE ZERO.  
        05 WS-Sum-Age-Sub-Premium        PIC 9(11) VALUE ZERO.
        05 WS-Max-Age-Sub-Total          PIC 9(03) VALUE ZERO.
        05 WS-Min-Age-Sub-Total          PIC 9(03) VALUE 999.
        05 WS-Avg-Age-Sub-Total          PIC 9(03) VALUE ZERO.
        05 WS-Sum-Age-Sub-Total          PIC 9(11) VALUE ZERO.
        05 WS-Total-Content-Adult        PIC 9(11) VALUE ZERO.
        05 WS-Total-Content-Non-Adult    PIC 9(11) VALUE ZERO. 
        05 WS-Min-Age-Content            PIC 9(04) VALUE 9999.
        05 WS-Max-Age-Content            PIC 9(04) VALUE ZERO.   

      * End-Of-Files
       01 End-Of-File-M                  PIC X VALUE 'N'.
       01 End-Of-File-T                  PIC X VALUE 'N'.
           
       
       PROCEDURE DIVISION.
      *-----------------------------------------------------------------
      * Principal Process
      *-----------------------------------------------------------------
       
       1000-Principal-Process.

           PERFORM 9000-Initialization
           PERFORM 9100-Open-Files
           PERFORM 2000-Secondary-Process
           PERFORM 5530-Avg-Calculation
           PERFORM 6000-Move-Working-Variables 
           PERFORM 6100-Write-Statistics-Report 
           PERFORM 9900-Close-Files
           PERFORM 9999-Finalize.
      
      *-----------------------------------------------------------------
      * Secondary Process
      *-----------------------------------------------------------------

       2000-Secondary-Process.

      * First Read
           PERFORM 9200-Read-Merged-Record 

      * Process Records
           PERFORM 2100-Process-Record
             UNTIL End-Of-File-M = 'Y' AND End-Of-File-T = 'Y'.

      *-----------------------------------------------------------------
      * Process Record
      *-----------------------------------------------------------------

       2100-Process-Record.
           
           PERFORM 9210-Read-Title-Record 
           PERFORM 2200-Process-Merged-Record
             UNTIL End-Of-File-M = 'Y' AND End-Of-File-T = 'Y'
             OR contentId  of WS-Title-File 
             < contentId  of WS-Merged-File.
             
      *-----------------------------------------------------------------
      * Process Merged Record
      *-----------------------------------------------------------------

       2200-Process-Merged-Record.

           IF contentId of WS-Title-File < 
              contentId of WS-Merged-File
              PERFORM 3100-Unmatched-Customer-Record
           END-IF.

           
           IF contentId of WS-Title-File  = 
              contentId of WS-Merged-File 
             PERFORM 3000-Retrieve-Data
             PERFORM 9200-Read-Merged-Record
           END-IF.
           
           IF contentId of WS-Title-File > 
              contentId of WS-Merged-File
             PERFORM 3000-Retrieve-Data
             PERFORM 9200-Read-Merged-Record
           END-IF.
      *-----------------------------------------------------------------
      * Retrieve Data   
      *-----------------------------------------------------------------

       3000-Retrieve-Data.

      * General
      *     PERFORM 5000-Total-Viewer-Count
      *     PERFORM 5010-Total-Content-Count
      *     PERFORM 5020-Total-Subscription-Count       

      * Customer Level
           IF WS-Merged-File NOT = HIGH-VALUES
      * Views
                PERFORM 5430-Max-Min-Views-Cont-Calc
                PERFORM 5440-Max-Min-Views-Cust-Calc 
                

      * Subscription
                PERFORM 5200-Total-Sub-Standard-Count
                PERFORM 5210-Total-Sub-Extended-Count
                PERFORM 5220-Total-Sub-Premium-Count
                PERFORM 5230-Max-Min-Age-Sub-Calc
                PERFORM 5240-Avg-Age-Sub-Calc
      
      * Customer
                PERFORM 5300-Max-Min-Age-Customer-Calc 
                PERFORM 5310-Avg-Age-Customer-Calc 
           END-IF 

      * Content Level
           IF WS-Title-File NOT = HIGH-VALUES
               PERFORM 5450-Max-View-Contt-Time-Calc 
               PERFORM 5460-Max-View-Cust-Time-Calc
           END-IF
           .

      *-----------------------------------------------------------------
      * Unmatched Customer Record
      *-----------------------------------------------------------------

       3100-Unmatched-Customer-Record.    

           DISPLAY 'Unmatched Customer Record : ' 
                   contentId  of WS-Merged-File.

      *=================================================================
      * General Subroutines
      *=================================================================

      *-----------------------------------------------------------------
      * Total Viewer Calculation
      *-----------------------------------------------------------------

       5000-Total-Viewer-Count.
       
           ADD 1 TO WS-Total-Viewer.

      *-----------------------------------------------------------------
      * Total Content Calculation               
      *-----------------------------------------------------------------

       5010-Total-Content-Count.
       
           ADD 1 TO WS-Total-Content.

      *-----------------------------------------------------------------
      * Total Subscription Calculation
      *-----------------------------------------------------------------

       5020-Total-Subscription-Count.
       
           ADD 1 TO WS-Total-Subscription.

      *-----------------------------------------------------------------
      * Total Views Calculation
      *-----------------------------------------------------------------

       5030-Total-Views-Count.
       
           ADD 1 TO WS-Total-Views.     

      *-----------------------------------------------------------------
      * Total Watch Time Calculation
      *-----------------------------------------------------------------

       5040-Total-Watch-Time-Count.
       
           COMPUTE WS-Watch-Time-TMP = 
           WS-watchPercent / 100 * WS-runtimeMinutes
           ADD WS-Watch-Time-TMP TO WS-Total-Watch-Time.

      *=================================================================
      * Content Subroutines
      *=================================================================

      *-----------------------------------------------------------------
      * Decade Calculation
      *-----------------------------------------------------------------

       5100-Decade-Count.
       
           MOVE startYear (1:3) to WS-Decade
           
           Evaluate WS-Decade 
           When '188' ADD 1 TO WS-Total-Content-1880s 
           When '189' ADD 1 TO WS-Total-Content-1890s 
           When '190' ADD 1 TO WS-Total-Content-1900s 
           When '191' ADD 1 TO WS-Total-Content-1910s 
           When '192' ADD 1 TO WS-Total-Content-1920s 
           When '193' ADD 1 TO WS-Total-Content-1930s 
           When '194' ADD 1 TO WS-Total-Content-1940s 
           When '195' ADD 1 TO WS-Total-Content-1950s 
           When '196' ADD 1 TO WS-Total-Content-1960s 
           When '197' ADD 1 TO WS-Total-Content-1970s 
           When '198' ADD 1 TO WS-Total-Content-1980s 
           When '199' ADD 1 TO WS-Total-Content-1990s 
           When '200' ADD 1 TO WS-Total-Content-2000s 
           When '201' ADD 1 TO WS-Total-Content-2010s 
           When '202' ADD 1 TO WS-Total-Content-2020s 
           End-evaluate.

      *-----------------------------------------------------------------
      * Min Age Calculation
      *-----------------------------------------------------------------

       5110-Min-Age-Calculation.


           MOVE startYear of WS-Title-File TO WS-startYear-TMP 

           IF WS-startYear-TMP < WS-Min-Age-Content
               MOVE WS-startYear-TMP TO WS-Min-Age-Content
           END-IF.

      *-----------------------------------------------------------------
      * Max Age Calculation
      *-----------------------------------------------------------------

       5120-Max-Age-Calculation.

           MOVE startYear of WS-Title-File TO WS-startYear-TMP

           IF WS-startYear-TMP > WS-Max-Age-Content
               MOVE WS-startYear-TMP TO WS-Max-Age-Content
           END-IF.   

      *-----------------------------------------------------------------
      * Total Adult Content
      *-----------------------------------------------------------------

       5130-Total-Adult-Count.
       
           EVALUATE isAdult of WS-Title-File
           WHEN '1' ADD 1 TO WS-Total-Content-Adult
           WHEN '0' ADD 1 TO WS-Total-Content-Non-Adult
           WHEN OTHER 
                  DISPLAY 'Invalid isAdult : ' isAdult of WS-Title-File
           END-EVALUATE.  

      *=================================================================
      * Subscription Subroutines
      *=================================================================

      *-----------------------------------------------------------------
      * Total Subscription Standard Count
      *-----------------------------------------------------------------

       5200-Total-Sub-Standard-Count.
       
           IF subscriptionProgram  = 'standard'
               ADD 1 TO WS-Total-Sub-Program-Std
           END-IF.

      *-----------------------------------------------------------------
      * Total Subscription Extended Count
      *-----------------------------------------------------------------

       5210-Total-Sub-Extended-Count.
       
           IF subscriptionProgram  = 'extended'
               ADD 1 TO WS-Total-Sub-Program-Ext
           END-IF.

      *-----------------------------------------------------------------
      * Total Subscription Premium Count
      *-----------------------------------------------------------------

       5220-Total-Sub-Premium-Count.
       
           IF subscriptionProgram  = 'premium'
               ADD 1 TO WS-Total-Sub-Program-Prm
           END-IF.

      *-----------------------------------------------------------------
      * Max/Min Age Subscription Calculation
      *
      * Note : The following code is not the most efficient way to
      *        calculate the max/min age of customers.  It is
      *        included to demonstrate the use of the EVALUATE
      *        statement.
      *-----------------------------------------------------------------

       5230-Max-Min-Age-Sub-Calc.

           PERFORM 5510-Sub-Age-Calculation
       
           EVALUATE subscriptionProgram
           WHEN 'standard'
                IF WS-Age-Sub-TMP > 
                   WS-Max-Age-Sub-Standard
                   MOVE WS-Age-Sub-TMP
                        TO WS-Max-Age-Sub-Standard
                END-IF
                IF WS-Age-Sub-TMP < 
                   WS-Min-Age-Sub-Standard
                   MOVE WS-Age-Sub-TMP
                        TO WS-Min-Age-Sub-Standard
                END-IF
           WHEN 'extended'
                IF WS-Age-Sub-TMP > 
                   WS-Max-Age-Sub-Extended
                   MOVE WS-Age-Sub-TMP 
                        TO WS-Max-Age-Sub-Extended
                END-IF
                IF WS-Age-Sub-TMP < 
                   WS-Min-Age-Sub-Extended
                   MOVE WS-Age-Sub-TMP
                        TO WS-Min-Age-Sub-Extended
                END-IF
           WHEN 'premium'
                IF WS-Age-Sub-TMP > 
                   WS-Max-Age-Sub-Premium
                   MOVE WS-Age-Sub-TMP 
                        TO WS-Max-Age-Sub-Premium
                END-IF
                IF WS-Age-Sub-TMP < 
                   WS-Min-Age-Sub-Premium
                   MOVE WS-Age-Sub-TMP 
                        TO WS-Min-Age-Sub-Premium
                END-IF
           WHEN OTHER
                DISPLAY 'Invalid subscriptionProgram : ' 
                        subscriptionProgram 
           END-EVALUATE
             
           IF WS-Age-Sub-TMP > 
                WS-Max-Age-Sub-Total
                MOVE WS-Age-Sub-TMP 
                       TO WS-Max-Age-Sub-Total
           END-IF
           IF WS-Age-Sub-TMP < 
                WS-Min-Age-Sub-Total
                MOVE WS-Age-Sub-TMP 
                       TO WS-Min-Age-Sub-Total
           END-IF.

      *-----------------------------------------------------------------
      * Avg Age Subscription Calculation
      *-----------------------------------------------------------------

       5240-Avg-Age-Sub-Calc.
       
           IF subscriptionProgram  = 'standard'
               ADD WS-Age-Sub-TMP TO WS-Sum-Age-Sub-Standard
           END-IF.
           IF subscriptionProgram  = 'extended'
               ADD WS-Age-Sub-TMP TO WS-Sum-Age-Sub-Extended
           END-IF.
           IF subscriptionProgram  = 'premium'
               ADD WS-Age-Sub-TMP TO WS-Sum-Age-Sub-Premium
           END-IF.
           ADD WS-Age-Sub-TMP TO WS-Sum-Age-Sub-Total.

      *=================================================================
      * Customer Subroutines
      *=================================================================

      *-----------------------------------------------------------------
      * Max/Min Age Customer Calculation
      *
      * Note : The following code is not the most efficient way to
      *        calculate the max/min age of customers.  It is
      *        included to demonstrate the use of the EVALUATE
      *        statement.
      *-----------------------------------------------------------------

       5300-Max-Min-Age-Customer-Calc.

           PERFORM 5520-Cust-Age-Calculation
       
           EVALUATE subscriptionProgram
           WHEN 'standard'
                IF WS-Age-Cust-TMP > 
                   WS-Max-Age-Cust-Standard
                   MOVE WS-Age-Cust-TMP
                        TO WS-Max-Age-Cust-Standard
                END-IF
                IF WS-Age-Cust-TMP < 
                   WS-Min-Age-Cust-Standard
                   MOVE WS-Age-Cust-TMP
                        TO WS-Min-Age-Cust-Standard
                END-IF
           WHEN 'extended'
                IF WS-Age-Cust-TMP > 
                   WS-Max-Age-Cust-Extended
                   MOVE WS-Age-Cust-TMP 
                        TO WS-Max-Age-Cust-Extended
                END-IF
                IF WS-Age-Cust-TMP < 
                   WS-Min-Age-Cust-Extended
                   MOVE WS-Age-Cust-TMP
                        TO WS-Min-Age-Cust-Extended
                END-IF
           WHEN 'premium'
                IF WS-Age-Cust-TMP > 
                   WS-Max-Age-Cust-Premium
                   MOVE WS-Age-Cust-TMP 
                        TO WS-Max-Age-Cust-Premium
                END-IF
                IF WS-Age-Cust-TMP < 
                   WS-Min-Age-Cust-Premium
                   MOVE WS-Age-Cust-TMP 
                        TO WS-Min-Age-Cust-Premium
                END-IF
           WHEN OTHER
                DISPLAY 'Invalid subscriptionProgram : ' 
                        subscriptionProgram 
           END-EVALUATE
             
           IF WS-Age-Cust-TMP  > 
                WS-Max-Age-Cust-Total
                MOVE WS-Age-Cust-TMP 
                       TO WS-Max-Age-Cust-Total
           END-IF
           IF WS-Age-Cust-TMP  < 
                WS-Min-Age-Cust-Total
                MOVE WS-Age-Cust-TMP 
                       TO WS-Min-Age-Cust-Total
           END-IF.

      *-----------------------------------------------------------------
      * Avg Age Customer Calculation
      *-----------------------------------------------------------------

       5310-Avg-Age-Customer-Calc.
       
           IF subscriptionProgram  = 'standard'
               ADD WS-Age-Cust-TMP TO WS-Sum-Age-Cust-Standard
           END-IF.
           IF subscriptionProgram  = 'extended'
               ADD WS-Age-Cust-TMP TO WS-Avg-Age-Cust-Extended
           END-IF.
           IF subscriptionProgram  = 'premium'
               ADD WS-Age-Cust-TMP TO WS-Sum-Age-Cust-Premium
           END-IF.
           ADD WS-Age-Cust-TMP TO WS-Sum-Age-Cust-Total.

      *=================================================================
      * Views Subroutines
      *=================================================================     

      *-----------------------------------------------------------------
      * Total Views Web Calculation
      *-----------------------------------------------------------------

       5400-Total-Views-Web-Count.
       
           IF mediaType = 'Web'
               ADD 1 TO WS-Total-Views-Web
           END-IF.

      *-----------------------------------------------------------------
      * Total Views App Calculation
      *-----------------------------------------------------------------

       5410-Total-Views-App-Count.
       
           IF mediaType = 'App'
               ADD 1 TO WS-Total-Views-App
           END-IF.

      *-----------------------------------------------------------------
      * Total Views TV Calculation
      *-----------------------------------------------------------------

       5420-Total-Views-TV-Count.
       
           IF mediaType = 'TV'
               ADD 1 TO WS-Total-Views-TV
           END-IF.  

      *-----------------------------------------------------------------
      * Max/Min Views Content Calculation
      *-----------------------------------------------------------------

       5430-Max-Min-Views-Cont-Calc.
       
           IF contentId  of WS-Title-File = 
              contentId  of WS-Merged-File
              ADD 1 TO WS-Content-Views 
           ELSE  
              IF WS-Content-Views > WS-Max-Views-Content
                 MOVE WS-Content-Views TO WS-Max-Views-Content
              END-IF
              IF WS-Content-Views < WS-Min-Views-Content
                 MOVE WS-Content-Views TO WS-Min-Views-Content
              END-IF
              MOVE 0 TO WS-Content-Views
           END-IF.  

      *-----------------------------------------------------------------
      * Max/Min Views Customer Calculation
      *-----------------------------------------------------------------

       5440-Max-Min-Views-Cust-Calc.
       
           IF WS-acct-Number-TMP = 
              accountNumber of WS-Merged-File
              ADD 1 TO WS-Customer-Views
           ELSE  
              IF WS-Customer-Views > WS-Max-Views-Customer
                 MOVE WS-Customer-Views TO WS-Max-Views-Customer
              END-IF
              IF WS-Customer-Views < WS-Min-Views-Customer
                 MOVE WS-Customer-Views TO WS-Min-Views-Customer
              END-IF
              MOVE 0 TO WS-Customer-Views
           END-IF.   

      *-----------------------------------------------------------------
      * Max/Min View Content Time Calculation
      *-----------------------------------------------------------------

       5450-Max-View-Contt-Time-Calc.
       
           IF contentId  of WS-Title-File = 
              contentId  of WS-Merged-File
              COMPUTE WS-Watch-Time-TMP = 
              WS-watchPercent / 100 * WS-runtimeMinutes
              ADD WS-Watch-Time-TMP TO WS-Total-Content-Watch-Time
           ELSE  
              IF WS-Total-Content-Watch-Time > 
                 WS-Max-View-Content-Time
                 MOVE WS-Total-Content-Watch-Time 
                      TO WS-Max-View-Content-Time
              END-IF
              IF WS-Total-Content-Watch-Time < 
                 WS-Min-View-Content-Time
                 MOVE WS-Total-Content-Watch-Time 
                      TO WS-Min-View-Content-Time
              END-IF
              MOVE 0 TO WS-Total-Content-Watch-Time
           END-IF.           
      
      *-----------------------------------------------------------------
      * Max/Min View Customer Time Calculation
      *-----------------------------------------------------------------

       5460-Max-View-Cust-Time-Calc.
       
           CONTINUE.
      * TODO

      *=================================================================
      * Common Subroutines
      *=================================================================

      *-----------------------------------------------------------------
      * Date and Time Retrieval
      *-----------------------------------------------------------------

       5500-Datetime-Retrieval.
       
           MOVE FUNCTION CURRENT-DATE TO WS-Current-Date  
           .

      *-----------------------------------------------------------------
      * Subscription Age Calculation
      *-----------------------------------------------------------------

       5510-Sub-Age-Calculation.
       
           MOVE subscriptionDate TO WS-Subscription-Date

           COMPUTE WS-Age-Sub-TMP = 
           WS-Current-YYYY - WS-Subscription-Date-YYYY 
           IF WS-Subscription-Date-MM  > WS-Current-MM
            IF WS-Subscription-Date-DD  > WS-Current-DD
                  SUBTRACT 1 FROM WS-Age-Sub-TMP
            END-IF
           END-IF.

      *-----------------------------------------------------------------
      * Customer Age Calculation
      *-----------------------------------------------------------------

       5520-Cust-Age-Calculation.
       
           MOVE birthDate TO WS-BirthDate

           COMPUTE WS-Age-Cust-TMP = 
           WS-Current-YYYY - WS-BirthDate-YYYY 
           IF WS-BirthDate-MM  > WS-Current-MM
            IF WS-BirthDate-DD  > WS-Current-DD
                  SUBTRACT 1 FROM WS-Age-Cust-TMP
            END-IF
           END-IF.

      *-----------------------------------------------------------------
      * Avg Calculation
      *
      * Note : 
      *        The average is calculated by dividing the total by the
      *        count.  The count is incremented each time a record is
      *        processed.  The total is incremented each time a record
      *        is processed.  The total is divided by the count to
      *        calculate the average.
      *     
      *-----------------------------------------------------------------

       5530-Avg-Calculation.
       
           IF WS-Total-Sub-Program-Std NOT = 0
                COMPUTE WS-Avg-Age-Sub-Standard = 
                WS-Sum-Age-Sub-Standard / WS-Total-Sub-Program-Std
           END-IF.
           IF WS-Total-Sub-Program-Ext NOT = 0
                COMPUTE WS-Avg-Age-Sub-Extended = 
                WS-Sum-Age-Sub-Extended / WS-Total-Sub-Program-Ext
           END-IF.
           IF WS-Total-Sub-Program-Prm NOT = 0
                COMPUTE WS-Avg-Age-Sub-Premium = 
                WS-Sum-Age-Sub-Premium / WS-Total-Sub-Program-Prm
           END-IF.
           IF WS-Total-Subscription NOT = 0
                COMPUTE WS-Avg-Age-Sub-Total = 
                WS-Sum-Age-Sub-Total / WS-Total-Subscription
           END-IF.
           IF WS-Total-Sub-Program-Std NOT = 0
                COMPUTE WS-Avg-Age-Cust-Standard = 
                WS-Sum-Age-Cust-Standard / WS-Total-Sub-Program-Std
           END-IF.
           IF WS-Total-Sub-Program-Ext NOT = 0
                COMPUTE WS-Avg-Age-Cust-Extended = 
                WS-Sum-Age-Cust-Extended / WS-Total-Sub-Program-Ext
           END-IF.
           IF WS-Total-Sub-Program-Prm NOT = 0
                COMPUTE WS-Avg-Age-Cust-Premium = 
                WS-Sum-Age-Cust-Premium / WS-Total-Sub-Program-Prm
           END-IF.
           IF WS-Total-Subscription NOT = 0
                COMPUTE WS-Avg-Age-Cust-Total = 
                WS-Sum-Age-Cust-Total / WS-Total-Subscription
           END-IF.
           IF WS-Total-Content NOT = 0
                COMPUTE WS-Avg-Views-Content = 
                WS-Sum-Views-Content / WS-Total-Content
           END-IF.
           IF WS-Total-Viewer NOT = 0
                COMPUTE WS-Avg-Views-Customer = 
                WS-Sum-Views-Customer / WS-Total-Viewer
           END-IF.
           IF WS-Total-Content NOT = 0
                COMPUTE WS-Avg-View-Content-Time = 
                WS-Sum-View-Content-Time / WS-Total-Content
           END-IF.
           IF WS-Total-Viewer NOT = 0
                COMPUTE WS-Avg-View-Customer-Time = 
                WS-Sum-View-Customer-Time / WS-Total-Viewer
           END-IF.


      *-----------------------------------------------------------------
      * Moving Working Variables to Statistics File
      *-----------------------------------------------------------------

       6000-Move-Working-Variables.
      
      * Date
           MOVE WS-Current-YYYY TO Date-of-Run-YYYY
           MOVE WS-Current-MM TO Date-of-Run-MM
           MOVE WS-Current-DD TO Date-of-Run-DD
      * Time
           MOVE WS-Current-HH TO Time-of-Run-HH
           MOVE WS-Current-MIN TO Time-of-Run-MIN
           MOVE WS-Current-SS TO Time-of-Run-SS
      * Total Content
           MOVE WS-Total-Content TO Total-Content
      * Total Subscription
           MOVE WS-Total-Subscription TO Total-Subscription
      * Total Viewer
           MOVE WS-Total-Viewer TO Total-Viewer
      * Total Views
           MOVE WS-Total-Views TO Total-Views
      * Total Watch Time
           MOVE WS-Total-Watch-Time TO Total-Watch-Time
      * Total Views Web
           MOVE WS-Total-Views-Web TO Total-Views-Web
      * Total Views App
           MOVE WS-Total-Views-App TO Total-Views-App
      * Total Views TV
           MOVE WS-Total-Views-TV TO Total-Views-TV
      * Total Content 1880s
           MOVE WS-Total-Content-1880s TO Total-Content-1880s
      * Total Content 1890s
           MOVE WS-Total-Content-1890s TO Total-Content-1890s
      * Total Content 1900s
           MOVE WS-Total-Content-1900s TO Total-Content-1900s
      * Total Content 1910s
           MOVE WS-Total-Content-1910s TO Total-Content-1910s
      * Total Content 1920s
           MOVE WS-Total-Content-1920s TO Total-Content-1920s
      * Total Content 1930s
           MOVE WS-Total-Content-1930s TO Total-Content-1930s
      * Total Content 1940s
           MOVE WS-Total-Content-1940s TO Total-Content-1940s
      * Total Content 1950s
           MOVE WS-Total-Content-1950s TO Total-Content-1950s
      * Total Content 1960s
           MOVE WS-Total-Content-1960s TO Total-Content-1960s
      * Total Content 1970s
           MOVE WS-Total-Content-1970s TO Total-Content-1970s
      * Total Content 1980s
           MOVE WS-Total-Content-1980s TO Total-Content-1980s
      * Total Content 1990s
           MOVE WS-Total-Content-1990s TO Total-Content-1990s
      * Total Content 2000s
           MOVE WS-Total-Content-2000s TO Total-Content-2000s
      * Total Content 2010s
           MOVE WS-Total-Content-2010s TO Total-Content-2010s
      * Total Content 2020s
           MOVE WS-Total-Content-2020s TO Total-Content-2020s
      * Total Content Adult
           MOVE WS-Total-Content-Adult TO Total-Content-Adult
      * Total Content Non-Adult
           MOVE WS-Total-Content-Non-Adult TO Total-Content-Non-Adult
      * Total Subscription Standard
           MOVE WS-Total-Sub-Program-Std TO Total-Sub-Program-Stantard 
      * Total Subscription Extended
           MOVE WS-Total-Sub-Program-Ext TO Total-Sub-Program-Extended
      * Total Subscription Premium
           MOVE WS-Total-Sub-Program-Prm TO Total-Sub-Program-Premium
      * Min Age Content
           MOVE WS-Min-Age-Content TO Min-Age-Content
      * Max Age Content
           MOVE WS-Max-Age-Content TO Max-Age-Content
      * Min Age Customer Standard
           MOVE WS-Min-Age-Cust-Standard TO Min-Age-Cust-Standard
      * Max Age Customer Standard
           MOVE WS-Max-Age-Cust-Standard TO Max-Age-Cust-Standard
      * Avg Age Customer Standard   
           MOVE WS-Avg-Age-Cust-Standard TO Avg-Age-Cust-Standard
      * Min Age Customer Extended
           MOVE WS-Min-Age-Cust-Extended TO Min-Age-Cust-Extended
      * Max Age Customer Extended
           MOVE WS-Max-Age-Cust-Extended TO Max-Age-Cust-Extended
      * Avg Age Customer Extended
           MOVE WS-Avg-Age-Cust-Extended TO Avg-Age-Cust-Extended
      * Min Age Customer Premium
           MOVE WS-Min-Age-Cust-Premium TO Min-Age-Cust-Premium
      * Max Age Customer Premium
           MOVE WS-Max-Age-Cust-Premium TO Max-Age-Cust-Premium
      * Avg Age Customer Premium
           MOVE WS-Avg-Age-Cust-Premium TO Avg-Age-Cust-Premium
      * Min Age Customer Total
           MOVE WS-Min-Age-Cust-Total TO Min-Age-Cust-Total 
      * Max Age Customer Total
           MOVE WS-Max-Age-Cust-Total TO Max-Age-Cust-Total
      * Avg Age Customer Total
           MOVE WS-Avg-Age-Cust-Total TO Avg-Age-Cust-Total
      * Min Age Subscription Standard
           MOVE WS-Min-Age-Sub-Standard TO Min-Age-Sub-Standard
      * Max Age Subscription Standard
           MOVE WS-Max-Age-Sub-Standard TO Max-Age-Sub-Standard
      * Avg Age Subscription Standard
           MOVE WS-Avg-Age-Sub-Standard TO Avg-Age-Sub-Standard
      * Min Age Subscription Extended
           MOVE WS-Min-Age-Sub-Extended TO Min-Age-Sub-Extended
      * Max Age Subscription Extended
           MOVE WS-Max-Age-Sub-Extended TO Max-Age-Sub-Extended
      * Avg Age Subscription Extended
           MOVE WS-Avg-Age-Sub-Extended TO Avg-Age-Sub-Extended
      * Min Age Subscription Premium
           MOVE WS-Min-Age-Sub-Premium TO Min-Age-Sub-Premium
      * Max Age Subscription Premium
           MOVE WS-Max-Age-Sub-Premium TO Max-Age-Sub-Premium
      * Avg Age Subscription Premium
           MOVE WS-Avg-Age-Sub-Premium TO Avg-Age-Sub-Premium
      * Min Age Subscription Total
           MOVE WS-Min-Age-Sub-Total TO Min-Age-Sub-Total
      * Max Age Subscription Total
           MOVE WS-Max-Age-Sub-Total TO Max-Age-Sub-Total
      * Avg Age Subscription Total
           MOVE WS-Avg-Age-Sub-Total TO Avg-Age-Sub-Total
      * Max Views Content
           MOVE WS-Max-Views-Content TO Max-Views-Content
      * Min Views Content
           MOVE WS-Min-Views-Content TO Min-Views-Content
      * Avg Views Content
           MOVE WS-Avg-Views-Content TO Avg-Views-Content
      * Max Views Customer
           MOVE WS-Max-Views-Customer TO Max-Views-Customer
      * Min Views Customer
           MOVE WS-Min-Views-Customer TO Min-Views-Customer
      * Avg Views Customer
           MOVE WS-Avg-Views-Customer TO Avg-Views-Customer
      * Max View Content Time
           MOVE WS-Max-View-Content-Time TO Max-View-Content-Time
      * Min View Content Time
           MOVE WS-Min-View-Content-Time TO Min-View-Content-Time
      * Avg View Content Time
           MOVE WS-Avg-View-Content-Time TO Avg-View-Content-Time
      * Max View Customer Time
           MOVE WS-Max-View-Customer-Time TO Max-View-Customer-Time
      * Min View Customer Time
           MOVE WS-Min-View-Customer-Time TO Min-View-Customer-Time
      * Avg View Customer Time
           MOVE WS-Avg-View-Customer-Time TO Avg-View-Customer-Time
           .

      *-----------------------------------------------------------------
      * Writing Statistics Report
      *-----------------------------------------------------------------

       6100-Write-Statistics-Report.

      * Time-Line
           MOVE Time-Line TO WS-All-Stat-File
           PERFORM 9320-Write-All-Record
      * Blank Line
           MOVE Blank-Line TO WS-All-Stat-File
           PERFORM 9320-Write-All-Record
      * Blank Line
           MOVE Blank-Line TO WS-All-Stat-File
           PERFORM 9320-Write-All-Record
      * Dash Line
           MOVE Dash-Line TO WS-All-Stat-File
           PERFORM 9320-Write-All-Record
      * Blank Line
           MOVE Blank-Line TO WS-All-Stat-File
           PERFORM 9320-Write-All-Record                 
      * Header
           MOVE Header-Line TO WS-All-Stat-File
           PERFORM 9320-Write-All-Record
      * Blank Line
           MOVE Blank-Line TO WS-All-Stat-File
           PERFORM 9320-Write-All-Record      
      *Dash Line
           MOVE Dash-Line TO WS-All-Stat-File
           PERFORM 9320-Write-All-Record
      * Blank Line
           MOVE Blank-Line TO WS-All-Stat-File
           PERFORM 9320-Write-All-Record
      * Blank Line
           MOVE Blank-Line TO WS-All-Stat-File
           PERFORM 9320-Write-All-Record
      * Dash Line
           MOVE Dash-Line TO WS-All-Stat-File
           PERFORM 9320-Write-All-Record
      * Header General
           MOVE Header-General-Line TO WS-All-Stat-File
           PERFORM 9320-Write-All-Record
      * Dash Line
           MOVE Dash-Line TO WS-All-Stat-File
           PERFORM 9320-Write-All-Record
      * Blank Line
           MOVE Blank-Line TO WS-All-Stat-File
           PERFORM 9320-Write-All-Record
      * Total Content
           MOVE Total-Content-Line TO WS-All-Stat-File
           PERFORM 9320-Write-All-Record    
      * Total Subscription
           MOVE Total-Subscription-Line TO WS-All-Stat-File
           PERFORM 9320-Write-All-Record 
      * Total Viewer
           MOVE Total-Viewer-Line TO WS-All-Stat-File
           PERFORM 9320-Write-All-Record
      * Total Views
           MOVE Total-Views-Line TO WS-All-Stat-File      
           PERFORM 9320-Write-All-Record
      * Total Watch Time
           MOVE Total-Watch-Time-Line TO WS-All-Stat-File
           PERFORM 9320-Write-All-Record  
      * Blank Line
           MOVE Blank-Line TO WS-All-Stat-File
           PERFORM 9320-Write-All-Record
      * Dash Line
           MOVE Dash-Line TO WS-All-Stat-File
           PERFORM 9320-Write-All-Record
      * Header Content
           MOVE Header-Content-Line TO WS-All-Stat-File
           PERFORM 9320-Write-All-Record
      * Dash Line
           MOVE Dash-Line TO WS-All-Stat-File
           PERFORM 9320-Write-All-Record
      * Blank Line
           MOVE Blank-Line TO WS-All-Stat-File
           PERFORM 9320-Write-All-Record
      * Total Content 1880s
           MOVE Total-Content-1880s-Line TO WS-All-Stat-File
           PERFORM 9320-Write-All-Record
      * Total Content 1890s
           MOVE Total-Content-1890s-Line TO WS-All-Stat-File
           PERFORM 9320-Write-All-Record
      * Total Content 1900s
           MOVE Total-Content-1900s-Line TO WS-All-Stat-File
           PERFORM 9320-Write-All-Record
      * Total Content 1910s
           MOVE Total-Content-1910s-Line TO WS-All-Stat-File
           PERFORM 9320-Write-All-Record
      * Total Content 1920s
           MOVE Total-Content-1920s-Line TO WS-All-Stat-File
           PERFORM 9320-Write-All-Record
      * Total Content 1930s
           MOVE Total-Content-1930s-Line TO WS-All-Stat-File
           PERFORM 9320-Write-All-Record
      * Total Content 1940s
           MOVE Total-Content-1940s-Line TO WS-All-Stat-File
           PERFORM 9320-Write-All-Record
      * Total Content 1950s
           MOVE Total-Content-1950s-Line TO WS-All-Stat-File
           PERFORM 9320-Write-All-Record
      * Total Content 1960s
           MOVE Total-Content-1960s-Line TO WS-All-Stat-File
           PERFORM 9320-Write-All-Record
      * Total Content 1970s
           MOVE Total-Content-1970s-Line TO WS-All-Stat-File
           PERFORM 9320-Write-All-Record
      * Total Content 1980s
           MOVE Total-Content-1980s-Line TO WS-All-Stat-File
           PERFORM 9320-Write-All-Record
      * Total Content 1990s
           MOVE Total-Content-1990s-Line TO WS-All-Stat-File
           PERFORM 9320-Write-All-Record
      * Total Content 2000s
           MOVE Total-Content-2000s-Line TO WS-All-Stat-File
           PERFORM 9320-Write-All-Record
      * Total Content 2010s
           MOVE Total-Content-2010s-Line TO WS-All-Stat-File
           PERFORM 9320-Write-All-Record
      * Total Content 2020s
           MOVE Total-Content-2020s-Line TO WS-All-Stat-File
           PERFORM 9320-Write-All-Record
      * Total Content non-Adult
           MOVE Total-Content-Non-Adult-Line TO WS-All-Stat-File
           PERFORM 9320-Write-All-Record
      * Total Content Adult
           MOVE Total-Content-Adult-Line TO WS-All-Stat-File
           PERFORM 9320-Write-All-Record
      * Min Age Content
           MOVE Min-Age-Content-Line TO WS-All-Stat-File
           PERFORM 9320-Write-All-Record
      * Max Age Content
           MOVE Max-Age-Content-Line TO WS-All-Stat-File
           PERFORM 9320-Write-All-Record
      * Blank Line
           MOVE Blank-Line TO WS-All-Stat-File
           PERFORM 9320-Write-All-Record
      * Dash Line
           MOVE Dash-Line TO WS-All-Stat-File
           PERFORM 9320-Write-All-Record
      * Header Subscription
           MOVE Header-Subscription-Line TO WS-All-Stat-File
           PERFORM 9320-Write-All-Record    
      * Dash Line
           MOVE Dash-Line TO WS-All-Stat-File
           PERFORM 9320-Write-All-Record   
      * Blank Line
           MOVE Blank-Line TO WS-All-Stat-File
           PERFORM 9320-Write-All-Record
      * Total Subscription Standard
           MOVE Total-Sub-Program-Std-Line TO WS-All-Stat-File
           PERFORM 9320-Write-All-Record
      * Total Subscription Extended
           MOVE Total-Sub-Program-Ext-Line TO WS-All-Stat-File
           PERFORM 9320-Write-All-Record
      * Total Subscription Premium
           MOVE Total-Sub-Program-Prm-Line TO WS-All-Stat-File
           PERFORM 9320-Write-All-Record
      * Min Age Subscription Standard
           MOVE Min-Age-Sub-Standard-Line TO WS-All-Stat-File
           PERFORM 9320-Write-All-Record
      * Avg Age Subscription Standard
           MOVE Avg-Age-Sub-Standard-Line TO WS-All-Stat-File
           PERFORM 9320-Write-All-Record
      * Max Age Subscription Standard
           MOVE Max-Age-Sub-Standard-Line TO WS-All-Stat-File
           PERFORM 9320-Write-All-Record
      * Min Age Subscription Extended
           MOVE Min-Age-Sub-Extended-Line TO WS-All-Stat-File
           PERFORM 9320-Write-All-Record
      * Avg Age Subscription Extended
           MOVE Avg-Age-Sub-Extended-Line TO WS-All-Stat-File
           PERFORM 9320-Write-All-Record
      * Max Age Subscription Extended
           MOVE Max-Age-Sub-Extended-Line TO WS-All-Stat-File
           PERFORM 9320-Write-All-Record
      * Min Age Subscription Premium
           MOVE Min-Age-Sub-Premium-Line TO WS-All-Stat-File
           PERFORM 9320-Write-All-Record
      * Avg Age Subscription Premium
           MOVE Avg-Age-Sub-Premium-Line TO WS-All-Stat-File
           PERFORM 9320-Write-All-Record
      * Max Age Subscription Premium
           MOVE Max-Age-Sub-Premium-Line TO WS-All-Stat-File
           PERFORM 9320-Write-All-Record
      * Min Age Subscription Total
           MOVE Min-Age-Sub-Total-Line TO WS-All-Stat-File
           PERFORM 9320-Write-All-Record
      * Avg Age Subscription Total
           MOVE Avg-Age-Sub-Total-Line TO WS-All-Stat-File
           PERFORM 9320-Write-All-Record
      * Max Age Subscription Total
           MOVE Max-Age-Sub-Total-Line TO WS-All-Stat-File
           PERFORM 9320-Write-All-Record
      * Blank Line
           MOVE Blank-Line TO WS-All-Stat-File
           PERFORM 9320-Write-All-Record
      * Dash Line
           MOVE Dash-Line TO WS-All-Stat-File
           PERFORM 9320-Write-All-Record
      * Header Customer
           MOVE Header-Customer-Line TO WS-All-Stat-File
           PERFORM 9320-Write-All-Record
      * Dash Line
           MOVE Dash-Line TO WS-All-Stat-File
           PERFORM 9320-Write-All-Record
      * Blank Line
           MOVE Blank-Line TO WS-All-Stat-File
           PERFORM 9320-Write-All-Record
      * Min Age Customer Standard
           MOVE Min-Age-Cust-Standard-Line TO WS-All-Stat-File
           PERFORM 9320-Write-All-Record
      * Avg Age Customer Standard
           MOVE Avg-Age-Cust-Standard-Line TO WS-All-Stat-File
           PERFORM 9320-Write-All-Record
      * Max Age Customer Standard
           MOVE Max-Age-Cust-Standard-Line TO WS-All-Stat-File
           PERFORM 9320-Write-All-Record
      * Min Age Customer Extended
           MOVE Min-Age-Cust-Extended-Line TO WS-All-Stat-File
           PERFORM 9320-Write-All-Record
      * Avg Age Customer Extended   
           MOVE Avg-Age-Cust-Extended-Line TO WS-All-Stat-File
           PERFORM 9320-Write-All-Record
      * Max Age Customer Extended
           MOVE Max-Age-Cust-Extended-Line TO WS-All-Stat-File
           PERFORM 9320-Write-All-Record
      * Min Age Customer Premium
           MOVE Min-Age-Cust-Premium-Line TO WS-All-Stat-File
           PERFORM 9320-Write-All-Record
      * Avg Age Customer Premium
           MOVE Avg-Age-Cust-Premium-Line TO WS-All-Stat-File
           PERFORM 9320-Write-All-Record
      * Max Age Customer Premium
           MOVE Max-Age-Cust-Premium-Line TO WS-All-Stat-File
           PERFORM 9320-Write-All-Record
      * Min Age Customer Total
           MOVE Min-Age-Cust-Total-Line TO WS-All-Stat-File
           PERFORM 9320-Write-All-Record
      * Avg Age Customer Total
           MOVE Avg-Age-Cust-Total-Line TO WS-All-Stat-File
           PERFORM 9320-Write-All-Record
      * Max Age Customer Total
           MOVE Max-Age-Cust-Total-Line TO WS-All-Stat-File
           PERFORM 9320-Write-All-Record
      * Blank Line
           MOVE Blank-Line TO WS-All-Stat-File
           PERFORM 9320-Write-All-Record
      * Dash Line
           MOVE Dash-Line TO WS-All-Stat-File
           PERFORM 9320-Write-All-Record
      * Header Views
           MOVE Header-Views-Line TO WS-All-Stat-File
           PERFORM 9320-Write-All-Record 
      * Dash Line
           MOVE Dash-Line TO WS-All-Stat-File
           PERFORM 9320-Write-All-Record
      * Blank Line
           MOVE Blank-Line TO WS-All-Stat-File
           PERFORM 9320-Write-All-Record
      * Total Views Web 
           MOVE Total-Views-Web-Line TO WS-All-Stat-File
           PERFORM 9320-Write-All-Record
      * Total Views App
           MOVE Total-Views-App-Line TO WS-All-Stat-File
           PERFORM 9320-Write-All-Record
      * Total Views TV
           MOVE Total-Views-TV-Line TO WS-All-Stat-File
           PERFORM 9320-Write-All-Record
      * Min Views Content
           MOVE Min-Views-Content-Line TO WS-All-Stat-File
           PERFORM 9320-Write-All-Record
      * Avg Views Content
           MOVE Avg-Views-Content-Line TO WS-All-Stat-File
           PERFORM 9320-Write-All-Record
      * Max Views Content
           MOVE Max-Views-Content-Line TO WS-All-Stat-File
           PERFORM 9320-Write-All-Record
      * Min Views Customer
           MOVE Min-Views-Customer-Line TO WS-All-Stat-File
           PERFORM 9320-Write-All-Record
      * Avg Views Customer
           MOVE Avg-Views-Customer-Line TO WS-All-Stat-File
           PERFORM 9320-Write-All-Record
      * Max Views Customer
           MOVE Max-Views-Customer-Line TO WS-All-Stat-File
           PERFORM 9320-Write-All-Record
      * Min View Content Time
           MOVE Min-View-Content-Time-Line TO WS-All-Stat-File
           PERFORM 9320-Write-All-Record     
      * Avg View Content Time
           MOVE Avg-View-Content-Time-Line TO WS-All-Stat-File
           PERFORM 9320-Write-All-Record
      * Max View Content Time
           MOVE Max-View-Content-Time-Line TO WS-All-Stat-File
           PERFORM 9320-Write-All-Record
      * Min View Customer Time
           MOVE Min-View-Customer-Time-Line TO WS-All-Stat-File
           PERFORM 9320-Write-All-Record
      * Avg View Customer Time
           MOVE Avg-View-Customer-Time-Line TO WS-All-Stat-File
           PERFORM 9320-Write-All-Record
      * Max View Customer Time
           MOVE Max-View-Customer-Time-Line TO WS-All-Stat-File
           PERFORM 9320-Write-All-Record
      * Blank Line
           MOVE Blank-Line TO WS-All-Stat-File
           PERFORM 9320-Write-All-Record
      * Dash Line
           MOVE Dash-Line TO WS-All-Stat-File
           PERFORM 9320-Write-All-Record
           .    
           
      *-----------------------------------------------------------------
      * Initialization
      *-----------------------------------------------------------------

       9000-Initialization.

           MOVE SPACES TO WS-Merged-File
           INITIALIZE WS-Title-File 
           MOVE SPACES TO WS-Customer-Stat-File 
           MOVE SPACES TO WS-Title-Stat-File
           PERFORM 5500-Datetime-Retrieval.
 
      *-----------------------------------------------------------------
      * Open Files
      *-----------------------------------------------------------------

       9100-Open-Files.

           OPEN INPUT MERGED-FILE
           IF STATUS-MERGED-FILE NOT = '00'
               DISPLAY 'Error while opening MERGED-FILE - FS : ' 
                          STATUS-MERGED-FILE
           END-IF

           OPEN INPUT TITLE-FILE
           IF STATUS-TITLE-FILE NOT = '00'
               DISPLAY 'Error while opening TITLE-FILE - FS : ' 
                          STATUS-TITLE-FILE
           END-IF

           OPEN OUTPUT CSTSTAT-FILE
           IF STATUS-CSTSTAT-FILE NOT = '00'
               DISPLAY 'Error while opening CSTSTAT-FILE - FS : ' 
                          STATUS-CSTSTAT-FILE
           END-IF

           OPEN OUTPUT TTLSTAT-FILE
           IF STATUS-TTLSTAT-FILE NOT = '00'
               DISPLAY 'Error while opening TTLSTAT-FILE - FS : ' 
                          STATUS-TTLSTAT-FILE
           END-IF

           OPEN OUTPUT ALLSTAT-FILE
           IF STATUS-ALLSTAT-FILE NOT = '00'
               DISPLAY 'Error while opening ALLSTAT-FILE - FS : ' 
                          STATUS-ALLSTAT-FILE
           END-IF.

      *-----------------------------------------------------------------
      * Process Merged Record
      *-----------------------------------------------------------------

       9200-Read-Merged-Record.
           
           IF End-Of-File-M = 'N'
               READ MERGED-FILE
                   AT END MOVE 'Y' TO End-Of-File-M
                   DISPLAY 'End of MERGED-FILE'
                   MOVE HIGH-VALUES TO MERGED-RECORD 
               END-READ  
                   MOVE MERGED-RECORD TO WS-Merged-File 
               IF WS-Merged-File NOT = HIGH-VALUES 
                   MOVE watchPercent TO WS-watchPercent
                   PERFORM 5030-Total-Views-Count
                   PERFORM 5040-Total-Watch-Time-Count
                   PERFORM 5400-Total-Views-Web-Count
                   PERFORM 5410-Total-Views-App-Count
                   PERFORM 5420-Total-Views-TV-Count
               END-IF
               IF STATUS-MERGED-FILE NOT = '00' AND '10'
                   DISPLAY 'Error while reading Merged-File - FS : ' 
                              STATUS-MERGED-FILE 
               END-IF
           END-IF.
      
      *-----------------------------------------------------------------
      * Process Title Record
      *-----------------------------------------------------------------

       9210-Read-Title-Record.
           
           IF End-Of-File-T = 'N'
               READ TITLE-FILE
                   AT END MOVE 'Y' TO End-Of-File-T
                   DISPLAY 'End of TITLE-FILE'
                   MOVE HIGH-VALUES TO TITLE-RECORD 
               END-READ
               MOVE TITLE-RECORD TO WS-Title-File 
               IF WS-Title-File NOT = HIGH-VALUES 
                    MOVE runtimeMinutes TO WS-runtimeMinutes
                    PERFORM 5010-Total-Content-Count
                    PERFORM 5100-Decade-Count
                    PERFORM 5110-Min-Age-Calculation
                    PERFORM 5120-Max-Age-Calculation
                    PERFORM 5130-Total-Adult-Count
               END-IF 
               IF STATUS-TITLE-FILE  NOT = '00' AND '10'
                   DISPLAY 'Error while reading Title-File - FS : ' 
                              STATUS-TITLE-FILE 
               END-IF
           END-IF.

      *-----------------------------------------------------------------
      * Write Costumer Statistics
      *-----------------------------------------------------------------

        9300-Write-Costumer-Record.
           
           IF STATUS-CSTSTAT-FILE  = '00'
               WRITE CSTSTAT-RECORD FROM WS-Customer-Stat-File
                   INVALID KEY
                   DISPLAY 'Error while writing to CSTSTAT-FILE - FS : '
                          STATUS-CSTSTAT-FILE 
               END-WRITE
           END-IF.

      *-----------------------------------------------------------------
      * Write Title Statistics
      *-----------------------------------------------------------------

        9310-Write-Title-Record.
           
           IF STATUS-TTLSTAT-FILE  = '00'
               WRITE TTLSTAT-RECORD FROM WS-Title-Stat-File
                   INVALID KEY
                   DISPLAY 'Error while writing to TTLSTAT-FILE - FS : '
                          STATUS-TTLSTAT-FILE 
               END-WRITE
           END-IF.

      *-----------------------------------------------------------------
      * Write All Statistics
      *-----------------------------------------------------------------

        9320-Write-All-Record.
           
           IF STATUS-ALLSTAT-FILE  = '00'
               WRITE ALLSTAT-RECORD FROM WS-All-Stat-File
                   INVALID KEY
                   DISPLAY 'Error while writing to ALLSTAT-FILE - FS : '
                          STATUS-ALLSTAT-FILE 
               END-WRITE
           END-IF.

      *-----------------------------------------------------------------
      * Close Files
      *-----------------------------------------------------------------

       9900-Close-Files.
           
           CLOSE MERGED-FILE
           IF STATUS-MERGED-FILE NOT = '00'
               DISPLAY 'Error while closing MERGED-FILE - FS : ' 
                          STATUS-MERGED-FILE
           END-IF.

           CLOSE TITLE-FILE
           IF STATUS-TITLE-FILE NOT = '00'
             DISPLAY 'Error while closing TITLE-FILE - FS : ' 
                           STATUS-TITLE-FILE
           END-IF.

           CLOSE CSTSTAT-FILE
           IF STATUS-CSTSTAT-FILE NOT = '00'
             DISPLAY 'Error while closing CSTSTAT-FILE - FS : ' 
                           STATUS-CSTSTAT-FILE
           END-IF.

           CLOSE TTLSTAT-FILE
           IF STATUS-TTLSTAT-FILE NOT = '00'
                 DISPLAY 'Error while closing TTLSTAT-FILE - FS : ' 
                              STATUS-TTLSTAT-FILE
           END-IF.

           CLOSE ALLSTAT-FILE
           IF STATUS-ALLSTAT-FILE NOT = '00'
               DISPLAY 'Error while closing ALLSTAT-FILE - FS : ' 
                             STATUS-ALLSTAT-FILE
           END-IF.

      *-----------------------------------------------------------------
      * Finalize
      *-----------------------------------------------------------------

       9999-Finalize.

      * End of Program     
              STOP RUN.
