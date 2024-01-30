        05 contentId                   PIC X(10).
        05 titleType                   PIC X(15).
        05 primaryTitle                PIC X(100).
        05 originalTitle               PIC X(100).
        05 isAdult                     PIC X(1).
        05 startYear                   PIC 9(4).
        05 endYear                     PIC 9(4).
        05 runtimeMinutes              PIC 9(5).
        05 genres.
         10 genre                    PIC X(15) OCCURRS 3 TIMES.

        CREATE TABLE TableName (
            (  "content_id" CHAR(10) Not Null,
              "title_type" VARCHAR(15) Null by Default,
              "primary_title" VARCHAR(100) Null by Default,
              "original_title" VARCHAR(100) Null by Default,
              "is_adult" CHAR(1) Null by Default,
              "start_year" INTEGER Null by Default,
              "end_year" INTEGER Null by Default,
              "runtime_minutes" INTEGER Null by Default,
              "genre1" VARCHAR(15) Null by Default,
              "genre2" VARCHAR(15) Null by Default,
              "genre3" VARCHAR(15) Null by Default,
              PRIMARY KEY ("content_id")
            )
        )

         CREATE TABLE "SALESSUP"."AIF_CATALOG"                          
         (  "CONTENT_ID" CHAR(10) NOT NULL,                    
            "TITLE_TYPE" VARCHAR(15) WITH DEFAULT NULL,          
            "PRIMARY_TITLE" VARCHAR(100) WITH DEFAULT NULL,     
            "ORIGINAL_TITLE" VARCHAR(100) WITH DEFAULT NULL, 
            "IS_ADULT" CHAR(1) WITH DEFAULT NULL,      
            "START_YEAR" DECIMAL(4,0) WITH DEFAULT NULL,       
            "END_YEAR" DECIMAL(4,0) WITH DEFAULT NULL,          
            "RUNTIME_MINUTES" DECIMAL(5,0) WITH DEFAULT NULL,      
            "GENRE1" VARCHAR(15) WITH DEFAULT NULL,             
            "GENRE2" VARCHAR(15) WITH DEFAULT NULL,          
            "GENRE3" VARCHAR(15) WITH DEFAULT NULL,             
            PRIMARY KEY ("CONTENT_ID")                         
          )                                                    
  IN "FA2301SM"."CONTTS"                                    
  AUDIT NONE;        

  CREATE UNIQUE INDEX "SALESSUP"."CONT_ID_IDX"           
  ON "SALESSUP"."AIF_CATALOG"                      
    ("CONTENT_ID"     ASC                          
    )                                              
  USING  STOGROUP "STG011" PRIQTY 12 SECQTY 12     
  FREEPAGE 0 PCTFREE 10                            
  CLUSTER                                          
  BUFFERPOOL BP0                                   
  CLOSE YES COPY NO;      


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

        CREATE TABLE AIF_ACCOUNT (
          account_Number CHAR(10) NOT NULL,
          first_Name VARCHAR(15) WITH DEFAULT NULL, 
          last_Name VARCHAR(15) WITH DEFAULT NULL, 
          gender VARCHAR(10) WITH DEFAULT NULL, 
          birth_Date VARCHAR(10) WITH DEFAULT NULL, 
          subscription_Date VARCHAR(10) WITH DEFAULT NULL, 
          email_Address VARCHAR(40) WITH DEFAULT NULL, 
          subscription_Program VARCHAR(10) WITH DEFAULT NULL
        );

  