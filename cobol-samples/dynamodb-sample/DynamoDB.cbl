       IDENTIFICATION DIVISION.
       PROGRAM-ID. DynamoDB.
      ******************************************************************
      *  BLU AGE SERVERLESS COBOL - AWS DYNAMODB EXTENSION - SAMPLE 
      *  PROGRAM OBJECT: 
      *  DEMONSTRATING DYNAMODB ATOMIC DATA OPERATIONS:
      *  - STORE DATA
      *  - UPDATE DATA
      *  - READ DATA
      *  - REMOVE DATA
      *  (C) BLU AGE 2019 - ALL RIGHTS RESERVED
      ******************************************************************  
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.

       01  in-rec-as-str1           PIC X(15).
       01  in-rec-as-str2           PIC X(15).
       01  in-rec-as-str3           PIC X(15).

       01 dynamoDB-op-result  PIC 9(2) BINARY.

       01 dynamoDB-op-res-displ.
          05 dynamodb-op-err-msg PIC X(23)
             VALUE "ERROR ON DYNAMODB OPERATION: ".
          05 dynamodb-op-resultAsStr PIC X(2).

       01  dynamodb-data PIC X(70).
       .
       
       COPY DynamoDBRQA

       Linkage Section.
       01  in-rec1                Pic S9(15)      Packed-Decimal.
       01  in-rec2                Pic S9(15)      Packed-Decimal.
       01  in-rec3                Pic S9(15)      Packed-Decimal.

       PROCEDURE DIVISION using in-rec1, in-rec2, in-rec3.

       Main.
           Move in-rec1 to in-rec-as-str1
           Move in-rec2 to in-rec-as-str2
           Move in-rec3 to in-rec-as-str3
           Move "eu-central-1" to aws-region

      * STORE #1
           Set store-command to True
           Move "telcoatom" to table-name
           Set number-key-type to True 
           Set string-record-type to True
           Move "toto" to dynamoDB-data
           Call "DYNAMODBOP" Using dynamoDB-request-area in-rec-as-str1
                dynamoDB-data dynamoDB-op-result
           Display "ResultStatut :"
           Display dynamoDB-op-result
           Perform Checkdynamodb-op-result

      * STORE #2
           Set number-record-type to True
           Move "2" to dynamoDB-data
           Call "DYNAMODBOP" Using dynamoDB-request-area in-rec-as-str2
                dynamoDB-data dynamoDB-op-result
           Display "ResultStatut :"
           Display dynamoDB-op-result
           Perform Checkdynamodb-op-result
    
      * STORE #3
           Set binary-record-type to True
           Move "ciao" to dynamoDB-data
           Call "DYNAMODBOP" Using dynamoDB-request-area in-rec-as-str3
                dynamoDB-data dynamoDB-op-result 
           Display "ResultStatut :"
           Display dynamoDB-op-result
           Perform Checkdynamodb-op-result
    
      * READ #1
           Set read-command to True
           Set string-record-type to True
           Call "DYNAMODBOP" Using dynamoDB-request-area in-rec-as-str1
                dynamoDB-data dynamoDB-op-result 
           Display "ResultStatut :"
           Display dynamoDB-op-result 
           Perform Checkdynamodb-op-result  
           Display "Data :"
           Display dynamoDB-data    

      * READ #2
           Set number-record-type to True
           Call "DYNAMODBOP" Using dynamoDB-request-area in-rec-as-str2
                dynamoDB-data dynamoDB-op-result  
           Display "ResultStatut :"
           Display dynamoDB-op-result
           Perform Checkdynamodb-op-result
           Display "Data :"
           Display dynamoDB-data

      * UPDATE
           Set update-command to True
           Set string-record-type to True
           Move "hello" to dynamoDB-data
           Call "DYNAMODBOP" Using dynamoDB-request-area in-rec-as-str1
                dynamoDB-data dynamoDB-op-result
           Display "ResultStatut :"
           Display dynamoDB-op-result
           Perform Checkdynamodb-op-result

      * READ #3
           Set read-command to True
           Set binary-record-type to True
           Call "DYNAMODBOP" Using dynamoDB-request-area in-rec-as-str3
                dynamoDB-data dynamoDB-op-result
           Display "ResultStatut :"
           Display dynamoDB-op-result  
           Perform Checkdynamodb-op-result
           Display "Data :"
           Display dynamoDB-data
    
      * READ #4
           Set read-command to True
           Set string-record-type to True
           Call "DYNAMODBOP" Using dynamoDB-request-area in-rec-as-str1
                dynamoDB-data dynamoDB-op-result  
           Display "ResultStatut :"
           Display dynamoDB-op-result 
           Perform Checkdynamodb-op-result
           Display "Data :"
           Display dynamoDB-data
    
      * REMOVE
           Set remove-command to True
           Call "DYNAMODBOP" Using dynamoDB-request-area in-rec-as-str2
                dynamoDB-op-result
           Display "ResultStatut :"
           Display dynamoDB-op-result
           Perform Checkdynamodb-op-result

           Goback
           .

      * CHECK DYNAMODB OPERATION OUTCOME
       Checkdynamodb-op-result.
           EVALUATE dynamoDB-op-result
               WHEN 1 
                DISPLAY "NO ITEM HAS BEEN FOUND AT THE GIVEN KEY."
               WHEN 2
                DISPLAY "EMPTY TABLE NAME."
               WHEN 3
                DISPLAY "UNSUPPORTED COMMAND."
               WHEN 4
                DISPLAY "UNSUPPORTED TYPE CONVERSION."
               WHEN 18
                DISPLAY "INVALID REGION."
               WHEN 20
                MOVE dynamoDB-op-result TO dynamodb-op-resultAsStr
                DISPLAY dynamodb-op-res-displ
                CALL "FORCEABEND" using dynamodb-op-err-msg                         
           END-EVALUATE.      
        
       END PROGRAM DynamoDB.
