       IDENTIFICATION DIVISION.
       PROGRAM-ID. Kinesis.
      ******************************************************************
      *  BLU AGE SERVERLESS COBOL - AWS KINESIS EXTENSION - SAMPLE 
      *  PROGRAM OBJECT: 
      *  DEMONSTRATING KINESIS ATOMIC RECORD OPERATIONS:
      *  - CREATE STREAM
      *  - DELETE STREAM
      *  - PUBLISH RECORD
      *  - READ RECORD
      *  (C) BLU AGE 2019 - ALL RIGHTS RESERVED
      ******************************************************************  
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.

       01  kinesis-op-result  PIC 9(2) BINARY.

       01  kinesis-op-res-displ.
           05 kinesis-op-err-msg PIC X(23)
             VALUE "ERROR ON KINESIS OPERATION: ".
           05 kinesis-op-resultAsStr PIC X(2).

       01  kinesis-data PIC X(70).

       .

       COPY KinesisRQA

       PROCEDURE DIVISION.

       Main.
           Move "eu-central-1" to aws-region

      *     CREATE STREAM 1
           Set create-command to True
           Move "newStream" to stream-name
           Move 10 to shard-count
           Call "KINESISOP" Using kinesis-request-area kinesis-op-result
           Display "ResultStatut :"
           Display kinesis-op-result
           Perform Check-kinesis-op-result

      *     CREATE STREAM 2
           Move "newStreamToDelete" to stream-name
           Move 1 to shard-count
           Call "KINESISOP" Using kinesis-request-area kinesis-op-result
           Display "ResultStatut :"
           Display kinesis-op-result
           Perform Check-kinesis-op-result
    

      *     PUBLISH 1
           Set publish-command to True
           Move "newStream" to stream-name
           Move "Hello" to kinesis-data
           Move "first" to partition-key
           Call "KINESISOP" Using kinesis-request-area kinesis-data
               kinesis-op-result
           Display "ResultStatut :"
           Display kinesis-op-result
           Perform Check-kinesis-op-result

      *     PUBLISH 2
           Move "World" to kinesis-data
           Call "KINESISOP" Using kinesis-request-area kinesis-data
               kinesis-op-result
           Display "ResultStatut :"
           Display kinesis-op-result
           Perform Check-kinesis-op-result

      *     PUBLISH 3
           Move "Kinesis" to kinesis-data
           Call "KINESISOP" Using kinesis-request-area kinesis-data
               kinesis-op-result
           Display "ResultStatut :"
           Display kinesis-op-result
           Perform Check-kinesis-op-result

      *     PUBLISH 4
           Move "Test" to kinesis-data
           Move "second" to partition-key
           Call "KINESISOP" Using kinesis-request-area kinesis-data
               kinesis-op-result
           Display "ResultStatut :"
           Display kinesis-op-result
           Perform Check-kinesis-op-result
    
      *     READ 1
           Set read-command to True
           Move "first" to partition-key
           Call "KINESISOP" Using kinesis-request-area kinesis-data
               kinesis-op-result
           Display "ResultStatut :"
           Display kinesis-op-result
           Perform Check-kinesis-op-result
           Display "Data :"
           Display kinesis-data

      *     READ 2
           Move "second" to partition-key
           Call "KINESISOP" Using kinesis-request-area kinesis-data
               kinesis-op-result
           Display "ResultStatut :"
           Display kinesis-op-result
           Perform Check-kinesis-op-result
           Display "Data :"
           Display kinesis-data

      *     READ 3
           Move "first" to partition-key
           Call "KINESISOP" Using kinesis-request-area kinesis-data
               kinesis-op-result
           Display "ResultStatut :"
           Display kinesis-op-result
           Perform Check-kinesis-op-result
           Display "Data :"
           Display kinesis-data


      *     DELETE STREAM
           Set delete-command to True
           Move "newStreamToDelete" to stream-name
           Call "KINESISOP" Using kinesis-request-area kinesis-op-result
           Display "ResultStatut :"
           Display kinesis-op-result
           Perform Check-kinesis-op-result
    

      * CHECK KINESIS OPERATION OUTCOME
       Check-kinesis-op-result.
           EVALUATE kinesis-op-result
               WHEN 1
                   DISPLAY "EMPTY STREAM NAME"
               WHEN 2
                   DISPLAY "STREAM NAME ALREADY EXISTS"
               WHEN 3
                   DISPLAY "STREAM NAME DOESN'T EXIST"
               WHEN 4
                   DISPLAY "SHARD NUMBER ISN'T BETWEEN 1 AND 200"
               WHEN 5
                   DISPLAY "UNSUPPORTED COMMAND"
               WHEN 11
                   DISPLAY "STREAM CREATED NEVER WENT ACTIVE"
               WHEN 18
                   DISPLAY "INVALID REGION"
               WHEN 20
                MOVE kinesis-op-result TO kinesis-op-resultAsStr
                DISPLAY kinesis-op-res-displ
                CALL "FORCEABEND" using kinesis-op-err-msg                         
           END-EVALUATE.     
        
       END PROGRAM Kinesis.
