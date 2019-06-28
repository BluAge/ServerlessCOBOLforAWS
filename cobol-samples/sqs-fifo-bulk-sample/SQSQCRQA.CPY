      ******************************************************************
      * SQS CREATE QUEUE REQUEST AREA
      * BLU AGE SERVERLESS COBOL AWS EXTENSION
      * ----------------------------------------------------------------
      * THIS COPYBOOK CONTAINS THE DEFINITION OF THE REQUEST AREA USED
      * TO PASS SQS CREATE QUEUE OPTIONS TO THE SQSOP PROGRAM.
      * PLEASE SEE: 
      * https://docs.aws.amazon.com/AWSSimpleQueueService
      * /latest/APIReference/API_CreateQueue.html
      * FOR DETAILS ABOUT THE USE OF EACH OPTION.
      * DEFAULTS VALUES HAVE BEEN SET ACCORDING TO THE INFORMATION
      * PROVIDED IN THE LINK ABOVE.
      *-----------------------------------------------------------------
      * AUTHOR: BLU AGE 2019
      * REV: 1.0
      ****************************************************************** 
       01 sqs-create-queue-request-area.
      * FROM 0 TO 900 - defaults to 0
         05 delay-seconds PIC 9(3) BINARY VALUE 0.
      * FROM 1024 (1 KB) TO 262,144 (256 KB) - defaults to 256 KB
         05 maximum-message-size PIC 9(6) BINARY VALUE 262144.
      * FROM 60 TO 1,209,600 seconds  - defaults to 4 days.
         05 message-retention-period PIC 9(7) BINARY VALUE 345600.
      * PROVIDE YOUR OWN JSON POLICY IF REQUIRED  
         05 policy PIC X(2048) VALUE SPACES.
      * FROM 0 TO 20 seconds - defaults to 0 
         05 receive-message-wait-time-seconds PIC 9(2) BINARY VALUE 0.
      * PROVIDE YOUR OWN JSON POLICY IF REQUIRED  
         05 redrive-policy PIC X(2048) VALUE SPACES.
      * FROM 0 TO 43,200 seconds (12 HOURS) - defaults to 30 seconds
         05 visibility-timeout PIC 9(5) BINARY VALUE 30. 
         05 kms-master-key-id PIC X(128) VALUE SPACES.
      *  FROM 60 TO 86,400 s (1 day) - default to 300.
         05 kms-data-key-reuse-period-seconds PIC 9(5) BINARY VALUE 300. 
      *  FIFO QUEUE ? T TRUE , F FALSE
         05 fifo-queue-flag PIC X(1) VALUE 'F'.
           88 FIFO-QUEUE VALUE 'T'.
           88 REGULAR-QUEUE VALUE 'F'.
      *  FLAG FOR CONTENT BASED DEDUPLICATION FOR FIFO 
         05 content-based-deduplication PIC X(1) VALUE 'F'.
           88 content-based-deduplication-enabled VALUE 'T'.  
           88 content-based-deduplication-disabled VALUE 'F'.
           


