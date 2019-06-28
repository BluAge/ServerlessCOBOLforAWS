       Identification Division.
       Program-ID. SQSFBSAMPLE.
      ******************************************************************
      *  BLU AGE SERVERLESS COBOL - AWS SQS EXTENSION - SAMPLE PROGRAM    
      *  OBJECT: DEMONSTRATING SQS FIFO QUEUE OPERATIONS:
      *  - CREATE QUEUE
      *  - STACKING ATOMIC MESSAGE FOR BULK SEND
      *  - BULK SEND MESSAGES
      *  - PURGE QUEUE
      *  - DELETE QUEUE
      *  (C) BLU AGE 2019 - ALL RIGHTS RESERVED
      ******************************************************************  
       Environment Division.
       Input-Output Section.

       Data Division.
       Working-Storage Section.

       01 msg-body      Pic X(32).
       01 msg-body-header PIC X(20) VALUE 'THIS IS MSG NUMBER #'.

       01 sqs-op-result  PIC 9(2) BINARY.
       01 ge-op-result  PIC 9(2) BINARY.

       01 ge-op-res-displ.
          05 ge-op-err-msg PIC X(27)
             VALUE "ERROR ON GETENV OPERATION: ".
          05 ge-op-result-as-str PIC X(2).  

       01 sqs-queue-var-name PIC X(9) VALUE "SQS_QUEUE".
       01 sqs-region-var-name PIC X(14) VALUE "SQS_AWS_REGION".

      * MSG LOOP COUNTER
       01 msgct PIC 9(3) BINARY.
       01 msgctstr PIC X(3).
       01 msg-sent-cnt PIC X(9).

       01 group-id PIC X(8).
       01 dedup-id PIC X(32) VALUE SPACES.

      * SQS OPERATION CHECK OUTCOME RES
       COPY SQSCHKRES
      * SQS REQUEST AREA 
       COPY SQSRQA
      * SQS CREATE QUEUE REQUEST AREA
       COPY SQSQCRQA

       procedure division.

       Main.
        MOVE SPACES TO msg-body
        perform get-config-from-env

      * CREATE QUEUE
        SET FIFO-QUEUE TO TRUE
        MOVE 40 TO visibility-timeout 
        SET sqs-create-queue TO TRUE 
        DISPLAY "1 - QUEUE CREATION."
        CALL "SQSOP" using sqs-request-area 
                           sqs-create-queue-request-area  
                           sqs-op-result 
        PERFORM Checksqs-op-result  

        MOVE "1" TO group-id
        SET sqs-stack-single-message TO TRUE
        DISPLAY "2- STACKING MESSAGES INTO CREATED QUEUE."
      * STACK MESSAGES TO BE SENT (LOOP)
        PERFORM VARYING msgct FROM 1 BY 1 UNTIL msgct=101
          MOVE msgct TO msgctstr
          MOVE SPACES TO msg-body
          STRING msg-body-header DELIMITED BY ':'
                 msgctstr DELIMITED BY ':' INTO msg-body
          MOVE SPACES TO dedup-id
          MOVE msgctstr TO dedup-id       
      *   DISPLAY "msg body to be stacked: " msg-body
          CALL "SQSOP" using sqs-request-area 
                            msg-body 
                            group-id 
                            dedup-id 
                            sqs-op-result
          PERFORM Checksqs-op-result
        END-PERFORM

      * SEND ALL STACKED MESSAGES TO QUEUE
        SET sqs-send-multiple-messages TO TRUE.
        DISPLAY "3 - SENDING STACKED MESSAGES."
        CALL "SQSOP" using sqs-request-area sqs-op-result
        PERFORM Checksqs-op-result
        MOVE sqs-sent-messages-count TO msg-sent-cnt
        DISPLAY "Number of sent messages: " msg-sent-cnt
        
        SET sqs-purge-queue TO TRUE
        DISPLAY "4 - QUEUE PURGE."
        CALL "SQSOP" using sqs-request-area sqs-op-result
        PERFORM Checksqs-op-result

        SET sqs-delete-queue TO TRUE
        DISPLAY "5 - QUEUE DELETION."
        CALL "SQSOP" using sqs-request-area sqs-op-result
        PERFORM Checksqs-op-result
        DISPLAY "Normal End of EXECUTION."
        GOBACK.
        .

      * GET VALUES FROM ENVIRONMENT VARIABLES: OUT QUEUE NAME AND REGION 
        get-config-from-env.
           CALL "GETENVOP" using sqs-queue-var-name
                                 queue-name
                                 ge-op-result
           PERFORM check-ge-res
           CALL "GETENVOP" using sqs-region-var-name
                                 aws-region
                                 ge-op-result
           PERFORM check-ge-res
        .

      * CHECK GETENVOP OUTCOME
       check-ge-res. 
         IF ge-op-result >= 19 THEN 
            MOVE ge-op-result TO ge-op-result-as-str
            DISPLAY ge-op-res-displ
            CALL "FORCEABEND" using ge-op-err-msg 
        END-IF.

      * CHECK SQS OP OUTCOME 
       COPY SQSCHK.






