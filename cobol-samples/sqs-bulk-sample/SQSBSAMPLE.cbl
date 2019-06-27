       Identification Division.
       Program-ID. SQSBSAMPLE.
      ******************************************************************
      *  BLU AGE SERVERLESS COBOL - AWS SQS EXTENSION - SAMPLE PROGRAM    
      *  OBJECT: DEMONSTRATING SQS BULK MESSAGE OPERATIONS:
      *  - BULK RECEIVE MESSAGE
      *    |__ ATOMIC MESSAGE PULL FROM STACKED MESSAGES
      *  - STACKING ATOMIC MESSAGE FOR BULK SEND
      *  - BULK SEND MESSAGE
      *  (C) BLU AGE 2019 - ALL RIGHTS RESERVED
      ******************************************************************  
       Environment Division.
       Input-Output Section.

       Data Division.
       Working-Storage Section.

       01 msg-body      Pic X(32).
       01 msg-body-header PIC X(20) VALUE 'THIS IS MSG NUMBER #'.

       01 sqs-op-result  PIC 9(2) BINARY.

      * MSG LOOP COUNTER
       01 msgct PIC 9(3) BINARY.
       01 msgctstr PIC X(3).
       01 msg-sent-cnt PIC X(9).
       01 msg-rcv-cnt PIC 9(4) BINARY VALUE 0.
       01 msg-rcv-cnt-str PIC X(4).
       01 msg-pul-cnt PIC 9(4) BINARY VALUE 0.
       01 msg-pul-cnt-str PIC X(4).
       01 msg-del-cnt PIC 9(4) BINARY VALUE 0.
       01 msg-del-cnt-str PIC X(4).

      * SQS OPERATION CHECK OUTCOME RES
       COPY SQSCHKRES
      * SQS REQUEST AREA 
       COPY SQSRQA

       procedure division.

       Main.
           MOVE SPACES TO msg-body
           MOVE "testlambdaout" to queue-name
           MOVE "eu-central-1" to aws-region
           SET sqs-stack-single-message TO TRUE
           DISPLAY "1- Stacking"

      * STACK MESSAGES TO BE SENT (LOOP)
        PERFORM VARYING msgct FROM 1 BY 1 UNTIL msgct=101
          MOVE msgct TO msgctstr
          MOVE SPACES TO msg-body
          STRING msg-body-header DELIMITED BY ':'
                 msgctstr DELIMITED BY ':' INTO msg-body
      *   DISPLAY "msg body to be stacked: " msg-body        
          CALL "SQSOP" using sqs-request-area msg-body sqs-op-result
          PERFORM Checksqs-op-result
        END-PERFORM

      * SEND ALL STACKED MESSAGES TO QUEUE
        SET sqs-send-multiple-messages TO TRUE.
        DISPLAY "2 - Sending stacked messages"
        CALL "SQSOP" using sqs-request-area sqs-op-result
        PERFORM Checksqs-op-result
        MOVE sqs-sent-messages-count TO msg-sent-cnt
        DISPLAY "Number of sent messages: " msg-sent-cnt

      * READ ALL SEND MESSAGES AGAIN
        SET sqs-receive-multiple-messages TO TRUE
        DISPLAY "3 - Receiving sent messages"
        MOVE 10 TO sqs-max-number-of-messages
        MOVE 30 TO sqs-visibility-timeout
        PERFORM read-loop WITH TEST AFTER 
           UNTIL sqs-received-messages-count=0
        MOVE msg-rcv-cnt TO msg-rcv-cnt-str
        DISPLAY "Number or read messages: " msg-rcv-cnt-str

      * LOOP PULLING FROM THE STACK
        DISPLAY "4 - PULLING IN A LOOP "
        PERFORM pull-loop WITH TEST AFTER
          UNTIL sqs-op-result = 4 OR sqs-op-result=20
        MOVE msg-pul-cnt TO msg-pul-cnt-str
        MOVE msg-del-cnt TO msg-del-cnt-str
        DISPLAY "Pulled " msg-pul-cnt-str " messages."
        DISPLAY "Deleted " msg-del-cnt-str " pulled messages."
        DISPLAY "Normal End of EXECUTION."
        GOBACK.
        .
       COPY SQSCHK.

       pull-loop.
       MOVE SPACES TO sqs-message-receipt-handle
       MOVE SPACES TO msg-body 
       SET sqs-pull-single-message TO TRUE
      * PULL ATOMIC MESSAGE
       CALL "SQSOP" using sqs-request-area msg-body sqs-op-result
       PERFORM Checksqs-op-result
       IF sqs-op-result NOT EQUAL 4
         ADD 1 TO msg-pul-cnt
      * DISPLAY MSG
         DISPLAY "Pulled Message content " msg-body
         DISPLAY "Pulled Message Receipt Handle :" 
         DISPLAY sqs-message-receipt-handle
       END-IF
      * THEN DELETE IT (ONLY IF VALID RECEIPT HANDLE)
       IF sqs-op-result NOT EQUAL 4
         SET sqs-delete-single-message TO TRUE
         CALL "SQSOP" using sqs-request-area sqs-op-result
         PERFORM Checksqs-op-result
         ADD 1 TO msg-del-cnt
       END-IF
       .

       read-loop.
        CALL "SQSOP" using sqs-request-area sqs-op-result
        PERFORM Checksqs-op-result
        ADD sqs-received-messages-count TO msg-rcv-cnt
        .

