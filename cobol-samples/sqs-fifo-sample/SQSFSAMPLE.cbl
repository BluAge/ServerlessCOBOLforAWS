       Identification Division.
       Program-ID. SQSFSAMPLE.
      ******************************************************************
      *  BLU AGE SERVERLESS COBOL - AWS SQS EXTENSION - SAMPLE PROGRAM    
      *  OBJECT: DEMONSTRATING SQS ATOMIC MESSAGE OPERATIONS ON FIFO
      *  QUEUE :
      *  - SEND MESSAGE
      *  (C) BLU AGE 2019 - ALL RIGHTS RESERVED
      ******************************************************************  
       Environment Division.
       Input-Output Section.

       Data Division.
       Working-Storage Section.

       01 msg-body      Pic X(32).
       01 msg-body-header PIC X(20) VALUE 'THIS IS MSG NUMBER #'.

       01 sqs-op-result  PIC 9(2) BINARY.
       01 msgct PIC 9(3) BINARY.
       01 msgctstr PIC X(3).

       01 sqs-op-res-displ.
          05 sqs-op-err-msg PIC X(23)
             VALUE "ERROR ON SQS OPERATION: ".
          05 sqs-op-result-as-str PIC X(2).
        
        01 group-id PIC X(8).
        01 dedup-id PIC X(32).

       COPY SQSRQA
        
       procedure division.

       Main.      
           MOVE SPACES TO msg-body
           MOVE "testautodedup.fifo" to queue-name
           MOVE "eu-central-1" to aws-region
           
           MOVE "GROUP1" to group-id
           MOVE 1 TO msgct
           MOVE msgct TO msgctstr
           STRING msg-body-header DELIMITED BY ':'
                 msgctstr DELIMITED BY ':' INTO msg-body
           
           SET clear-text TO TRUE
           SET sqs-send-single-message TO TRUE      
           
           PERFORM send-to-out-fifo-no-dedup
           DISPLAY 'USED QUEUE NO DEDUP ' queue-url

      *    SELECT ANOTHER FIFO QUEUE WITH NO DEDUP 
           MOVE "testlambda.fifo" to queue-name
           MOVE SPACES TO queue-url
           MOVE '1' TO dedup-id
           
           PERFORM send-to-out-fifo-dedup
           DISPLAY 'USED QUEUE DEDUP ' queue-url

         DISPLAY "NORMAL END OF EXECUTION"
         GOBACK.
         .

        send-to-out-fifo-dedup.
           DISPLAY "DEDUP " queue-name
           CALL "SQSOP" using sqs-request-area 
                              msg-body 
                              group-id 
                              dedup-id 
                              sqs-op-result
           PERFORM Checksqs-op-result
        .


        send-to-out-fifo-no-dedup.
           DISPLAY "NO DEDUP " queue-name
           CALL "SQSOP" using 
           sqs-request-area msg-body group-id sqs-op-result
           PERFORM Checksqs-op-result
        .
        
        COPY SQSRESCHK
