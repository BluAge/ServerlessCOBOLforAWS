       Identification Division.
       Program-ID. SQSTSAMPLE.
      ******************************************************************
      *  BLU AGE SERVERLESS COBOL - AWS SQS EXTENSION - SAMPLE PROGRAM    
      *  OBJECT: DEMONSTRATING SQS EVENT DRIVEN LAMBDA FUNCTION
      *  - RECEIVE MESSAGE
      *  - SEND MODIFIED MESSAGE BACK TO ANOTHER QUEUE
      *  (C) BLU AGE 2019 - ALL RIGHTS RESERVED
      ******************************************************************  
       Environment Division.
       Input-Output Section.

       Data Division.
       Working-Storage Section.
       01 msg-body-fwd    Pic X(37) VALUE SPACES.
       01 msg-body-header PIC X(5) VALUE 'FWD: '.
       01 sqs-op-result  PIC 9(2) BINARY.
       01 ge-op-result  PIC 9(2) BINARY.
       
       01 sqs-op-res-displ.
          05 sqs-op-err-msg PIC X(23)
             VALUE "ERROR ON SQS OPERATION: ".
          05 sqs-op-result-as-str PIC X(2).

       01 ge-op-res-displ.
          05 ge-op-err-msg PIC X(27)
             VALUE "ERROR ON GETENV OPERATION: ".
          05 ge-op-result-as-str PIC X(2).          

       01 sqs-queue-out-var-name PIC X(13) VALUE "SQS_OUT_QUEUE".
       01 sqs-region-var-name PIC X(14) VALUE "SQS_AWS_REGION".

       COPY SQSRQA
       
       linkage section.
       01 msg-body.
        05 msg-body-len PIC 9(9) COMP-5.      
        05 msg-body-data Pic X(32).

       procedure division using msg-body.

       Main.
           DISPLAY "Triggering message:" msg-body-data(1:msg-body-len)
           PERFORM get-config-from-env

      *  SEND BACK TO ANOTHER QUEUE 
           PERFORM send-to-out
           DISPLAY "SEND MODIFIED MESSAGE TO " queue-name
           DISPLAY "NORMAL END OF EXECUTION"
           GOBACK.
         .

      * MODIFY RECEIVED MESSAGE AND SENT IT TO CONFIGURED OUT QUEUE
        send-to-out.
           SET clear-text TO TRUE
           STRING msg-body-header DELIMITED BY ':'
                 msg-body-data(1:msg-body-len) 
                 DELIMITED BY '#' INTO msg-body-fwd
           SET sqs-send-single-message TO TRUE
           CALL "SQSOP" using sqs-request-area 
                              msg-body-fwd 
                              sqs-op-result
           PERFORM Checksqs-op-result
        .
      * GET VALUES FROM ENVIRONMENT VARIABLES: OUT QUEUE NAME AND REGION 
        get-config-from-env.
           CALL "GETENVOP" using sqs-queue-out-var-name
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
        
        COPY SQSRESCHK
