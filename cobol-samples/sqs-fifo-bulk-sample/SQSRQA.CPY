      ******************************************************************
      * SQS REQUEST AREA
      * BLU AGE SERVERLESS COBOL AWS EXTENSION
      * ----------------------------------------------------------------
      * THIS COPYBOOK CONTAINS THE DEFINITION OF THE REQUEST AREA USED
      * TO PASS SQS COMMAND AND OPTIONS TO THE SQSOP PROGRAM.
      * PLEASE CHECK THE BLU AGE SERVERLESS COBOL AWS EXTENSION 
      * DOCUMENTATION FOR MORE DETAILS ABOUT THE USAGE OF EVERY
      * ELEMENT.
      * 
      *-----------------------------------------------------------------
      * AUTHOR: BLU AGE 2019
      * REV: 1.0
      ******************************************************************
       01 sqs-request-area.
           05 sqs-command PIC X(7).
             88 sqs-receive-single-message value 'RCVSMSG'.
             88 sqs-delete-single-message value 'DELSMSG'.
             88 sqs-consume-single-message value 'CSMSMSG'.
             88 sqs-stack-single-message value 'STASMSG'.
             88 sqs-send-single-message value 'SNDSMSG'.
             88 sqs-receive-multiple-messages value 'RCVMMSG'.
             88 sqs-send-multiple-messages value 'SNDMMSG'.
             88 sqs-pull-single-message value 'PULSMSG'.
             88 sqs-create-queue value 'CREATEQ'.
             88 sqs-purge-queue value 'PURGEQ '.
             88 sqs-delete-queue value 'DELETEQ'.
           05 sqs-msg-option PIC X(1) VALUE 'T'.
             88 clear-text VALUE 'T'.
             88 base64-encoded VALUE 'B'.
           05 sqs-queue-description.
              10 queue-name PIC X(80).
              10 queue-url PIC X(200).
              10 aws-region PIC X(24).
           05 sqs-max-number-of-messages PIC 9(2) COMP-5 value 1.
           05 sqs-visibility-timeout PIC 9(4) COMP-5 value 30.
           05 sqs-sent-messages-count PIC 9(9) COMP-5 value 0.
           05 sqs-received-messages-count PIC 9(2) COMP-5 value 0.
           05 sqs-message-receipt-handle PIC X(1024).
           