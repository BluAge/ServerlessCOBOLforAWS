      ******************************************************************      
      *   BLU AGE SERVERLESS COBOL AWS EXTENSION SAMPLE 
      *   SQS OPERATION OUTCOME HANDLING EXAMPLE
      ******************************************************************
        Checksqs-op-result.
         EVALUATE sqs-op-result
           WHEN 1
             DISPLAY "EMPTY QUEUE! NO MSG TO DISPLAY..."
           WHEN 2
             DISPLAY "MSG IO AREA TOO SMALL TO HANDLE MSG CONTENT!"
           WHEN 3
             DISPLAY "NO MESSAGE TO DELETE!"
           WHEN 4
             DISPLAY "NO MESSAGES IN THE STACK FOR QUEUE "queue-url
          WHEN 5
             DISPLAY 'BAD API USAGE - CHOOSING TO ABEND TO FIX'
             CALL "FORCEABEND" using sqs-op-err-msg       
          WHEN 9
             MOVE sqs-op-result TO sqs-op-result-as-str
             DISPLAY "QUEUE " queue-url " IS NOT ACCESSIBLE!"
             CALL "FORCEABEND" using sqs-op-err-msg
           WHEN 16 THRU 20
             MOVE sqs-op-result TO sqs-op-result-as-str
             DISPLAY sqs-op-res-displ
             CALL "FORCEABEND" using sqs-op-err-msg
           END-EVALUATE.
           