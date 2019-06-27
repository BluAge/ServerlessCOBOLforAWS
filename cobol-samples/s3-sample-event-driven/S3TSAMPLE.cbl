       Identification Division.
       Program-ID. S3TSAMPLE.
      ******************************************************************
      *  BLU AGE SERVERLESS COBOL - AWS S3  EXTENSION - SAMPLE PROGRAM  
      *  OBJECT: DEMONSTRATING S3 EVENT DRIVEN LAMBDA FUNCTION
      *  - OPEN FILE
      *  - READ FILE
      *  - CLOSE FILE 
      *  - DELETE FILE 
      *  (C) BLU AGE 2019 - ALL RIGHTS RESERVED
      ******************************************************************
       Environment Division.
       Input-Output Section.

       Data Division.
       Working-Storage Section.

       01  in-rec PIC X(32).

       01 s3-op-result  PIC 9(2) BINARY.
       
       01 s3-op-res-displ.
          05 s3-op-err-msg PIC X(23)
             VALUE "ERROR ON S3 OPERATION: ".
          05 s3-op-result-as-str PIC X(2).

       linkage section.
        01 s3-request-area.
           05 s3-handle PIC 9(9) COMP-5.  
           05 command PIC X(6).
              88 s3-open-file VALUE "OPEN  ".
              88 s3-read-file VALUE "READ  ".
              88 s3-close-file VALUE "CLOSE ".
              88 s3-delete-file VALUE "DELETE".
           05 s3-object-description.
              10 bucket-name PIC X(63).
              10 object-key PIC X(1024).
              10 region PIC X(24).


       procedure division using s3-request-area.

       Main.
      *    OPEN S3 FILE
           SET s3-open-file TO TRUE
           CALL "S3OP" using s3-request-area s3-op-result
           PERFORM checks3-op-result
      
      *    READ LOOP
           SET s3-read-file TO TRUE
           PERFORM read-loop UNTIL s3-op-result=9

      *    CLOSE S3 FILE           
           SET s3-close-file TO TRUE
           call "S3OP" using s3-request-area s3-op-result
           PERFORM checks3-op-result
 
      *    DELETE
           SET s3-delete-file TO TRUE
           call "S3OP" using s3-request-area s3-op-result
           PERFORM checks3-op-result
           DISPLAY "NORMAL END OF EXECUTION."
           GOBACK.

        read-loop.
           call "S3OP" using s3-request-area in-rec s3-op-result
           PERFORM checks3-op-result
           DISPLAY in-rec
           .

      *   CHECK S3 OPERATION OUTCOME
        checks3-op-result.
           IF s3-op-result >= 10 THEN 
             MOVE s3-op-result TO s3-op-result-as-str
             DISPLAY s3-op-res-displ
             CALL "FORCEABEND" using s3-op-err-msg 
           END-IF. 
