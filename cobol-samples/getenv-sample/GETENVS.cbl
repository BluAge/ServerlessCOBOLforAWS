       Identification Division.
       Program-ID. GETENVS.
      ******************************************************************
      *  BLU AGE SERVERLESS COBOL - GETENV EXTENSION - SAMPLE PROGRAM   
      *  OBJECT: DEMONSTRATING USAGE OF GETENV FACILITY
      *  (C) BLU AGE 2019 - ALL RIGHTS RESERVED
      ******************************************************************  
       Environment Division.
       Input-Output Section.

       Data Division.
       Working-Storage Section.

       01 env-var-name PIC X(32) VALUE "BA_TEST_ENV_VARIABLE".
       01 env-var-value PIC X(32).
       01 ge-op-result  PIC 9(2) BINARY.

       01 ge-op-res-displ.
          05 ge-op-err-msg PIC X(27)
             VALUE "ERROR ON GETENV OPERATION: ".
          05 ge-op-result-as-str PIC X(2).

       procedure division.

       Main.
       DISPLAY "GETTING "env-var-name " VALUE."
       CALL "GETENVOP" using env-var-name env-var-value ge-op-result
       PERFORM check-ge-op-res.
       DISPLAY env-var-name " => " env-var-value
       GOBACK.

       check-ge-op-res.
       IF ge-op-result >= 19 THEN 
             MOVE ge-op-result TO ge-op-result-as-str
             DISPLAY ge-op-res-displ
             CALL "FORCEABEND" using ge-op-err-msg 
       END-IF. 

