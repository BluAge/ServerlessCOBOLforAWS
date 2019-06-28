       Identification Division.
       Program-ID. TELCOATOM.
       Environment Division.
       Input-Output Section.

       Data Division.

       Working-Storage Section.
       01  InRecAsStr           Pic X(15).
         
       01  Misc.
           05                   Pic  X          Value "N".
             88  EOF                            Value "Y".
           05.
               10  Start-Time   Pic X(21).
               10  End-Time     Pic X(21).
       01  Misc-Num.
           05  Price-Dec5       Pic S9(05)V9(06).
           05  Redefines Price-Dec5.
               10               Pic X(3).
               10               Pic S9(05).
                 88  Even-Round
                                Value 05000 25000 45000 65000 85000.
           05  Running-Totals.
               10  Price-Tot   Pic S9(07)V99    Binary.
               10  BTax-Tot    Pic S9(07)v99    Binary.
               10  DTax-Tot    Pic S9(07)V99    Binary  Value Zero.
               10  Output-Tot  Pic S9(07)V99    Binary.
           05  Temp-Num.
               10  Temp-Price  Pic S9(05)V99   Binary.
               10  Temp-Btax   Pic S9(05)V99   Binary.
               10  Temp-DTax   Pic S9(05)V99   Binary.
       01  WS-Output.
           05  Detail-Line.
               10               Pic X(01)       Value Space.
               10  Time-Out-2   Pic zzzz9.
               10               Pic X(04)       Value Space.
               10  Rate-Out     Pic X.
               10               Pic X(04)       Value "  | ".
               10  Price-Out    Pic z,zzz,zz9.99.
               10               Pic X(01)       Value Spaces.
               10  Btax-Out     Pic z,zzz,zZ9.99.
               10               Pic X(01)       Value Spaces.
               10  Dtax-Out     Pic Z,zzz,zz9.99        Blank When Zero.
               10               Pic X(03)       Value " | ".
               10  Output-Out   Pic z,zzz,zZ9.99.

       Linkage Section.
       01  InRec                Pic S9(15)      Packed-Decimal.
       01  InRec2 Redefines InRec.
           05                   Pic  X(7).
           05                   Pic S9(1)       Packed-Decimal.
             88  Premimum-Rate                  Value 1 3 5 7 9.

       01  OutRec               Pic X(70).
           
       Procedure Division USING InRec, OutRec.
        Mainline.
           Perform  Calc-Para
           Move Detail-Line to OutRec
           Stop Run
           .
       Calc-Para.
           Display "NEW CALCULATION RULE"
           Move InRec   to Time-Out-2
           If Premimum-Rate
               Move "D"         To Rate-Out
              Compute Temp-Price Rounded Price-Out Rounded Price-Dec5
      *                = InRec * +0.00894
                       = InRec * +0.01788
               Compute Temp-DTax DTax-Out
                        = Temp-Price * 0.0341
               Add Temp-Dtax to DTax-Tot
           Else
               Move "L"         To Rate-Out
               Compute Temp-Price Rounded Price-Out Rounded Price-Dec5
                        = InRec * +0.00130
               Move Zero to DTax-Out Temp-DTax
           End-If
           If Even-Round
               Subtract .01 from Temp-Price
               Move Temp-Price to Price-Out
           End-If
           Compute Temp-Btax BTax-Out = Temp-Price * 0.0675
           Compute Output-Out = Temp-Price + Temp-Btax + Temp-Dtax
           Add Temp-BTax        To Btax-Tot
           Add Temp-Price       to Price-Tot
           Compute Output-Tot = 
                    Output-Tot + Function NumVal (Output-Out (1:))
           .
