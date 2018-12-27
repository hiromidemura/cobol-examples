*> The balance line algorithm is a widely used technique specially in >*
*> mainframe programs. It consists in matching two or more sequential >*
*> files to update or verify records.                                 >*
      
IDENTIFICATION DIVISION.
PROGRAM-ID. BALANCE-LINE.
DATA DIVISION.
WORKING-STORAGE SECTION.

    01 TABLE-CLIENT.
        05 REG-CLIENT  OCCURS 3 TIMES.
            10 CLIENT-ID      PIC 9(03).
            10 CLIENT-NAME    PIC X(40).
      
    01 TABLE-TRANSACTION.
        05 REG-TRANS   OCCURS 6 TIMES.
            10 TRANS-ID       PIC 9(03).
            10 TRANS-CLIENT   PIC 9(03).
            10 TRANS-AMMOUNT  PIC 9(03)V99.
      
    01 TABLE-SUMMARY.
        05 REG-SUM     OCCURS 3 TIMES.
            10 SUM-CLIENT     PIC X(40).
            10 SUM-AMMOUNT    PIC 9(03)V99.

    01 IDX-TRANS              PIC 9(01).
    01 IDX-CLIENT             PIC 9(01).
    01 IDX-CLIENT-SUM         PIC 9(01).

PROCEDURE DIVISION.

    PERFORM NEST-TABLES
    
    MOVE 1         TO IDX-TRANS
                      IDX-CLIENT
                      IDX-CLIENT-SUM
    
    PERFORM UNTIL IDX-TRANS  > 6
            OR    IDX-CLIENT > 3

        EVALUATE TRUE
            WHEN TRANS-CLIENT(IDX-TRANS) > CLIENT-ID(IDX-CLIENT)
                ADD 1      TO IDX-CLIENT
            WHEN TRANS-CLIENT(IDX-TRANS) < CLIENT-ID(IDX-CLIENT)
                ADD 1      TO IDX-TRANS            
            WHEN TRANS-CLIENT(IDX-TRANS) = CLIENT-ID(IDX-CLIENT)
                ADD TRANS-AMMOUNT(IDX-TRANS)  TO SUM-AMMOUNT(IDX-CLIENT)
                ADD 1      TO IDX-TRANS
        END-EVALUATE
    END-PERFORM

    MOVE 0         TO IDX-CLIENT-SUM
    
    PERFORM UNTIL IDX-CLIENT-SUM EQUAL 3
        ADD 1      TO IDX-CLIENT-SUM
        DISPLAY SUM-CLIENT(IDX-CLIENT-SUM) ': ' SUM-AMMOUNT(IDX-CLIENT-SUM)
    END-PERFORM
    
    STOP RUN
    .

NEST-TABLES.

    MOVE 1         TO CLIENT-ID    (1)
    MOVE 'Adam'    TO CLIENT-NAME  (1)
                      SUM-CLIENT   (1)
    
    MOVE 2         TO CLIENT-ID    (2)
    MOVE 'John'    TO CLIENT-NAME  (2)
                      SUM-CLIENT   (2)
    
    MOVE 3         TO CLIENT-ID    (3)
    MOVE 'Nick'    TO CLIENT-NAME  (3)
                      SUM-CLIENT   (3)
    
    MOVE 0         TO SUM-AMMOUNT  (1)
                      SUM-AMMOUNT  (2)
                      SUM-AMMOUNT  (3)
    
    MOVE 1         TO TRANS-CLIENT (1)
    MOVE 10.00     TO TRANS-AMMOUNT(1)
    
    MOVE 1         TO TRANS-CLIENT (2)
    MOVE 5.00      TO TRANS-AMMOUNT(2)
    
    MOVE 3         TO TRANS-CLIENT (3)
    MOVE 4.00      TO TRANS-AMMOUNT(3)
    
    MOVE 3         TO TRANS-CLIENT (4)
    MOVE 1.00      TO TRANS-AMMOUNT(4)
    
    MOVE 3         TO TRANS-CLIENT (5)
    MOVE 5.00      TO TRANS-AMMOUNT(5)
    
    MOVE 3         TO TRANS-CLIENT (6)
    MOVE 25.00     TO TRANS-AMMOUNT(6)
    .
