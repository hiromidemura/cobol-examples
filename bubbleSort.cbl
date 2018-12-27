IDENTIFICATION DIVISION.
PROGRAM-ID. BUBBLE SORT.
DATA DIVISION.
WORKING-STORAGE SECTION.

    01 WS-TAB.
        05 WS-LN   OCCURS 10 TIMES.
            10 WS-REG       PIC 9(02).
    
    01 WS-TAB-LENGHT        PIC 9(02) VALUE 10.
    01 WS-SWAP              PIC 9(02).
    01 IDX                  PIC 9(02).
    01 IDX-IN               PIC 9(02).
    01 IDX-OUT              PIC 9(02).    

PROCEDURE DIVISION.

    PERFORM NEST-TAB
    
    MOVE 0         TO IDX-IN
                      IDX-OUT

    PERFORM WS-TAB-LENGHT TIMES
        ADD 1 TO IDX-IN
        COMPUTE IDX-OUT = IDX-IN + 1
        PERFORM UNTIL IDX-OUT > WS-TAB-LENGHT
            IF WS-REG(IDX-IN) > WS-REG(IDX-OUT)
                MOVE WS-REG(IDX-IN)   TO WS-SWAP
                MOVE WS-REG(IDX-OUT)  TO WS-REG(IDX-IN)
                MOVE WS-SWAP          TO WS-REG(IDX-OUT)
            END-IF
            ADD 1 TO IDX-OUT
        END-PERFORM
        MOVE 0 TO IDX-OUT
    END-PERFORM

    MOVE 0 TO IDX
    
    PERFORM UNTIL IDX = WS-TAB-LENGHT
        ADD 1 TO IDX
        DISPLAY 'Ouput Reg. ' IDX ': ' WS-REG(IDX)
    END-PERFORM.                      

    STOP RUN
    .

NEST-TAB.

    MOVE 0 TO IDX
    
    PERFORM UNTIL IDX = WS-TAB-LENGHT
        ADD 1 TO IDX
        COMPUTE WS-REG(IDX) = (FUNCTION RANDOM) * 10
        DISPLAY 'Input Reg. ' IDX ': ' WS-REG(IDX)
    END-PERFORM
    .
