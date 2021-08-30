DECLARE SUB dialog (stew$, anum, aname)
CLS

INPUT "1/2", choice
IF choice = 1 THEN
        anum = 4
        DIM stew(anum)
                FOR x = 1 TO anum
                        IF x <= 1 THEN prompt$ = "how old?"
                        IF x = 2 THEN prompt$ = "when born?"
                        IF x = 3 THEN prompt$ = "where born?"
                        IF x = 4 THEN prompt$ = "why live?"
       
                        CALL dialog(prompt$, x, aname)
                        stew(x) = aname



                NEXT x
        FOR x = 1 TO anum
                PRINT stew(x)
        NEXT x

END IF

        



SUB dialog (stew$, anum, aname)



        PRINT stew$
        PRINT anum
        INPUT aname
END SUB

