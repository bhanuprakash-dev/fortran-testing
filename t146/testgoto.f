        PROGRAM testgoto
        integer x
        x=-1
        IF(x.GT.0) THEN 
        GOTO 10
        
        ELSE 
        GOTO 20
        END IF

10      print *,"line 10"
20      print *,"line 20"
30      print *,"line 30"

        END

