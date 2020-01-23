************************************************************************
*
*      Subroutine PRWR
*      -----------------
*
C...   Title:  Printout of winding data
*
*      Written: 92-02-06 by B-G Bladh  , SETFO/TS
*
************************************************************************
*
       SUBROUTINE PRWR(TEXT,ARR,ID,FACTOR,JFC,NWILI)
*
C... Declarations
*
*
       REAL       ARR(9),RRA(9),FACTOR
*
       INTEGER    I,ID,IWDG,JFC,LRR(9),NC,NWILI
*
       CHARACTER  FMT*14,TEXT*9
*
C... Determine the value to be printed
*
       DO 299 IWDG=1,NWILI
         RRA(IWDG)=ARR(IWDG)*FACTOR
         LRR(IWDG)=NINT(RRA(IWDG))
  299  CONTINUE
*
C... No. of positions per winding column = NC
*
       NC=9
       IF(NWILI.GE.8) NC=7
*
C... Format generation & printout
*
       IF (ID.GT.0) THEN
          WRITE (FMT,81) NC,ID
          WRITE(JFC,FMT) TEXT,(RRA(I),I=1,NWILI)
       ELSE
          WRITE (FMT,82) NC
          WRITE(JFC,FMT) TEXT,(LRR(I),I=1,NWILI)
       END IF
       RETURN
*
   81  FORMAT('(6X,A9,9F' ,I1, '.' ,I1, ') ')
   82  FORMAT('(6X,A9,9I' ,I1,        '  ) ')
*
       END
