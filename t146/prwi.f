************************************************************************
*
*      Subroutine PRWI
*      -----------------
*
C...   Title:  Printout of winding data
*
*      Written: 92-02-06 by B-G Bladh  , SETFO/TS
*
************************************************************************
*
       SUBROUTINE PRWI(TEXT,KRR,JFC,NWILI)
*
C... Declarations
*
       INTEGER    I,JFC,KRR(9),NC,NWILI
*
       CHARACTER FMT*12, TEXT*9
*
C... No. of positions per winding column = NC
*
       NC=9
       IF(NWILI.GE.8) NC=7
*
C... Format generation & printout
*
       WRITE(FMT,82) NC
       WRITE(JFC,FMT) TEXT,(KRR(I),I=1,NWILI)
       RETURN
*
   82  FORMAT('(6X,A9,9I' ,I1, ') ')
*
       END
