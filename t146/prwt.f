************************************************************************
*
*      Subroutine PRWT
*      -----------------
*
C...   Title:  Printout of winding data
*
*      Written: 92-02-06 by B-G Bladh  , SETFO/TS
*
************************************************************************
*
       SUBROUTINE PRWT(TEXT,TRR,JFC,NWILI)
*
C... Declarations
*
       INTEGER    I,JFC,NC,NWILI
*
       DIMENSION  TRR(9)
       CHARACTER  FMT*20,TEXT*9,TRR*4
*
C... No. of positions per winding column = NC
       NC=9
       IF(NWILI.GE.8) NC=7
*
C... Format generation & printout
*
       WRITE (FMT,83) NC-4
       WRITE(JFC,FMT) TEXT,(TRR(I),I=1,NWILI)
       RETURN
*
   83  FORMAT('(6X,A9,1X,9(',I1,'X,A4)) ')
*
       END
