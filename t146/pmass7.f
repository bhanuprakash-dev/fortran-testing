C*..* PMASS7
CPMASS7    *******PMASS7 - PRINT OUT OF INPUT DATA MASS07
       SUBROUTINE PMASS7(BBPRNT,JFC,NCLA,NWILI,NG,VTANK,RLTANK,BTANK,
     *                   HTANK,BB132,RAACON,VACTP,GVVAH,GOFWF)
*
       DIMENSION RAACON(9)
*
       LOGICAL BB132,BBPRNT
*
       CHARACTER SP1*7,SP*10
       DATA SP1/'       '/,SP/'          '/
*

*---------------------------------------------------------------------
       WRITE(JFC,*) '===== INPUT DATA TO MASS07 ====='
       WRITE(JFC,*) SP1,'NCLA  ',SP,'NWILI ',SP,'NG    ',SP,'VTANK '
       WRITE(JFC,*) NCLA,NWILI,NG,VTANK
       WRITE(JFC,*) SP1,'RLTANK',SP,'BTANK ',SP,'HTANK ',SP,'BB132 '
       WRITE(JFC,*) RLTANK,BTANK,HTANK,BB132
       WRITE(JFC,*) SP1,'RAACON(1)-RAACON(4)'
       WRITE(JFC,*) (RAACON(I),I=1,4)
       WRITE(JFC,*) SP1,'RAACON(5)-RAACON(8)'
       WRITE(JFC,*) (RAACON(I),I=5,8)
       WRITE(JFC,*) SP1,'RAACON(9)',SP1,'VACTP ',SP,'GVVAH ',SP,'GOFWF '
       WRITE(JFC,*) RAACON(9),VACTP,GVVAH,GOFWF
C
       RETURN
       END
