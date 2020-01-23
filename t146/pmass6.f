C*..* PMASS6
CPMASS6  *******PMASS6 - PRINTOUT OF INPUT DATA MASS06
       SUBROUTINE PMASS6(BBPRNT,JFC,BB08,KCOOL1,PLOSS,KCOOL2,GFAN)
*
       LOGICAL BB08,BBPRNT
*
       CHARACTER SP1*7,SP*10
       DATA SP1/'       '/,SP/'          '/
*

*---------------------------------------------------------------------
       WRITE(JFC,*) '===== INPUT DATA TO MASS06 ====='
       WRITE(JFC,*) SP1,'BB08  ',SP,'KCOOL1',SP,'PLOSS ',SP,'KCOOL2'
       WRITE(JFC,*) BB08,KCOOL1,PLOSS,KCOOL2
       WRITE(JFC,*) SP1,'GFAN  '
       WRITE(JFC,*) GFAN
C
       RETURN
       END
