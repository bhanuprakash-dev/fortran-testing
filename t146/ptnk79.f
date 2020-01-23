C*..* PTNK79
CPTNK79       *******PTNK79 - PRINTOUT OF INPUT DATA TANK79
       SUBROUTINE PTNK79(BBPRNT,JFC,HTANK,BTANK,RLTANK,VVAH,RRAD,VTANK,
     *                   ASHELL,ACOVER,KTAN79,TANKDI,GTANK,BBLT10,
     *                   BBEXAC)
*
       LOGICAL BBLT10,BBEXAC,BBPRNT
*
       CHARACTER SP1*7,SP*10
       DATA SP1/'       '/,SP/'          '/
*

*....................................................................
       WRITE(JFC,*) '===== INPUT DATA TO TANK79 ====='
       WRITE(JFC,*) SP1,'HTANK ',SP,'BTANK ',SP,'RLTANK',SP,'VVAH  '
       WRITE(JFC,*) HTANK,BTANK,RLTANK,VVAH
       WRITE(JFC,*) SP1,'RRAD  ',SP,'VTANK ',SP,'ASHELL',SP,'ACOVER'
       WRITE(JFC,*) RRAD,VTANK,ASHELL,ACOVER
       WRITE(JFC,*) SP1,'KTAN79',SP,'BBLT10',SP,'BBEXAC'
       WRITE(JFC,*) KTAN79,BBLT10,BBEXAC
C
       RETURN
       END
