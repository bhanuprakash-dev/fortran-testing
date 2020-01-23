C*..* PMASS3
CPMASS3   *******PMASS3 - PRINTOUT OF INPUT DATA MASS03
       SUBROUTINE  PMASS3(BBPRNT,JFC,J,KCON,UPHAS,RIPHAS,BH,NWOLI,
     *                    NCON,RLCON,BOOST)
*
       LOGICAL BBPRNT
*
       CHARACTER SP1*7,SP*10
       DATA SP1/'       '/,SP/ '          '/
*

*-------------------------------------------------------------------
       WRITE(JFC,*) '===== INPUT DATA TO MASS03 ====='
       WRITE(JFC,*) SP1,'J     ',SP,'KCON  ',SP,'UPHAS ',SP,'RIPHAS'
       WRITE(JFC,*) J,KCON,UPHAS,RIPHAS
       WRITE(JFC,*) SP1,'BH    ',SP,'NWOLI ',SP,'NCON  ',SP,'RLCON '
       WRITE(JFC,*) BH,NWOLI,NCON,RLCON
       WRITE(JFC,*) SP1,'BOOST '
       WRITE(JFC,*) BOOST
*
       RETURN
       END
