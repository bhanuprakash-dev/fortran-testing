C*..* PMASS1
CPMASS1   *******PMASS1 - PRINTOUT INPUT DATA FOR MASS01
       SUBROUTINE PMASS1(BBPRNT,JFC,NCOR,NLAM,NCLA,BH,DCORE,OH,OL,
     *                   COCAN,CORLAN,CORLAX,GASEC )
*
       LOGICAL BBPRNT
*
       CHARACTER SP1*7,SP*10
       DATA SP1/'       '/,SP/'          '/
*

*
*--------------------------------------------------------------
       WRITE(JFC,*) '===== INPUT DATA TO MASS01 ====='
       WRITE(JFC,*)  SP1,'NCOR  ',SP,'NLAM  ','NCLA  ',SP,'BH    '
       WRITE(JFC,*)  NCOR,NLAM,NCLA,BH
       WRITE(JFC,*)  SP1,'DCORE ',SP,'OH    ',SP,'OL    ',SP,'COCAN '
       WRITE(JFC,*)  DCORE,OH,OL,COCAN
       WRITE(JFC,*)  SP1,'CORLAN',SP,'CORLAX',SP,'GASEC '
       WRITE(JFC,*)  CORLAN,CORLAX,GASEC
*
       RETURN
       END
