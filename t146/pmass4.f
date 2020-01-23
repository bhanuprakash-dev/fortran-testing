C*..* PMASS4
*******PMASS4 - PRINTOUT OF INPUT DATA MASS04
       SUBROUTINE PMASS4(BBPRNT,JFC,DBW,DWINDY,DUMMY,RJH,NWOLI,NCLA,
     *                   CORLAN,CORLAX,NCOR,DCORE,DWOL,BH,TWSUP,BB048)
*
       LOGICAL BB048,BBPRNT
*
       CHARACTER SP1*7,SP*10
       DATA SP1/'       '/,SP/'          '/
*

*---------------------------------------------------------------------
       WRITE(JFC,*) '===== INPUT DATA TO MASS04 ====='
       WRITE(JFC,*) SP1,'DBW   ',SP,'DWINDY',SP,'DUMMY ',SP,'RJH   '
       WRITE(JFC,*) DBW,DWINDY,DUMMY,RJH
       WRITE(JFC,*) SP1,'NWOLI ',SP,'NCLA  ',SP,'CORLAN',SP,'CORLAX'
       WRITE(JFC,*) NWOLI,NCLA,CORLAN,CORLAX
       WRITE(JFC,*) SP1,'NCOR  ',SP,'DCORE ',SP,'DWOL  ',SP,'BH    '
       WRITE(JFC,*) NCOR,DCORE,DWOL,BH
       WRITE(JFC,*) SP1,'TWSUP ',SP,'BB048 '
       WRITE(JFC,*) TWSUP,BB048
C
       RETURN
       END
