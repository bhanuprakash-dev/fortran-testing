*********************************************************************
*                                                                   *
*      The copyright to the computer program herein is  the         *
*      property of ABB TRANSFORMERS , Sweden.  The  program         *
*      may be used or copied only with the written  permis-         *
*      sion of ABB TRANSFORMERS  or in accordance with  the         *
*      terms of agreement under which the program has  been         *
*      supplied.                                                    *
*                                                                   *
*      In no event shall ABB TRANSFORMERS   be  liable  for         *
*      incidental or consequential damages arising from use         *
*      of this program.                                             *
*                                                                   *
*********************************************************************
*
************************************************************************
*
*      Subroutine CCORET
*      -----------------
*
C...   This routine calculates cost for the core.
*
*      Written: XX-XX-XX by A.N. Other      , XXXX
*      Revised: 90-02-12 by Per S|derberg   , SETFO/IK
*
************************************************************************
*
C*..* ccoret-S
*CCORET
C
C     *******************    NYTT    *************************
C     ALL PARAMETERS PREVIOUSLY STARTING WITH THE PREFIX 'GSN'
C     HAVE BEEN CHANGED TO 'GS' TO ACCOMODATE THE FORTRAN 77
C     NAMING REQUIREMENT OF NOT MORE THAN 6 CHARACTERS.
C     ********************************************************
C
       SUBROUTINE CCORET (BBPRNT,JFC,NCOR,NLAM,NCLA,GS211,GS212,
     *                    GG21,GS241,GS242,GG24,GS25,DCORE)
*
       CHARACTER SP1*7,SP*10
       LOGICAL BBPRNT
       DATA SP1/'       '/,SP/'          '/

*
 100   IF(.NOT.BBPRNT) GO TO 101
       WRITE(JFC,*) '===== INPUT DATA TO CCORET                   ====='
       WRITE(JFC,*) SP1,'NCOR  ',SP,'NLAM  ',SP,'NCLA  ',SP,'GS211'
       WRITE(JFC,*) NCOR,NLAM,NCLA,GS211
       WRITE(JFC,*) SP1,'GS212',SP,'GG21  ',SP,'GS241',SP,'GS242'
       WRITE(JFC,*) GS212,GG21,GS241,GS242
       WRITE(JFC,*) SP1,'GG24  ',SP,'GS25 ',SP,'DCORE '
       WRITE(JFC,*) GG24,GS25,DCORE
 101   CONTINUE
*
***    PRESSFL#NSAR
***    DETALJER AV ST*L
       IX=29+NCLA
       CALL COST (4,IX,153,GS211,1.,1.,0.,1.,GS211,1.,1.,1.)
***    @VRIGA DETALJER
       IX=33+NCLA
       WRITE(*,*) 'IX, GS212', IX, GS212
       CALL COST (4,IX,154,GS212,1.,1.,0.,1.,GS212,1.,1.,1.)
***    SAKVAROR
       IX=37+NCLA
       CALL COST (3,IX,155,0.,1.,1.,GG21,GG21,1.,1.,1.,1.)
       GOTO (31,32,33,34),NLAM
*
 31    CONTINUE
       CALL COST (4,46,160,GS241,1.,1.,0.,1.,1.,1.,1.,1.)
       CALL COST (4,47,161,GS242,1.,1.,0.,1.,1.,1.,1.,1.)
       GOTO 40
 32    CONTINUE
       CALL COST (4,48,162,GS241,1.,1.,0.,1.,1.,1.,1.,1.)
       CALL COST (4,49,163,GS242,1.,1.,0.,1.,1.,1.,1.,1.)
       GOTO 40
 33    CONTINUE
       CALL COST (4,104,162,GS241,1.,1.,0.,1.,1.,1.,1.,1.)
       CALL COST (4,106,163,GS242,1.,1.,0.,1.,1.,1.,1.,1.)
       GOTO 40
 34    CONTINUE
       CALL COST (4,76,162,GS241,1.,1.,0.,1.,1.,1.,1.,1.)
       CALL COST (4,76,163,GS242,1.,1.,0.,1.,1.,1.,1.,1.)
*
 40    IF(NCOR.EQ.1) IX=50
       IF(NCOR.EQ.3) IX=51
       IF(NCOR.GE.7) IX=45+NCOR
***    TILLVERKNING + KONSTR. SKROT
       IF(NLAM.EQ.3.OR.NLAM.EQ.4) GOTO 45
       CALL COST (4,IX,164,0.999,DCORE,1.,GG24,1.,DCORE,1.,1.,1.)
       GOTO 46
 45    CALL COST (4,IX,164,0.999,DCORE,1.,GG24,1.,DCORE,1.3,1.,1.)
 46    CALL COST (3,0,165,0.,1.,1.,0.,1.,1.,1.,1.,1.)
       IF(INT(GS25).EQ.0) GOTO 99
       CALL COST (3,63,166,GS25,1.,1.,0.,1.,GS25,1.,1.,1.)
 99    CALL COST (2,0,167,0.,1.,1.,0.,1.,1.,1.,1.,1.)
*
       RETURN
       END
