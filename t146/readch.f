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

************************************************************************
*
*      Subroutine Readch
*      -----------------
*
C...   Title:  Read cost files from external files.
*
*      Written: XX-XX-XX by B-G Bladh  , SETFO/TS
*      Moved from OPTSUB to T146 and rewritten
*               92-01-07 by B-G Bladh  , SETFO/TS
************************************************************************
*
       SUBROUTINE READCH(MNLY,FILECH,MNL,RUBCH,CHCOST)
*
*
*     THIS ROUTINE WILL FIND THE COST FILE,
*     READ IN COST CONSTANTS (R79,C75), CURRENCY (CHCOST),
*     AND HEAD FOR COST FILE (RUBCH).
C
       include'com1.h'
C
       COMMON/KONSIN/  R79
C
       COMMON/CST92/
     &   CUC92, CUC92P, FEC92, FEC92P, OLC92, OLC92P
       REAL
     &   CUC92(3), CUC92P(3), FEC92(4), FEC92P(4), OLC92, OLC92P,
     &   R79(15,146), PNFDUC, PNFWIN, PNFYOK
C
       INTEGER MNLY, ISTOP, JFC, IF30
C
       LOGICAL BB79
C
       DIMENSION RUBCH(15)
       CHARACTER
     &   FILECH*24,MNL*8,RUBCH*4,CHCOST*6,
     &   CHLP*1, FILDEF*66
C
       EQUIVALENCE
     &  (XZ2746       ,  PNFDUC        ),
     &  (XZ1486       ,  JFC           ),
     &  (XZ2747       ,  PNFWIN        ),
     &  (XZ2782       ,  ISTOP         ),
     &  (XZ2748       ,  PNFYOK        )
C
C---------------------------------------------------------------
 100   CONTINUE
C
       MNLY=90
C... MNL.YEAR
       BB79  = (MNLY.GE.79 .AND. MNLY.LT.91)
C
C------------------------------------------------------------------
       IF30  = 30
*
C      WRITE(FILDEF,11) FILECH
C11    FORMAT('CMD(FI FT30F001 DISK ',A24,' (RECFM F LRECL 80 )')
*
       IRC=0
       write(*,*)'filech=',filech
C      irc=isplnk('select',66,fildef)
C      IF(IRC.NE.0) GOTO 999
       open(30, file='jawcost', form='formatted', err=999)
C------------------------------------------------------------------
C
*
       IF(BB79) THEN
C
C... MNL79
C
       READ(IF30,2010,END=50) (RUBCH(I), I=1,15)
 50    CONTINUE
 2010  FORMAT(15(A4))
       DO 1099 I = 1, 146
          READ(IF30,2020,END=999,ERR=999) RUB
 2020     FORMAT(A80)
          READ(IF30,*,END=999,ERR=999) (R79(J,I), J=1,5)
          READ(IF30,*,END=999,ERR=999) (R79(J,I), J=6,10)
          READ(IF30,*,END=999,ERR=999) (R79(J,I), J=11,15)
 1099  CONTINUE
C
C... CALCULATION OF WORKSHOPTIMES ?
         CHLP=' '
         READ(MNL, 82,END=999,ERR=999 ) CHLP
 82      FORMAT(A1)
C
         IF(CHLP .EQ. 'T') THEN
C...       HOUR-CALCULATION
 700       DO 799 I=1,146
             R79(3 ,I) = 0.
             R79(4 ,I) = 0.
             R79(5 ,I) = 0.
             R79(6 ,I) = 0.
             R79(8 ,I) = 1.
             R79(13,I) = 0.
             R79(14,I) = 0.
 799         R79(15,I) = 0.
           RUBCH(14) = 'HRS '
         ENDIF
C
         CHCOST='      '
         CALL CONCAT(CHCOST,0,RUBCH(14),0,4)
C
       ELSE
C---------------------------------------------------------------
*
C... MNL92 - FORMULAS
*
         READ(IF30,80,END=999,ERR=999)( RUBCH(I), I=1, 13)
         READ(IF30,81,END=999,ERR=999)  CHCOST
         READ(IF30,*,END=999,ERR=999)   CUC92(1), CUC92P(1)
         READ(IF30,*,END=999,ERR=999)   CUC92(2), CUC92P(2)
         READ(IF30,*,END=999,ERR=999)   CUC92(3), CUC92P(3)
         READ(IF30,*,END=999,ERR=999)   FEC92(1), FEC92P(1)
         READ(IF30,*,END=999,ERR=999)   FEC92(2), FEC92P(2)
         READ(IF30,*,END=999,ERR=999)   FEC92(3), FEC92P(3)
         READ(IF30,*,END=999,ERR=999)   FEC92(4), FEC92P(4)
         READ(IF30,*,END=999,ERR=999)   OLC92, OLC92P
         READ(IF30,*,END=999,ERR=999)   PNFDUC
         READ(IF30,*,END=999,ERR=999)   PNFWIN
         READ(IF30,*,END=999,ERR=999)   PNFYOK
 80      FORMAT(13A4)
 81      FORMAT(A6)
         CALL CONCAT(RUBCH(14),0,CHCOST,0,4)
       ENDIF
       GO TO 800
C
C---------------------------------------------------------------
C...   Error during external file reading
 999   WRITE(JFC,*)' *** ERROR **** during reading external cost file'
       WRITE( * ,*)' *** ERROR **** during reading external cost file'
       WRITE(JFC,*)' *** ERROR **** (missing data or no permission)'
       WRITE( * ,*)' *** ERROR **** (missing data or no permission)'
       WRITE(JFC,*)' *** ERROR **** FILE= ',FILECH
       WRITE( * ,*)' *** ERROR **** FILE= ',FILECH
       WRITE(JFC,*)'               '
       WRITE( * ,*)'               '
        STOP
C---------------------------------------------------------------
 800   REWIND IF30
       CLOSE(IF30)
C
 199   RETURN
C---------------------------------------------------------------
        END
