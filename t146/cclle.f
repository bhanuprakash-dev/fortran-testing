C*..* CCLLE
CCCLLE      ********CCLLE
C
C     *******************    NYTT    *************************
C     ALL PARAMETERS PREVIOUSLY STARTING WITH THE PREFIX 'GSN'
C     HAVE BEEN CHANGED TO 'GS' TO ACCOMODATE THE FORTRAN 77
C     NAMING REQUIREMENT OF NOT MORE THAN 6 CHARACTERS.
C     ********************************************************
C
       SUBROUTINE CCLLE (BBPRNT,JFC,NCOND,NGATE,GS31,GS32,GG32,EFKV)
*
       CHARACTER SP1*7,SP*10
       DIMENSION NCOND(4),NGATE(4),GS31(4),GS32(4),GG32(4),EFKV(4),
     *           IHLP(4)
       LOGICAL   BBPRNT
       DATA IHLP/ 72, 63, 65, 67/
       DATA SP1/'       '/,SP/'          '/

***************
       IF(.NOT.BBPRNT) GO TO 101
       WRITE(JFC,*) '===== INPUT DATA TO CCLLE                    ====='
       WRITE(JFC,*) SP1,'NCOND ',SP,'NGATE ',SP,'GS31 ',SP,'GS32 '
       WRITE(JFC,*) NCOND,NGATE,GS31,GS32
       WRITE(JFC,*) SP1,'GG32  ',SP,'EFKV  '
       WRITE(JFC,*) GG32,EFKV
 101   CONTINUE
***************
*
***    LEDARE A-DON
       DO 10 I=1,4
       IF(GS31(I).LE.1.E-5)  GO TO 10
       IX=69+NCOND(I)
       CALL COST (4,IX,167+I ,GS31(I),1.,1.,0.,1.,GS31(I),1.,1.,1.)
 10    CONTINUE
       CALL COST (3,0,172,0.,1.,1.,0.,1.,1.,1.,1.,1.)
*
***    A-DONS-GRINDAR
       DO 15 I=1,4
       IF(GS32(I).LT.1.E-4) GO TO 15
       IX=IHLP(I) +NGATE(I)
       CALL COST(4,IX,172+I,GS32(I),1.,1.,GG32(I),1.,
     *             GS32(I)+GG32(I),1., 1., 1.  )
 15    CONTINUE
       CALL COST (3,0,177,0.,1.,1.,0.,1.,1.,1.,1.,1.)
*
***    F@R-MONTERING
       DO 25 I=1,4
       GS3 = GS31(I) + GS32(I) + GG32(I)
       CALL COST(4,75,178,0.,1.,1.,0.,1.,EFKV(I),GS3,1.,1.)
 25    CONTINUE
       CALL COST (3,0,178,0.,1.,1.,0.,1.,1.,1.,1.,1.)
       CALL COST (2,0,179,0.,1.,1.,0.,1.,1.,1.,1.,1.)
*
       RETURN
       END
