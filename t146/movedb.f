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
*      Subroutine MOVEDB
*      -----------------
*
C...   Title:  Move calculated results for program T31146
C...           to dialog variables for the display of
C...           the results panel P146
*
*      Written: XX-XX-XX by A.N.Other  , XXXX
*      Revised: 85-10-18 by Ron Bell   , TRAFO/IK
*      Revised: 88-02-10 by Ron Bell   , TRAFO/IK
*
************************************************************************
*
       SUBROUTINE MOVEDB
*
C... Declarations
*
CC*SBBL Start of the declarations block.
*
       REAL           RES79 (241,6)
       COMMON/KONSRE/ RES79
*
       include'com1.h'
*
       include'cominp.h'
*
       EQUIVALENCE
     &  (XZ0651(1)    ,  ACONDX (1)    ),
     &  (XZ0701(1)    ,  CURDEX (1)    ),
     &  (XZ0731(1)    ,  CURRUT (1)    ),
     &  (XZ0791(1)    ,  FILLFX (1)    ),
     &  (XZ1001(1)    ,  RRWDGX (1)    )
       EQUIVALENCE
     &  (XZ1041(1)    ,  STRWMX (1)    ),
     &  (XZ1051(1)    ,  STRWPX (1)    ),
     &  (XZ1191(1)    ,  ZWIND  (1)    ),
     &  (XZ1393       ,  BBVFRX        )
       EQUIVALENCE
     &  (XZ1407       ,  BMAXPU        ),
     &  (XZ1410       ,  BTANKX        ),
     &  (XZ1414       ,  CLOSSX        ),
     &  (XZ1415       ,  COSTX         ),
     &  (XZ1422       ,  CTROVX        )
       EQUIVALENCE
     &  (XZ1426       ,  DCOREX        ),
     &  (XZ1466       ,  GTRPX         ),
     &  (XZ1472       ,  HLIMBX        ),
     &  (XZ1474       ,  HTANKX        ),
     &  (XZ1508       ,  NWILI         )
       EQUIVALENCE
     &  (XZ1509       ,  NWOULI        ),
     &  (XZ1515       ,  PLIMB         ),
     &  (XZ1528       ,  RLTANX        ),
     &  (XZ1624       ,  MNLX          ),
     &  (XZ1632(1)    ,  RUBCH  (1)    ),
     &  (XZ1650(1)    ,  P00X   (1)    ),
     &  (XZ1686(1)    ,  SOUNDX( 1)    )
       EQUIVALENCE
     &  (XZ2245(1)    ,  WLOSEX (1)    ),
     &  (XZ2254(1)    ,  WLOSRX (1)    )
*
       REAL      ACONDX(9),CURDEX(9),FILLFX(9),P00(3),P00X(3),
     &           CURRUT(9),SOUND0(3),SOUNDX(3),RRWDGX(9),ZWIND(9),
     &           STRWMM(9),STRWMP(9),WLOSSR(9),WLOSSE(9),
     &           STRWMX(9),STRWPX(9),WLOSRX(9),WLOSEX(9),
     &           U1(4),U2(4),U3(4),UX12(4),UX13(4),UX23(4),
     &           UR12(4),UR13(4),UR23(4),BMAXPU,BTANKX,
     &           CLOSSX,CMANUF,COSTX,CRAZY,CS,CTROVX,DCOREX,GTRPX,
     &           HLIMBX,HTANKX,PLIMBX,PLIMB,RLTANX,TEST
*
       INTEGER   I,IWDG,ND,NWILI,NWOULI
*
       DIMENSION RUBCH(15)
       CHARACTER MNLX*8,RUBCH*4,CURNCY*4
*
       LOGICAL   BBVFRX
*
       DATA CRAZY/1.e-20/
*
CC*SEBL End of the declarations block.
*
C... Local function
*
CC*SBBL Start of the function block
*
       CS(I)=RES79(I,3)+RES79(I,5)+RES79(I,6)
*
       CMANUF=CS(241)
*
CC*SEBL End of the function block
*
*... Rewind binaryfile 04.
*
CCC    REWIND(04)
       CURNCY=RUBCH(14)
*
C... Initialisations
*
       DO 5 I=1,3
       SOUND0(I)=CRAZY
       IF (BBVFRX) SOUND0(I)=SOUNDX(I)
    5  CONTINUE
       IF (.NOT.BBVFRX) SOUND0(1)=SOUNDX(1)
*
       HLIMB=1.E+3*HLIMBX
       write(*,*) 'movedb 123', HLIMB
       DCORE=1.E+3*DCOREX
*
C... For each winding
*
       DO 10 IWDG=1,NWILI
       FILLF(IWDG)=FILLFX(IWDG)
       BWIND(IWDG)=1.E+3*RRWDGX(IWDG)
       ACOND0(IWDG)=1.E+6*ACONDX(IWDG)
       CURD0(IWDG)=CURRUT(IWDG)/ACONDX(IWDG)/1.E+6
       WLOSSR(IWDG)=WLOSRX(IWDG)/1.E+3
       WLOSSE(IWDG)=WLOSEX(IWDG)/1.E+3
       STRWMP(IWDG)=STRWPX(IWDG)/1.E+6
       STRWMM(IWDG)=ABS(STRWMX(IWDG))/1.E+6
   10  CONTINUE
*
C... Output variables
*
c       CALL CMOVD(IDENT ,'ID ', 1,40)
       PLIMBX=1.E+3*PLIMB
       IF (NWOULI.EQ.1) PLIMBX=0.
c       CALL RMOVD(PLIMBX ,'X55 ',1,6,0)
*
CC*SOFF Switch OFF structure analyser (Not all calls need be shown)
*
c       CALL RMOVD(SOUND0(1)  ,'X56 ',1,6,1)
c       CALL RMOVD(SOUND0(2)  ,'X57 ',1,6,1)
c       CALL RMOVD(SOUND0(3)  ,'X58 ',1,6,1)
*
c       CALL RMOVD(ZWIND ,'Y*7 ' ,NWILI,6,1)
c       CALL RMOVD(STRWMP,'Y*8 ' ,NWILI,4,0)
c       CALL RMOVD(STRWMM,'Y*9 ' ,NWILI,4,0)
c       CALL RMOVD(WLOSSR,'Y*10 ',NWILI,5,1)
c       CALL RMOVD(WLOSSE,'Y*11 ',NWILI,5,1)
*
CC*SON  Switch ON  structure analyser (Not all calls need be shown)
*
C... Voltage reactances and losses
*
       CALL UKPK(U1,U2,U3,UX12,UX13,UX23,UR12,UR13,UR23)
*
C... For each tap
*
       DO 15 I=1,3
       IF (U1(I)  .LT.0.0001) U1(I)=CRAZY
       IF (U2(I)  .LT.0.0001) U2(I)=CRAZY
       IF (U3(I)  .LT.0.0001) U3(I)=CRAZY
       IF (UX12(I).LT.0.0001) UX12(I)=CRAZY
       IF (UX13(I).LT.0.0001) UX13(I)=CRAZY
       IF (UX23(I).LT.0.0001) UX23(I)=CRAZY
       IF (UR12(I).LT.0.0001) UR12(I)=CRAZY
       IF (UR13(I).LT.0.0001) UR13(I)=CRAZY
       IF (UR23(I).LT.0.0001) UR23(I)=CRAZY
   15  CONTINUE
*
C... For each tap
*
       DO 20 I=1,3
       P00(I)=CRAZY
       IF (BBVFRX) P00(I)=P00X(I)/1.E+3
   20  CONTINUE
       IF (.NOT.BBVFRX) P00(1)=P00X(1)/1.E+3
*
CC*SOFF Switch OFF structure analyser (Not all calls need be shown)
*
       ND=1
       TEST=U1(1)
       IF (TEST.GT.0.0001) ND=3-INT(ALOG10(U1(1)))
c       CALL RMOVD(U1(1),'Z11 ' ,1,5,ND)
       ND=1
       TEST=U1(2)
       IF (TEST.GT.0.0001) ND=3-INT(ALOG10(U1(2)))
c       CALL RMOVD(U1(2),'Z21 ' ,1,5,ND)
       ND=1
       TEST=U1(3)
       IF (TEST.GT.0.0001) ND=3-INT(ALOG10(U1(3)))
c       CALL RMOVD(U1(3),'Z31 ' ,1,5,ND)
       ND=1
       TEST=U2(1)
       IF (TEST.GT.0.0001) ND=3-INT(ALOG10(U2(1)))
c       CALL RMOVD(U2(1),'Z12 ' ,1,5,ND)
       ND=1
       TEST=U2(2)
       IF (TEST.GT.0.0001) ND=3-INT(ALOG10(U2(2)))
c       CALL RMOVD(U2(2),'Z22 ' ,1,5,ND)
       ND=1
       TEST=U2(3)
       IF (TEST.GT.0.0001) ND=3-INT(ALOG10(U2(3)))
c       CALL RMOVD(U2(3),'Z32 ' ,1,5,ND)
       ND=1
       TEST=U3(1)
       IF (TEST.GT.0.0001) ND=3-INT(ALOG10(U3(1)))
c       CALL RMOVD(U3(1),'Z13 ' ,1,5,ND)
       ND=1
       TEST=U3(2)
       IF (TEST.GT.0.0001) ND=3-INT(ALOG10(U3(2)))
c       CALL RMOVD(U3(2),'Z23 ' ,1,5,ND)
       ND=1
       TEST=U3(3)
       IF (TEST.GT.0.0001) ND=3-INT(ALOG10(U3(3)))
c       CALL RMOVD(U3(3),'Z33 ' ,1,5,ND)
*
c       CALL RMOVD(UX12,'Z*4 ' ,3,6,2)
c       CALL RMOVD(UX13,'Z*5 ' ,1,6,2)
c       CALL RMOVD(UX23,'Z*6 ' ,1,6,2)
c       CALL RMOVD(UR12,'Z*7 ' ,3,6,1)
c       CALL RMOVD(UR13,'Z*8 ' ,1,6,1)
c       CALL RMOVD(UR23,'Z*9 ' ,1,6,1)
c       CALL RMOVD(P00 ,'Z*11 ',3,5,1)
*
C... Tank dimensions & costs
*
c       CALL RMOVD(1.E+3*BTANKX,'W11 ',1,6,0)
c       CALL RMOVD(1.E+3*RLTANX,'W12 ',1,6,0)
c       CALL RMOVD(1.E+3*HTANKX,'W13 ',1,6,0)
c       CALL RMOVD(GTRPX       ,'W14 ',1,7,0)
c       CALL CMOVD(MNLX        ,'W15 ',1,4)
c       CALL RMOVD(CTROVX      ,'W16 ',1,8,0)
c       CALL RMOVD(CLOSSX      ,'W17 ',1,8,0)
c       CALL RMOVD(CLOSSX+COSTX,'W18 ',1,8,0)
c       CALL CMOVD(CURNCY      ,'W19 ',1,4)
c       CALL RMOVD(CMANUF      ,'W20 ',1,12,0)
c*
CC*SON  Switch ON  structure analyser (Not all calls need be shown)
*
*... Write on binaryfile the resultmatrix from OPTSUB.
*
CCC    WRITE(04) RES79
*
        write(*,*) 'P0sses',P00(1)
        write(*,*)'U1', U1

        write(*,*)'U2', U2
        write(*,*)'U3',U3
        write(*,*)'UX12',UX12
        write(*,*)'UX13',UX13
        write(*,*)'UX23',UX23
        write(*,*)'UR12',UR12
        write(*,*)'UR13',UR13
        write(*,*)'UR23',UR23
        write(*,*)'STR-',(STRWMM(i),i=1,NWILI)
        write(*,*) 'STR+',(STRWMP(i),i=1,NWILI)
        write(*,*) 'RII',(WLOSSR(i),i=1,NWILI)
        write(*,*) 'EDDY',(WLOSSE(i),i=1,NWILI)
        write(*,*) 'SOUND level',SOUND0(1)
        write(*,*) 'GTRPX=',GTRPX
        write(*,*) 'CTROVX=',CTROVX

       RETURN
       END
