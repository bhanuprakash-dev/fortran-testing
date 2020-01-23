*********************************************************************
*                                                                   *
*      The copyright to the computer program herein is  the         *
*      property of ABB  TRANSFORMERS, Sweden.  The  program         *
*      may be used or copied only with the written  permis-         *
*      sion of ABB  TRANSFORMERS or in accordance with  the         *
*      terms of agreement under which the program has  been         *
*      supplied.                                                    *
*                                                                   *
*      In no event shall ABB  TRANSFORMERS  be  liable  for         *
*      incidental or consequential damages arising from use         *
*      of this program.                                             *
*                                                                   *
*********************************************************************

************************************************************************
*
*      Subroutine TBAOK
*      ----------------
*
*  0.  Written: XX-XX-XX by S-E Jansson      , ZKB
*      Revised: 86-12-09 by J. Akesson       , Z1KDC
*      Revised: 91-11-27 by Ron Bell         , ABB Power T & D Muncie
*
*  1.  Description
C...       Title:  Calculates the yoke section
C...               for the TBA Core
*
************************************************************************
*
       SUBROUTINE TBAOK(N,BSTEP,B1,OLAPP,DCOR,TT,TP,
     &                  TWSUPC,TH,TB,B3,RK1,OHRED)
*
       COMMON /CTBAOK/ EOKF,FINDR
*
       REAL      BSTEP,AOKERF,AREA1,AREA2,AVTR,BMIN,DCOR,DCORA,
     &           EOKF,FIND,FINDR,OH,OHRED,OKM,OLAPP,TB,TH,TS,
     &           TSTEG,TWSUPC,TT
*
       REAL      B1(0:N),TP(0:N),RK1(N),B3(N)
*
       INTEGER   I,I1,IX,IX1,M,N,N1
*
C... Preliminary width of yoke plates
*
       DO 299 I=1,N
          RK1(I)=0.
          B3(I)=B1(I)+OLAPP
 299   CONTINUE
*
       M=N
*
  300  IF (TWSUPC.LE.5.) THEN
  301     CONTINUE
          IF (OHRED.GT.0.) THEN
             DCORA=660.
          ELSE
             DCORA=580.
          END IF
*
          IF (DCOR.GE.DCORA) THEN
             IX1=1
             CALL AVTRAL(M,IX1,RK1,TP,150.,50.-TT)
             IX1=1
             CALL AVTRAL(M,IX1,RK1,TP,80.,120.-TT)
             IX1=1
             CALL AVTRAL(M,IX1,RK1,TP,40.,186.-TT)
          ELSE
             IX1=1
             CALL AVTRAL(M,IX1,RK1,TP,40.,92.-TT)
          END IF
       ELSE
*
 302      AVTR=TH+50.+TWSUPC
          BMIN=TB+10.-TT
          IX1=1
          CALL AVTRAL(M,IX1,RK1,TP,AVTR,BMIN)
*
          AVTR=TWSUPC
          TS=0.
*
          IF (FINDR.EQ.0.) THEN
             FIND=2.
          ELSE
             FIND=FINDR
          END IF
*
          DO 799 I=1,IX1
             TS=TS+TP(I)
  799     CONTINUE
          BMIN=TS+(FIND*TWSUPC)+30.
          IX1=IX1+1
          CALL AVTRAL(M,IX1,RK1,TP,AVTR,BMIN)
       END IF
*
       N1=N-1
       DO 899 I=1,N1
       I1=N+1-I
       TSTEG=0.
 900   IF (RK1(I1)+B3(I1).GE.RK1(I1-1)+B3(I1-1)) GOTO 999
          TSTEG=TSTEG+BSTEP
          IF (TSTEG.GT.2.*BSTEP) GOTO 999
             B3(I1-1)=B3(I1-1)-BSTEP
             GOTO 900
 999      CONTINUE
 899   CONTINUE
*
       IF (EOKF.NE.0.) THEN
          AREA1=0.
          AREA2=0.
          DO 1199 I=1,N
             AREA1=AREA1+B1(I)*TP(I)
             AREA2=AREA2+B3(I)*TP(I)
 1199     CONTINUE
*
          AOKERF=EOKF*AREA1
*
          IF (AREA2.LT.AOKERF) THEN
             OH=B3(N)
             I=1
 1300        IF (I.GT.N) GOTO 1399
                I1=N+1-I
                IF (B3(I1)+RK1(I1).LT.OH) GOTO 1399
                   B3(I1)=B3(I1)+BSTEP
                   AREA2=AREA2+TP(I1)*BSTEP
                   N1=N
                   I=I+1
                   GOTO 1300
 1399        CONTINUE
*
             IF (AREA2.LT.AOKERF) THEN
             I=1
 1500        IF (I.GT.N1) GOTO 1599
                I1=N1+1-I
                B3(I1)=B3(I1)+BSTEP
                AREA2=AREA2+BSTEP*TP(I1)
                IF (AREA2.GE.AOKERF) GOTO 1599
                   I=I+1
                   GOTO 1500
 1599           CONTINUE
             END IF
          END IF
       END IF
*
       N1=N-1
       I=1
 1600  IF (I.GT.N1) GOTO 1699
 1700     IF (RK1(I+1)+B3(I+1).GE.RK1(I)+B3(I)) GOTO 1799
             B3(I+1)=B3(I+1)+BSTEP
             GOTO 1700
 1799     CONTINUE
          I=I+1
          GOTO 1600
 1699  CONTINUE
*
       IF (OHRED.GE.2.) THEN
          OKM=B3(N)-OHRED
          I=1
 1900     IF (I.GT.N) GOTO 1999
             IF (RK1(I)+B3(I).LE.OKM) THEN
                I=I+1
             ELSE
                IX=I
                I=N+1
             END IF
             GOTO 1900
 1999     CONTINUE
*
          DO 2199 I=IX,N
             B3(I)=OKM-RK1(I)
 2199     CONTINUE
*
          AREA1=0.
          AREA2=0.
          DO 2299 I=1,N
             AREA1=AREA1+B1(I)*TP(I)
             AREA2=AREA2+B3(I)*TP(I)
 2299     CONTINUE
*
          I=1
 2300     IF (I.GT.N1) GOTO 2399
             I1=IX+1-I
             IF (AREA2.GE.AREA1) THEN
                I=N1+1
             ELSE
                IF (RK1(I1)+B3(I1).GE.OKM) THEN
                   I=I+1
                ELSE
                   B3(I1)=B3(I1)+BSTEP
                   AREA2=AREA2+BSTEP*TP(I1)
                END IF
             END IF
             GOTO 2300
 2399     CONTINUE
       END IF
*
       B3(1)=B3(2)
       IF (DCOR.GE.700.) THEN
          DO 2799 I=1,3
            B3(I)=RK1(4)+B3(4)-RK1(I)
 2799     CONTINUE
       END IF
*
       RETURN
       END
