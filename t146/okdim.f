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
*      Subroutine OKDIM
*      ----------------
*
*  0.  Written: XX-XX-XX by S-E Jansson       , XXXX
*      Revised: 86-12-09 by J. Akesson        , Z1KDC
*      Revised: 91-11-27 by Ron Bell          , ABB Power T & D Muncie
*
*  1.  Description
C...       Title:  Calculates the Yoke Dimensions
*
************************************************************************
*
       SUBROUTINE OKDIM(COM,X1,Y1,Y2,XG,OK)
*
       INTEGER     IANL,IX,J,IX1,IY,N,LFPL,LC,IS,I

       INTEGER     DCOR,X1(0:100),XG(100),OK(100)
*
       REAL        BSTEP,A,S,ANL,EL,H,TFLPL,BAND,EXTRA,HYLLA

       REAL        COM(110),Y1(0:100),Y2(100),TS(100)

       include'consts.h'
*
       DCOR  = NINT(COM(3))
       BAND  = COM(10)
       LFPL  = COM(22)
       BSTEP = ANINT(COM(33))
       TFLPL = COM(38)
       N     = COM(53)
       LC    = COM(99)
*
C... Calculate the total thickness
*
       TS(1)=Y1(1)
       DO 299 I=2,N
          TS(I)=TS(I-1)+Y1(I)
  299  CONTINUE
*
       IF (LC.EQ.0) GOTO 302
*
C... Core with yoke bolts
*
       DO 499 I=1,N
          XG(I)=0
  499  CONTINUE
       HYLLA=60.
       IF (BAND.GT.0.1) THEN
          EXTRA=50.
       ELSE
          EXTRA=0.
       END IF
*
       DO 699 I=1,N
          OK(I)=X1(I)+20
  699  CONTINUE
*
C... Corrugations of the yoke against the winding
C... in regards to winding support thickness
*
       I=1
  700  IF (OK(I)+LFPL.GT.OK(N).OR.I.GT.N) GOTO 799
          XG(I)=LFPL
          I=I+1
          GOTO 700
  799  CONTINUE
*
C... Check for irregularities on the upper side
*
       I=2
*
  800  IF (I.GT.N) GOTO 899
  900     IF (OK(I)+XG(I).GE.OK(I-1)+XG(I-1)) GOTO 999
             OK(I)=OK(I)+NINT(BSTEP)
             GOTO 900
  999     CONTINUE
          I=I+1
          GOTO 800
  899  CONTINUE
*
       I=1
 1000  IF (I.GT.N.OR.Y2(1)-Y2(I).GE.HYLLA) GOTO 1099
          IS=I
          I=I+1
          GOTO 1000
 1099  CONTINUE
*
       DO 1199 I=1,IS
          XG(I)=OK(IS)+LFPL+EXTRA-OK(I)
 1199  CONTINUE
*
C... Adjust openings for yoke bolts
*
       IF (DCOR.GT.700) THEN
          H=80.
       ELSE
          H=70.
       END IF
*
       DO 1399 I=1,N
          IF (XG(I).GT.(H+LFPL)) THEN
             OK(I)=XG(I)+OK(I)-H-LFPL
             XG(I)=H+LFPL
          END IF
 1399  CONTINUE
*
C... In the case 1st & 2nd yoke plates of equal width
*
       OK(1)=OK(2)
       XG(1)=XG(2)
*
C... Check space for yoke banding
*
       J=1
 1500  IF (J.GT.N) GOTO 1599
          IF (XG(J).GT.0.) THEN
             J=J+1
          ELSE
             I=J
             J=N+1
          END IF
          GOTO 1500
 1599  CONTINUE
*
       A=(35.+LFPL)/SIN20/COS20-100.
*
 3500  IF (A.LT.TS(I-1)+TFLPL+3.) GOTO 3599
          XG(I)=LFPL
          I=I+1
          GOTO 3500
 3599  CONTINUE
*
C... Check irregularities on upper side
*
 1700  DO 1799 I=2,N
*
 1800     IF (OK(I)+XG(I).GE.OK(I-1)+XG(I-1)) GOTO 1899
             OK(I)=OK(I)+NINT(BSTEP)
             GOTO 1800
 1899     CONTINUE
 1799  CONTINUE
       GOTO 399
*
C... Core with NO yoke bolt
*
  302  CONTINUE
       DO 1999 I=1,N
          XG(I)=0
 1999  CONTINUE
       IANL=0
       IX=0
       IX1=0
       IY=0
*
C... Constant overlap
*
       DO 2099 I=1,N
          OK(I)=X1(I)+20
 2099  CONTINUE
*
       I=1
 2100  IF (I.GT.N.OR.OK(I)+LFPL.GT.OK(N)) GOTO 2199
          IANL=I
          XG(I)=LFPL
          I=I+1
          GOTO 2100
 2199  CONTINUE
*
C... Height of winding plate re-inforcement
*
       EL=0.
*
       IF (LFPL.GT.65.) THEN
          IF (DCOR.GT.1200) THEN
             S=93.
          ELSE IF (DCOR.GE.1000) THEN
             S=93./200.*(DCOR-1000)
          ELSE
             S=0.
          END IF
       ELSE IF (LFPL.GT.50) THEN
          IF (DCOR.GE.1050) THEN
             S=93.
          ELSE IF (DCOR.GT.750) THEN
             S=93./300.*(DCOR-750)
          ELSE
             S=0.
          END IF
       ELSE
          IF (DCOR.GT.900) THEN
             S=93.
          ELSE
             S=93./400.*(DCOR-500)
          END IF
       END IF
*
       IF (S.GT.15.) THEN
          I=1
 2700     IF (I.GT.N) GOTO 2799
             EL=EL+Y1(I)
*
             IF (EL.LT.S) THEN
                I=I+1
             ELSE
                IX=I
                I=N+1
             END IF
*
             GOTO 2700
 2799     CONTINUE
          DO 2999 I=1,IX
             XG(I)=80+LFPL
 2999     CONTINUE
       ELSE
          S=0.
       END IF
*
C... Check space for yoke banding
*
       A=(35.+LFPL)/(SIN20/COS20)-100.
       I=0
*
 3000  IF (A.LT.TS(IANL)+TFLPL+3..OR.I.EQ.1) GOTO 3099
          IF (IANL.EQ.N-1) THEN
             WRITE(6,3111)
             I=1
          ELSE
             IANL=IANL+1
             XG(IANL)=LFPL
          END IF
          GOTO 3000
 3099  CONTINUE
*
C... Check contact area of winding support
*
       ANL=2.*LFPL
       I=0
*
 3200  IF (TS(IANL).GE.ANL.OR.I.EQ.1) GOTO 3299
          IF (IANL.EQ.N-1) THEN
             WRITE(6,3333)
             I=1
          ELSE
             IANL=IANL+1
             XG(IANL)=LFPL
          END IF
          GOTO 3200
 3299  CONTINUE
*
C... In the case of 1st & 2nd yoke plates of equal width
*
       OK(1)=OK(2)
       XG(1)=XG(2)
*
       DO 3499 I=2,N
*
 3600     IF (OK(I)+XG(I).GE.OK(I-1)+XG(I-1)) GOTO 3699
             OK(I)=OK(I)+NINT(BSTEP)
             GOTO 3600
 3699     CONTINUE
 3499  CONTINUE
*
       COM(96)=EL
*
  399  CONTINUE
  199  RETURN
*
 3111  FORMAT('0','Inner Yoke Banding will make contact with ',
     &   'yoke. Support is too thick for this Core Diameter.')
 3333  FORMAT('0','Winding Support Contact Area too small. ',
     &         'Support is too thick for this Core Diameter.')
       END
