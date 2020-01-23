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
*      Subroutine PRECOR
*      -----------------
*
*  0.  Written: XX-XX-XX by S-E Jansson      , ZKB
*      Revised: 86-12-09 by J. Akesson       , Z1KDC
*      Revised: 91-11-27 by Ron Bell         , ABB Power T & D Muncie
*
*  1.  Description
C...       Title:  Calculates some input data required by
C...               the subroutine CORE
*
************************************************************************
*
       SUBROUTINE PRECOR(DCOR,HLIMB,KSERIE,ACCTR,DCORN,B1MAX,B1MIN,
     &                   T1,T2,BT,TT,BBFLPL,RNFLPL,BFLPL,TFLPL,
     &                   KOD,BSMAX,BSTEP,TYP,TCD)
*
       REAL      RCORDA,ACCTR,RNFLPL,BSTEP,B1M,B1MAX,B1MIN,BT,BFLPL,
     &           BSMAX,DCOR,DCORN,H,HLP,HS,HX,R,S,T1,T2,TFLPL,THLP,
     &           TCD,TT,TYP,HLIMB
*
       REAL      TCAD(6),FLPL(15)
*
       INTEGER   J,KOD,KSERIE

       LOGICAL   BBFLPL
*
       FLPL( 1)= 2.
       FLPL( 2)=75.
       FLPL( 3)=10.
       FLPL( 4)= 2.
       FLPL( 5)=75.
       FLPL( 6)=12.
       FLPL( 7)= 2.
       FLPL( 8)=75.
       FLPL( 9)=15.
       FLPL(10)= 4.
       FLPL(11)=60.
       FLPL(12)=12.
       FLPL(13)= 4.
       FLPL(14)=75.
       FLPL(15)=12.
*
       TCAD(1)= 4.
       TCAD(2)=75.
       TCAD(3)=12.
       TCAD(4)= 6.
       TCAD(5)=75.
       TCAD(6)=12.
*
C... Net Diameter, Maximum Limb-plate width
C... Number of and space for flitch plates
*
       IF (TYP.GE.7.) THEN
          R=0.5
          THLP=TCD
       ELSE
          R=1.
          THLP=0.
       END IF
*
 300   GOTO(301,302,303,304,305,301),KSERIE
*
C... TAA and TAC
*
 301   CALL DNET(KSERIE,KOD,HLIMB,ACCTR,DCOR,DCORN,T1,T2)
       IF (BBFLPL) THEN
          IF (DCOR.GE.700.) THEN
             B1MAX=DCOR-34.9
          ELSE
             B1MAX=DCOR-29.9
          END IF
       ELSE
          IF (DCOR.GT.400.) THEN
             S=45.
          ELSE
             S=25.
          END IF
*
          H=0.5*DCORN-0.5*SQRT(DCORN*DCORN-S*S)
          B1MAX=DCORN-2.*H
       END IF
*
       B1MAX=10.*AINT(B1MAX*0.1)
       HLP=ABS(BSMAX-B1MAX)
       B1MAX=B1MAX-AMOD(HLP,BSTEP)
       BT=0.
       TT=0.
*
       IF (.NOT.BBFLPL) THEN
          IF (KOD.EQ.1) THEN
             IF (RNFLPL.LT.4.) THEN
                BT=80.
             ELSE IF (.NOT.(RNFLPL.GE.4..AND.
     &                       BFLPL.EQ.75.)) THEN
                BT=130.
             ELSE
                BT=160.
             END IF
*
             TT=2.+TFLPL+2.+0.5
          ELSE
             BT=INT(RNFLPL)/2*BFLPL+INT(RNFLPL)/2*5-5
             TT=TFLPL+2.
          END IF
       END IF
*
       B1M=90.
       GOTO 399
*
C... TBA
*
 302   CALL DNET(KSERIE,KOD,HLIMB,ACCTR,DCOR,DCORN,T1,T2)
*
       IF (DCOR.GE.700.) THEN
          B1MAX=DCOR-34.9
       ELSE
          B1MAX=DCOR-29.9
       END IF
*
       B1MAX=10.*AINT(B1MAX*0.1)
       HLP=ABS(BSMAX-B1MAX)
       B1MAX=B1MAX-AMOD(HLP,BSTEP)
       BT=0.
       TT=0.
*
       IF (.NOT.BBFLPL) THEN
          IF (DCOR.LT.360.) THEN
             J=1
          ELSE IF (DCOR.LT.450.) THEN
             J=4
          ELSE IF (DCOR.LT.590.) THEN
             J=7
          ELSE IF (DCOR.LT.805.) THEN
             J=10
          ELSE
             J=13
          END IF
*
          RNFLPL=FLPL(J)
          BFLPL=FLPL(J+1)
          TFLPL=FLPL(J+2)
*
       END IF
*
       BT=INT(RNFLPL)/2*BFLPL+INT(RNFLPL)/2*5-5
       IF (KOD.EQ.1) THEN
          TT=2.+TFLPL+2.
       ELSE
          TT=TFLPL+2.
       END IF
*
       B1M=60.
       GOTO 399
*
C... TAA-M
*
 303   CALL DNET(KSERIE,KOD,HLIMB,ACCTR,DCOR,DCORN,T1,T2)
*
       IF (BBFLPL) THEN
          IF (DCOR.GE.700.) THEN
             B1MAX=DCOR-34.9
          ELSE
             B1MAX=DCOR-29.9
          END IF
       ELSE
          IF (DCOR.GT.400.) THEN
             S=45.
          ELSE
             S=25.
          END IF
*
          H=0.5*DCORN-0.5*SQRT(DCORN*DCORN-S*S)
          B1MAX=DCORN-2.*H
       END IF
*
       B1MAX=10.*AINT(B1MAX*0.1)
       HLP=ABS(BSMAX-B1MAX)
       B1MAX=B1MAX-AMOD(HLP,BSTEP)
       BT=0.
       TT=0.
*
       IF (.NOT.BBFLPL) THEN
          IF (KOD.EQ.1) THEN
             IF (RNFLPL.LT.4.) THEN
                BT=80.
             ELSE IF (BFLPL.NE.75.) THEN
                BT=130.
             ELSE
                BT=160.
             END IF
*
             TT=2.+TFLPL+2.+0.5
          ELSE
             BT=INT(RNFLPL)/2*BFLPL+INT(RNFLPL)/2*5-5
             TT=TFLPL+2.
          END IF
       END IF
*
       B1M=90.
       GOTO 399
*
C... TCA
*
 304   CALL DNET(KSERIE,KOD,HLIMB,ACCTR,DCOR,DCORN,T1,T2)
*
       IF (DCOR.GE.700.) THEN
          B1MAX=R*(DCOR-34.9-THLP)
       ELSE
          B1MAX=R*(DCOR-29.9-THLP)
       END IF
*
       B1MAX=10.*AINT(B1MAX*0.1)
       HLP=ABS(BSMAX-B1MAX)
       B1MAX=B1MAX-AMOD(HLP,BSTEP)
*
       IF (.NOT.BBFLPL) THEN
          IF (DCOR.GT.1040.) THEN
             J=4
          ELSE
             J=1
          END IF
*
          RNFLPL=TCAD(J)
          BFLPL=TCAD(J+1)
          TFLPL=TCAD(J+2)
       END IF
*
       BT=INT(RNFLPL)/2*BFLPL+INT(RNFLPL)/2*10-10
       TT=TFLPL+6.
       B1M=100.
       GOTO 399
*
C... Divided Core
*
 305   CONTINUE
 399   CONTINUE
*
C... Minimum Core Plate Width in the limb
*
       HX=0.5*DCORN-0.5*SQRT(DCORN*DCORN-BT*BT)
       HS=HX+TT
       RCORDA=SQRT(4.*((0.5*DCORN)**2-(0.5*DCORN-HS)**2))
       B1MIN=10.*AINT((R*RCORDA-R*THLP)*0.1+1.)
       B1MIN=AMAX1(B1MIN,B1M)
*
       RETURN
*
       END
