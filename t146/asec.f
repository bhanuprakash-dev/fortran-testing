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
*      Subroutine ASEC
*      ---------------
*
*  0.  Written: XX-XX-XX by S-E. Jansson    , ZKB
*      Revised: 86-12-09 by J. Akesson      , Z1KDC
*      Revised: 89-01-09 by G. Rasmussen    , TRAFO/IK
*      Revised: 91-11-27 by Ron Bell        , ABB Power T & D Muncie
*
*  1.  Description
C...       Title: Core Stockings of ASECOND band
C...              according to ZK-TA 4610-115
*
************************************************************************
*
       SUBROUTINE ASEC(DCOR,HLIMB,ACCTR,
     &                 N1C,N2C,T1C,T2C,RLCC,RLKRMC,RMC,
     &                 N1O,N2O,T1O,T2O,RLCO,RLKRMO,RMO)
*
*
       REAL       HLIMB,DCOR,DKX,DY,T1,T1C,T1O,T2,T2C,T2O,
     &            D1K,D2K,D3K,D4K,
     &            ACCTR,RLCC,RLKRMC,RMC,RLCO,RLKRMO,RMO,DKHLP(2)
*
       INTEGER    I,N1,N1C,N1O,N2O,N2C,N1X,N1Y,N1Z,N2

       include'consts.h'

       DY=DCOR/SQRT(2.)
*
C... Shrinking-tape
*
       RLKRMC=(HLIMB-120.)/25.*PI*DCOR*1.E-3
       RLKRMO=RLKRMC*DY/DCOR
*
C... Crepe paper
*
       RMC=6./15.*(HLIMB-120.)*PI*DCOR*1.E-6
       RMO=RMC*DY/DCOR
*
C... Number of ASECOND-layers in the limb-centre
*
       DKHLP(1)=DCOR
*
       DO 299 I=1,2
          DKHLP(2)=DCOR
          DKX=DKHLP(I)
          IF (I.EQ.1.) THEN
             D1K=5.26
             D2K=1.67
             D3K=1.50
          ELSE
             D1K=4.21
             D2K=0.95
             D3K=1.20
          END IF
*
          N1X=ACCTR*HLIMB*HLIMB*D1K*1E-7+0.99999
          N1Y=ACCTR*HLIMB*DKX*D2K*1E-6+0.99999
          N1Z=ACCTR*HLIMB*HLIMB*HLIMB/DKX*D3K*1E-7+0.99999
          N1 =MAX0(N1X,N1Y,N1Z,2)
*
C... Number of ASECOND-layers at the end of the cylinder
*
          IF (I.EQ.1.) THEN
             D4K=1.93
          ELSE
             D4K=0.96
          END IF
*
          N2=ACCTR*DKX*DKX*HLIMB*D4K*1E-9+0.99999
          N2=MAX0(N1,N2)
*
C... Corresponding Thicknesses
*
          T1 =0.5*N1
          T2 =0.5*N2
*
          DKHLP(2)=DY
          DKX=DKHLP(I)
          IF (I.EQ.1) THEN
             N1C=N1
             N2C=N2
             T1C=T1
             T2C=T2
             RLCC=PI*DKX*((HLIMB-120.)/60.*N1C+5.*(N2C-N1C))*1.E-3
          ELSE IF (I.EQ.2) THEN
             N1O=N1
             N2O=N2
             T1O=T1
             T2O=T2
             RLCO=PI*DKX*((HLIMB-120.)/60.*N1O+5.*(N2O-N1O))*1.E-3
          END IF
  299  CONTINUE
*
       RETURN
       END
