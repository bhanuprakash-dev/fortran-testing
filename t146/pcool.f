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
*      Subroutine PCOOL
*      ----------------
*
C...   Title:  Calculation of approximate 3-winding losses
C...           for the cooling program.
C...           Calculation is done using the method given in,
C...           amongst others, the SEN standards.
C...           Maximum losses between respective terminals
C...           are the basis for the calculation, independent of
C...           whether they belong to the same tap-position.
*
*      Written: XX-XX-XX by A.N.Other  , XXXX
*      Revised: 85-10-18 by Ron Bell   , TRAFO/IK
*      Revised: 88-02-10 by Ron Bell   , TRAFO/IK
*
************************************************************************
*
       SUBROUTINE PCOOL(URC,FACT,NG,SRATE,PKCOOL)
*
C... Declarations
*
CC*SBBL Start of the declarations block.
*
       REAL      URC(4,4,3),SRATE(4),FACT,
     &           PKCOOL,P1,P12,P13,P2,P23,P3,SUM,S12,S13,S23,
     &           X12,X13,X23
*
       INTEGER   I,J,NG
*
CC*SEBL End of the declarations block.
*
       S12=AMIN1(SRATE(1),SRATE(2))
       IF (S12.LT.1.) S12=1.
       S13=AMIN1(SRATE(1),SRATE(3))
       IF (S13.LT.1.) S13=1.
       S23=AMIN1(SRATE(2),SRATE(3))
       IF (S23.LT.1.) S23=1.
*
C... Establish some quantities
*
       P12=0.
       P13=0.
       P23=0.
       DO 299 I=1,4
*
C... For each terminal and tap
*
       DO 299 J=1,4
       X12=URC(I,J,1)
       X13=URC(I,J,2)
       X23=URC(I,J,3)
       IF (X12.GT.P12) P12=X12
       IF (X13.GT.P13) P13=X13
       IF (X23.GT.P23) P23=X23
  299  CONTINUE
*
C... Calculate PKCOOL
*
C,,, If only 2 terminals
*
       IF (NG.LE.2) THEN
          PKCOOL=P12
*
C... If more than 2 terminals
*
       ELSE
          P12=P12/S12**2
          P13=P13/S13**2
          P23=P23/S23**2
          SUM=0.5*(P12+P13+P23)
          P1=SUM-P23
          P2=SUM-P13
          P3=SUM-P12
          PKCOOL=P1*(SRATE(1))**2+P2*(SRATE(2))**2+P3*(SRATE(3))**2
       END IF
*
       PKCOOL=PKCOOL*FACT
*
       RETURN
       END
