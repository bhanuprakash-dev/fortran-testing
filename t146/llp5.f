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
*      Subroutine LLP5
*      ---------------
*
C...   Title: Performs calculations over rows
C...          to determine the pivot element
*
*      Written: XX-XX-XX by A.N.Other  , XXXX
*      Revised: 85-10-18 by Ron Bell   , TRAFO/IK
*      Revised: 88-02-10 by Ron Bell   , TRAFO/IK
*
************************************************************************
*
       SUBROUTINE LLP5(E,NA,IMAX,IND1,JM,K,LCOL,PHIMAX,TD,DELMAX,JMAX)
*
C... Local vectors:  DELMAX(M+1),JMAX(M+1)
C... Declarations
*
CC*SBBL Start of the declarations block.
*
       DIMENSION E(NA,*),IND1(*),DELMAX(*),JMAX(*)
*
CC*SEBL End of the declarations block.
*
C--- PHIMAX is set equal to a small number for initialisation purposes
*
       PHIMAX=-1E6
       IMAX=0
       KK=K-1
       LL=LCOL-1
*
C... Calculate
*
       DO 20 K1=1,KK
       KL=IND1(K1)
       JMAX(KL)=0
       DELMAX(KL)=-1E6
*
C... Calculate
*
       DO 10 J=1,LL
*
C... Calculate
*
C,,, If E(KL,J) <= -TD
*
       IF(E(KL,J).LE.(-TD)) THEN
*
C... Calculate
*
C,,, If E(1,J) >= -TD
*
          IF(E(1,J).GE.(-TD)) THEN
             DELTA=E(1,J)/E(KL,J)
*
C... Calculate
*
C,,, If DELTA > DELMAX(KL)
*
             IF(DELTA.GT.DELMAX(KL)) THEN
                DELMAX(KL)=DELTA
                JMAX(KL)=J
             END IF
          END IF
       END IF
   10  CONTINUE
*
       PHI=DELMAX(KL)*E(KL,LCOL)
       write(*,*) "PHI=",PHI
       write(*,*) "DELMAX=",(DELMAX(i),i=1,9)
       write(*,*) "KL=",KL
       write(*,*) "LCOL=",LCOL
       write(*,*) "E(KL,LCOL)=",E(KL,LCOL)
       IF(DELMAX(KL).EQ.(-1E6)) PHI=-1E8
*
C... Calculate
*
C,,, If PHI > PHIMAX
*
       IF(PHI.GT.PHIMAX) THEN
          PHIMAX=PHI
          IMAX=IND1(K1)
       END IF
   20  CONTINUE
*
       write(*,*) "PHIMAX-LLP5=",PHIMAX
       IF(IMAX.GT.0) JM=JMAX(IMAX)
*
       RETURN
       END
