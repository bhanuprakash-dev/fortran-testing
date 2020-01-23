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
*      Subroutine FRNAT
*      ----------------
*
*  0.  Written: XX-XX-XX by A.N. Other   , XXXX
*      Revised: 91-11-27 by Ron Bell     , ABB Power T & D Muncie
*
*  1.  Description
C...       Title:  Determines a resonant condition in the core
*
************************************************************************
*
       SUBROUTINE FRNAT(TYP,FREQ,DCORI,PLIMBI,HLIMBI,
     &                  BBRESO,FRNAT1,FRNAT2)
*
       REAL       FRNAT1,FRNAT2,R,X,Y,FG,DCORI,FREQ,TYP,PLIMBI,HLIMBI
*
       INTEGER    I

       LOGICAL    BBLIMT,BBRESO
*
       BBLIMT(X,Y)=(ABS(X*FREQ-Y).LE.10.)
*
       BBRESO=.FALSE.
       IF (TYP.NE.3.) THEN
          FG=2850.*DCORI/(HLIMBI+DCORI)**2*
     &        (1.-0.45*PLIMBI/(HLIMBI+DCORI))
          DO 50 I=2,6,2
             R=I
             IF (BBLIMT(R,FG)) BBRESO=.TRUE.
   50     CONTINUE
       ELSE
          CALL BRUM(NINT(DCORI*1000.),NINT(HLIMBI*1000.),
     &              FRNAT1,FRNAT2)
          DO 200 I=2,6,2
             R=I
             IF (I.LT.4) THEN
                IF (BBLIMT(R,FRNAT1).OR.
     &              BBLIMT(R,FRNAT2)) BBRESO=.TRUE.
             ELSE
                IF (BBLIMT(R,FRNAT1)) BBRESO=.TRUE.
             END IF
  200     CONTINUE
       END IF
*
  199  RETURN
       END
