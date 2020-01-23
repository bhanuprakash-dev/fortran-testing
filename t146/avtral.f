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
*      Subroutine AVTRAL
*      -----------------
*
*  0.  Written: XX-XX-XX by S-E Jansson      , XXXX
*      Revised: 86-12-09 by J. Akesson       , Z1KDC
*      Revised: 91-11-27 by Ron Bell         , ABB Power T & D Muncie
*
*  1.  Description
C...       Title:  Calculates the steps in the core-section
C...               against the winding
*
************************************************************************
*
       SUBROUTINE AVTRAL(N,IX1,RK1,TP,AVTR,BMIN)
*
       REAL      AVTR,BMIN,BX
*
       REAL      RK1(N),TP(0:N)
*
       INTEGER   I,N,IX,IX1
*
       IX=0
       BX=0.
       I=1
  200  IF (I.LE.N) THEN
          BX=BX+TP(I)
          IF (BX.GE.BMIN) THEN
             IX=I
             I=N+1
          ELSE
             IX=I
             I=I+1
          END IF
*
          GOTO 200
       END IF
*
       DO 499 I=IX1,IX
          IF (RK1(I).LT.0.1) RK1(I)=AVTR
  499  CONTINUE
*
       IX1=IX
*
       RETURN
       END
