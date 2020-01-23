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
*      Function POLY
*      -------------
*
*  0.  Written: XX-XX-XX by A.N. Other   , XXXX
*      Revised: 91-11-27 by Ron Bell     , ABB Power T & D Muncie
*
*  1.  Description
C...       Title:  Calculates the value of a polynomial at point X
C...               using Horner's method.
C...               The co-efficients are stored in A with
C...               the constant term first.
C...               N is the degree of the polynomial.
C...               N + 1 = Number of co-efficients.
*
************************************************************************
*
       REAL FUNCTION POLY(A,X,N)
*
       INTEGER I,N
*
       REAL X
*
       REAL A(N)
*
       write(*,*)"POLY"
       write(*,*)"A=",A
       IF (N.EQ.1) THEN
          POLY=A(N)
       ELSE
          POLY=A(N)
          DO 399 I=2,N
             POLY=POLY*X+A(N-I+1)
  399     CONTINUE
       END IF
*
       RETURN
       END
