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
*      Function DNTODK
*      ---------------
*
*  0.  Written: 91-08-31 by J. Johansson    , SETFO/TS
*      Revised: 91-11-27 by Ron Bell        , ABB Power T & D Muncie
*
*  1.  Description
C...       Title: Difference between Nominal diameter and Net Diameter
C...              for the TA1 core.
*
************************************************************************
*
       REAL FUNCTION DNTODK(DNI,RMLMB,ACCTR,HLIMBI,CBBAND,TASEC,JFC)
*
C... DNI     Net Diameter (m)
C... RMLMB   Mass of limbs (kg)
C... ACCTR   Transport force in transverse direction (*G)
C... HLIMBI  Limb Height (m)
C... HLIMB   Limb Height (mm)
C... CBBAND  Limb banding ASEC, PRESS
*
       REAL      DNI,DN,RMLMB,HLIMBI,HLIMB,ACCTR,TASEC,EA,EP,TLPC,
     &           A,TOL,T,T1,T2,N1,N2
*
       INTEGER   JFC
*
       CHARACTER CBBAND*5
*
       LOGICAL   BBERR

       include'consts.h'
*
       BBERR = .FALSE.

       EA = 4250.
       EP = 1500.
*
C... Convert to (mm)
*
       DN = DNI*1000.
       HLIMB = HLIMBI*1000.
       write(*,*) 'dntodk Line 59 =', HLIMB
*
       IF (DN.GT.600.) THEN
          TOL = 6.
       ELSE
          TOL = 4.
       END IF

       T2 = 0.
       TLPC = 0.
*
       IF (CBBAND.EQ.'ASEC') THEN
          N1=((RMLMB/3.*GFORCE*ACCTR*HLIMB*HLIMB*167.)/
     &       (PI*EA)+DN*DN*DN*DN)**0.25-DN
          N1 = MAX(N1,2.)
          N2 = N1 + 1.
          T1 = 0.5*N1
          T2 = 0.5*N2
          TASEC = T1/1000.
       ELSE IF (CBBAND.EQ.'PRESS') THEN
          TLPC = ((RMLMB/3.*GFORCE*ACCTR*HLIMB*HLIMB*167.)/
     &           (PI*EP)+DN*DN*DN*DN)**0.25-DN
          TASEC = 0.
       ELSE
          TASEC = 0.
          WRITE(JFC,*) 'Error in DNtoDK: Illegal limb banding ',
     &                CBBAND
          BBERR = .TRUE.
       END IF
*
       IF (BBERR) THEN
          DNTODK = 0.
          BBERR = .FALSE.
       ELSE
          T = MAX(T2,TLPC)
          A = NINT(2.*(0.5+T)+TOL)
          DNTODK = A/1000.
       END IF
*
       RETURN
       END
