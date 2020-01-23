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
*      SUBROUTINE COR130
*      -----------------
*
C...   TITLE:  CALCULATION OF CORE DATA FOR "130 KV CORE"
*
*      TRANSFERRED FROM PROGRAM R31009  1991-09-03
*                        BY B-G BLADH      , SETFO/TS
*      Revised 93-01-26 J Johansson SETFO/TS
*          Used Glued core for all cores blow 553 mm, also
*          if cooling ducts are used.
*          In case of cooling ducts use formula for banded
*          core, but add 3 mm to the diameter when calculating
*          the area.
*
************************************************************************
*
       SUBROUTINE COR130(ZLIMB, DCORE, NPHAS, FREQ, BLIMB, UMAXPU,
     &                   HLIMB, PLIMB,
     &                   ACORE,ANDEL,BDRAG,GCORN,GLIMBM,GLIMBY,GYOKE,
     &                   HYOKE,NCOOLC,RDRAG,RLYOKE,TASEC,TCORE,
     &                   TDRAG,U0,YOKAMP,CORBND,KBAND,STKFAC,
     &                   NCOOLI, NCOOLW,SPFADJ)
*
C... Declarations
*
CC*SBBL Start of the declarations block.
*
       REAL
     & ZLIMB, DCORE, FREQ, BLIMB,UMAXPU, PI, GCORN, GDLIMB,
     & GDYOKE, HYOKE, YOKAMP, FCONST, BLIM2, BLIM, FILLF, ACORE,
     & GSPYOC, U0, GSPLIM, AYOKE, SQR2, RAAFEC, GLIMBM, GYOKE, GCORLA,
     & HLIMB, PLIMB, STKFAC, SPFADJ
*
       INTEGER
     & NPHAS, NCOOLC, KBAND, NCOOLI, NCOOLW
*
       CHARACTER
     &  CORBND*8
*
       LOGICAL
     & BBGLCO
*
CC*SEBL End of the declarations block.
*
       PI     = 3.14159
       SQR2   = 1.41421
       RAAFEC = 7650.
*
*****
*  START  OF COPY FROM THE OLD ROUTINE CORE1 FROM R31009
*****

       GCORN  = ZLIMB*(5193.96+DCORE*(-62085.41+DCORE*(288812.02+
     &       DCORE*(-642959.03+DCORE*(713974.2-DCORE*304515.75)))))
       GDLIMB = 25.
*
       IF (NPHAS.EQ.1) THEN
           GDYOKE = -0.500*GCORN
       ELSE
           GDYOKE = -0.667*GCORN
       ENDIF
*
       HYOKE  = 0.94*DCORE+0.025
       YOKAMP = 1.18785-DCORE*(0.403-0.271*DCORE)
       FCONST = 0.735+FREQ*5.3E-3
*       "FREQ" = 50 HZ ==> FCONST = 1.0
*       "FREQ" = 60 HZ ==> FCONST = 1.053
*..................................................................
*....  Number of cooling ducts
       BLIM2=BLIMB*UMAXPU
       BLIM   = (2.569-1.514*DCORE)/FCONST
       NCOOLW = 0
*
       IF(BLIM2.LT.BLIM) GO TO 500
       BLIM   = (2.742-1.514*DCORE)/FCONST
       NCOOLW = 1
*
       IF(BLIM2.LT.BLIM) GO TO 500
       BLIM   = (2.560-1.001*DCORE)/FCONST
       NCOOLW = 2
*
       IF(BLIM2.LT.BLIM) GO TO 500
       NCOOLW =3
*
 500   CONTINUE
*
       IF (NCOOLI.LT.0) THEN
           NCOOLC = NCOOLW
       ELSE
           NCOOLC = NCOOLI
       ENDIF
*
*..................................................................
*
* Next part rewritten, revision 93-01-26

C.Rev 93-05-08  IF(DCORE.LT.0.554) THEN
       IF(NINT(DCORE*1000.) .LT. 554) THEN
*** GLUED CORE
         BBGLCO = .TRUE.
         CORBND = 'GLUED   '
         KBAND  = 0

       ELSE
*** BANDAGED CORE
         BBGLCO=.FALSE.
         CORBND='STEEL   '
         KBAND = 2
       ENDIF

       Z     =FLOAT(NCOOLC)
*
       IF(NCOOLC .EQ. 0 .AND. BBGLCO) THEN

C... Glued core without cooling ducts

         IF(STKFAC.LT. 0.1) THEN
           FILLF  = 0.01042*DCORE+0.8677-0.011*EXP(-8.03*(DCORE-0.22))
         ELSE
           FILLF  = STKFAC
         ENDIF

C...Use nominal diameter for calculation of core area

	 DC = DCORE

       ELSE

C... All cores with cooling ducts

         IF(STKFAC.LT. 0.1) THEN
              FILLF = 0.09833*DCORE+0.8114
     &            -0.0068*(Z-2.)*(Z-3.)*Z/2.
     &            +0.0175*(Z-1.)*(Z-3.)*Z/2.
     &            -0.0259*(Z-1.)*(Z-2.)*Z/6.
         ELSE
               FILLF = STKFAC
         ENDIF
	  
         FILLF=FILLF/0.95*SPFADJ

C...Use nominal diameter + 3 mm for calculation of core area
C...for glued cores
C...Use nominal diameter for bandaged cores.

	 IF(BBGLCO) THEN
	   DC = DCORE + 0.003
	 ELSE
	   DC = DCORE
	 ENDIF
       ENDIF
*
*..................................................................
*
*
*..................................................................
       ACORE = PI*DC*DC/4.*FILLF

*End of Revision 93-01-26
*
       GSPYOC=367.
*****
*    END OF COPY FROM THE OLD ROUTINE  CORE1 FROM R31009
*****
*    FROM ROUTINE CORCAL IN R31009:
*****
       U0     = ACORE*2.*PI*FREQ/SQR2
       GSPLIM = ACORE*RAAFEC
       AYOKE  = YOKAMP*ACORE
       GSPYOK = 2.*AYOKE*RAAFEC*(ZLIMB-1.)
*****
*    FROM ROUTINE ACTPAR IN R31009:
*****
       GLIMBM = ZLIMB*(GSPLIM*HLIMB+GDLIMB)
       GYOKE  = GSPYOK*PLIMB+GDYOKE
       GCORLA = GLIMBM+GYOKE+GCORN
*****
*    NEW   :
*****
       RLYOKE = (ZLIMB-1.)*PLIMB + DCORE
       ANDEL  = 0.
       BDRAG  = 0.
       TDRAG  = 0.
       RDRAG  = 0.
       GLIMBY = 0.
       TASEC  = 0.
       TCORE  = 0.
*
       RETURN
       END
