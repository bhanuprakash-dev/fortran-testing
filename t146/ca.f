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
*
************************************************************************
*
*
       program ca
C... Declarations
*
CC*SBBL Start of the declarations block.
*
       REAL
     & ZLIMB, DCORE, FREQ, BLIMB,UMAXPU, PI, GCORN, GDLIMB,
     & GDYOKE, HYOKE, YOKAMP, FCONST, BLIM2, BLIM, FILLF, ACORE,
     & GSPYOC, U0, GSPLIM, AYOKE, SQR2, RAAFEC, GLIMBM, GYOKE, GCORLA,
     & HLIMB, PLIMB, STKFAC
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

       freq = 60.
       FCONST = 0.735+FREQ*5.3E-3
*       "FREQ" = 50 HZ ==> FCONST = 1.0
*       "FREQ" = 60 HZ ==> FCONST = 1.053
*..................................................................
       ncoolc = 0
       stkfac = 0.
       write(*,*) '***cor130 ncoolc',ncoolc
*
*..................................................................
*
       do 1099 d = 250., 600., 20.
       dcore=d/1000.
*** BANDAGED CORE
         Z     =FLOAT(NCOOLC)
*
              FILLFB = 0.09833*DCORE+0.8114
     &            -0.0068*(Z-2.)*(Z-3.)*Z/2.
     &            +0.0175*(Z-1.)*(Z-3.)*Z/2.
     &            -0.0259*(Z-1.)*(Z-2.)*Z/6.
*
*..................................................................
*** GLUED CORE
*
           dcg=dcore
           dcb=dcore +0.003
           FILLFG  = 0.01042*DCORE+0.8677-0.011*EXP(-8.03*(DCORE-0.22))
       ACOREG = PI*dcg*dcg/4.*FILLFG
       ACOREB = PI*dcb*dcb/4.*FILLFB

       write(*,2000) dcore*1000.,acoreg*10000.,acoreb*10000.
*
 1099   continue
*..................................................................
*
       stop
2000   format('dcore ',f5.1, ' acoreg ',f7.2,' acoreb ',f7.2)
       END
