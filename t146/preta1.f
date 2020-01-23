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
*      Subroutine PRETA1
*      -----------------
*
C...   Title:  Prepar data for the TA1 core routines
*
*      WRITTEN: 91-09-06 BY J JOHANSSON  SETFO/TS
*      REVISED: 92-04-09 by B-G Bladh    SETFO/TS
*
************************************************************************

       SUBROUTINE PRETA1(DLIMB, HLIMB, CORBND, TOPOIL, BLIMB,
     &                   FREKV,TCORST,
     &                   DN, NK, FHD, IFLPMT,
     &                   MWB, ACCL, ACCV, TASEC, KBAND,ETA,BSTEPI,
     &                   BPLMAI,OVRLPI,NCOOLI,NCOOLW)


       include'com1.h'

       EQUIVALENCE
     &             (XZ1527    ,ISTGRD     ),
     &             (XZ0581 (1), UARR   (1)),
     &             (XZ0791 (1), FILLF  (1)),
     &             (XZ0671 (1), BDUCT  (1)),
     &             (XZ1001 (1), RRWDG  (1)),
     &             (XZ0751 (1), DYOKE  (1)),
     &             (XZ1486    , JFC       ),
     &             (XZ1508    , NWILI     )
*
       EQUIVALENCE
     &             (XZ2752  , LAMTH       ),
     &             (XZ2753  , LAMSPF      ),
     &             (XZ2754  , LAMMW       ),
     &             (XZ2755  , LAMOVL      ),
     &             (XZ2756  , ACCLON      ),
     &             (XZ2757  , ACCTRA      ),
     &             (XZ2758  , ACCVER      ),
     &             (XZ2759  , AMBTEM      ),
     &             (XZ2760  , LAMSTP      ),
     &             (XZ2761  , LOSCOR      ),
     &             (XZ2762  , TA1HOL      ),
     &             (XZ2770  , FLPLMA      )
*
       REAL DLIMB, HLIMB, TOPOIL, BLIMB, FREKV, TCORST,
     &      MLMB, DN,  FHD,  MWB, ACCL, ACCV, TASEC,
     &      ETA, BSTEPI,BPLMAI,OVRLPI,AMBTEM,
     &      LAMTH, LAMSPF, LAMMW, LAMOVL, ACCLON, ACCTRA, ACCVER,
     &      LAMSTP, LOSCOR, TA1HOL, TOPABS,
     &      FILLF(9), BDUCT(9), RRWDG(9), DYOKE(9)

       INTEGER NK, KBAND, IFLPMT, NCOOLI, NCOOLW,  NWILI, JFC
*
       DIMENSION
     &       UARR(100)
*
       CHARACTER
     &       FLPLMA*8, UARR*4, CORBND*8, CBBAND*5
*
       LOGICAL
     &       TSTP
*
C... Transfere data from input form
*
       TCORST  = LAMTH
       FHD     = TA1HOL
       ACCV    = ACCVER
       ACCL    = ACCLON
       ETA     = LAMSPF
       BSTEPI  = LAMSTP
       BPLMAI  = LAMMW
       OVRLPI  = LAMOVL
       TOPABS  = AMBTEM + TOPOIL
       TSTP    = (UARR(23) .EQ. 'YES ' .OR. UARR(23).EQ.'Y   ')
*
C... Estimate mass of winding block / limb
*
       MWB = 0.
       DOW = DLIMB
       DO 1099 I =1, NWILI
          IF(RRWDG(I) .GT. 0. ) THEN
             RR = RRWDG(I)
          ELSE
             RR = 0.
          ENDIF

          DIW = DOW + 2.*BDUCT(I)
          DWN = DIW + RR
          DOW = DWN + RR
          WCRA = RR*(HLIMB - DYOKE(I))
          WWGHT = WCRA*3.14159*DWN*FILLF(I)*8.9E3
          MWB = MWB + WWGHT
 1099  CONTINUE

C...Estimate mass of one limb

       MLMB = DLIMB*DLIMB/4.*3.14159*HLIMB*0.87*7650.

C...Find net diameter

       CBBAND = CORBND(1:5)
       IF(CBBAND .EQ. 'ASEC ') THEN
          KBAND = 1
       ELSE
          KBAND = 0
       ENDIF

       DN = DLIMB - DNTODK(DLIMB, MLMB, ACCTRA, HLIMB, CBBAND, TASEC)


C...Cooling Ducts

CCC    NCOOLW = NCDTA1(DN,TOPABS,BLIMB,FREKV,ISTGRD,TSTP,JFC)
*
          IF (NCOOLI.LT.0) THEN
              NK = NCOOLW
          ELSE
              NK = NCOOLI
          ENDIF
*
C
C...  Flitch plate
*
       IFLPMT  =  1
       IF(FLPLMA.EQ.'OX812   ') IFLPMT = 2
*
       RETURN
       END
