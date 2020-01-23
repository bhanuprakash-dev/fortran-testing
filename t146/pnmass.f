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
*      Subroutine PNMASS
*      -----------------
*
C...   Title:   Calculate masses of presspahn in the active part

*      Written: 92-01-30 by B-G Bladh  , SETFO/TS
*      Revised: ....................   , ........
*
************************************************************************
*
       SUBROUTINE PNMASS
*
*
C... Declarations
*
       include'com1.h'
       REAL      PI, RAAPN, ZWOULI, HLIMB, WD, DD, HD,
     &           PNFDUC, PNFWIN, PNFYOK, PNDUCT, PNWIND, PNYOKE,
     &           DWIND, DYOKE, RRWDG, FILLF, BDUCT
*
       DIMENSION DWIND(9),DYOKE(9),RRWDG(9),FILLF(9), BDUCT(9)
*
       INTEGER   IW,NWILI,NWOULI, JFC
*
       EQUIVALENCE
     &  (XZ1472       ,  HLIMB         ),
     &  (XZ1486       ,  JFC           ),
     &  (XZ1509       ,  NWOULI        ),
     &  (XZ1508       ,  NWILI         ),
     &  (XZ1513       ,  PI            )
       EQUIVALENCE
     &  (XZ0741(1)    ,  DWIND  (1)    ),
     &  (XZ0751(1)    ,  DYOKE  (1)    ),
     &  (XZ1001(1)    ,  RRWDG  (1)    ),
     &  (XZ0791(1)    ,  FILLF  (1)    ),
     &  (XZ0671(1)    ,  BDUCT  (1)    )
*
       EQUIVALENCE
     &  (XZ2746       ,  PNFDUC        ),
     &  (XZ2747       ,  PNFWIN        ),
     &  (XZ2748       ,  PNFYOK        ),
     &  (XZ2749       ,  PNDUCT        ),
     &  (XZ2750       ,  PNWIND        ),
     &  (XZ2751       ,  PNYOKE        )
*
*      PNFDUC = Fraction of main duct volume which contains presspahn
*      PNFWIN = Fraction of winding volume, excluding copper volume
*                        which contains presspahn.
*      PNFYOK = Fraction of yoke insulation volume
*                        which contains presspahn.
*      PNDUCT = Mass of presspahn in main ducts  (kg).
*      PNWIND = Mass of presspahn in windings (kg).
*      PNYOKE = Mass of presspahn in yoke insulation  (kg).
*      Output masses cover all wound limbs and all windings.
*
*
C... Pressphan density (kg/m3)
*
       RAAPN = 1220.
       ZWOULI= FLOAT(NWOULI)
*
       PNDUCT = 0.
       PNWIND = 0.
       PNYOKE = 0.
*
*
       DO 200 IW=1,NWILI
*
C... DUCT.................
*
C... Segment width
*
          WD = BDUCT(IW)
*
C... Segment mean diameter
*
          DD = DWIND(IW) - RRWDG(IW) - WD
*
C... Segment height
*
          HD = HLIMB - DYOKE(IW)
*
C... Presspahn mass
*
          PNDUCT = PNDUCT + PNFDUC*WD*DD*PI*HD*RAAPN*ZWOULI
*
*
C... WINDINGS................
*
C... Segment width
*
          WD = RRWDG(IW)
*
C... Segment mean diameter
*
          DD = DWIND(IW)
*
C... Segment height
*
          HD = HLIMB - DYOKE(IW)
*
C... Presspahn mass
*
          PNWIND=PNWIND+PNFWIN*WD*DD*PI*HD*RAAPN*(1.-FILLF(IW))*ZWOULI
*
*
*
C... YOKE ...................
*
C... Segment width
*
          WD = RRWDG(IW) + BDUCT(IW)
*
C... Segment mean diameter
*
          DD = DWIND(IW) - BDUCT(IW)
*
C... Segment height
*
          HD = DYOKE(IW)
*
C... Presspahn mass
*
          PNYOKE = PNYOKE + PNFYOK*WD*DD*PI*HD*RAAPN * ZWOULI
*
 200   CONTINUE
*
       RETURN
       END
