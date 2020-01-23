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
*      Subroutine VOLTAG
*      -----------------
*
C...   Title:  Assign dimensioning voltages to the
C...         array "UN", with the parameters
C...         "I" and "J", "UN(I,J).
C...         where "I" is the actual voltage group
C...           and "J" is the actual voltage regulation
C...      Position "J" = 1 ==> Nominal position
C...               "J" = 2 ==> Extreme PLUS position
C..                "J" = 3 ==> Extreme MINUS position
C...               "J" = 4 ==> Extra (arbitrary) position
C...  Relative flux densities( BMAXPU, BMINPU) are also calculated
*
*      Written: XX-XX-XX by A.N.Other  , XXXX
*      Revised: 85-10-18 by Ron Bell   , TRAFO/IK
*      Revised: 88-02-10 by Ron Bell   , TRAFO/IK
*
************************************************************************
*
       SUBROUTINE VOLTAG
*
C... Declarations
*
CC*SBBL Start of the declarations block.
*
       include'com1.h'
*
       EQUIVALENCE
     &  (XZ0821(1)    ,  FRMPOS (1)    ),
     &  (XZ0831(1)    ,  FRPPOS (1)    ),
     &  (XZ0841(1)    ,  FRZPOS (1)    ),
     &  (XZ0901(1)    ,  KGROUP (1)    ),
     &  (XZ1171(1)    ,  ZTALMW (1)    )
       EQUIVALENCE
     &  (XZ1181(1)    ,  ZTALRW (1)    ),
     &  (XZ1267(1)    ,  UDIM   (1)    ),
     &  (XZ1271(1,1)  ,  UN     (1,1)  ),
     &  (XZ1373       ,  BBAUTO        ),
     &  (XZ1392       ,  BBUPTR        )
       EQUIVALENCE
     &  (XZ1393       ,  BBVFR         ),
     &  (XZ1407       ,  BMAXPU        ),
     &  (XZ1408       ,  BMINPU        ),
     &  (XZ1499       ,  NG            ),
     &  (XZ1500       ,  NPG           )
       EQUIVALENCE
     &  (XZ1504       ,  NSG           ),
     &  (XZ1508       ,  NWILI         ),
     &  (XZ3004(1)    ,  FRXPOS (1)    )
*
       REAL      UDIM(4),ZTALRW(9),ZTALMW(9),FRZPOS(9),FRXPOS(6),
     &           FRPPOS(9),FRMPOS(9),UN(4,4),TM(4),TR(4,4),UTURN(4),
     &           BMAXPU,BMINPU,X
*
       INTEGER   KGROUP(9),ITML,IWDG,JS,JTAP,KTML,NG,NPG,NSG,NWILI
*
       LOGICAL   BBAUTO,BBUPTR,BBVFR
*
CC*SEBL End of the declarations block.
*
C... Initialisation of certain variables
*
CC*SBBL Start of the initialisation block.
*
C... For each terminal
*
       DO 299 ITML=1,4
       TM(ITML)=0.
*
C... For each tap
*
       DO 299 JTAP=1,4
       UN(ITML,JTAP)=0.
  299  TR(ITML,JTAP)=0
*
CC*SEBL End of the initialisation block.
*
C... For each winding
*
       DO 399 IWDG=1,NWILI
*
C... Summation of relative turns for the different terminals for
C... the different tap-positions
*
CC*SBBL Start of the summation block.
*
       KTML=KGROUP(IWDG)
       TR(KTML,1)=TR(KTML,1)+ZTALRW(IWDG)*FRZPOS(IWDG)
       TR(KTML,2)=TR(KTML,2)+ZTALRW(IWDG)*FRPPOS(IWDG)
       TR(KTML,3)=TR(KTML,3)+ZTALRW(IWDG)*FRMPOS(IWDG)
       TR(KTML,4)=TR(KTML,4)+ZTALRW(IWDG)*FRXPOS(IWDG)
       IF (ZTALMW(IWDG).GT.0.) TM(KTML)=ZTALMW(IWDG)
*
CC*SEBL End of the summation block.
*
  399  CONTINUE
*
C... Calculation of normalised turn voltage "UTURN" for the different
C... tap positions and relative flux densities
*
       JS=1
       IF(BBVFR) JS=4
*
C... Calculate UTURN for each tap
*
       DO 499  JTAP=1,JS
       X=TM(1)+TR(1,JTAP)
       IF(BBAUTO.AND..NOT.BBUPTR)  X=X+TM(2)
       IF(BBAUTO.AND..NOT.BBUPTR.AND.BBVFR)  X=X+TR(2,JTAP)
       UTURN(JTAP)=UDIM(1)/X
  499  CONTINUE
*
C... Calculation of relative flux density at
C... Extreme PLUS & MINUS positions
*
C... Calculate max & min B
*
C,,, VFR
*
       IF(BBVFR) THEN
*
C... Calculate max & min B
*
C,,, If UTURN(2) > UTURN(1)
*
          IF (UTURN(2).GE.UTURN(1)) THEN
             BMAXPU=UTURN(2)/UTURN(1)
             BMINPU=UTURN(3)/UTURN(1)
*
C... If UTURN(2) <= UTURN(1)
*
          ELSE
             BMAXPU=UTURN(3)/UTURN(1)
             BMINPU=UTURN(2)/UTURN(1)
          END IF
*
C... CFR
*
       ELSE
          UTURN(4)=UTURN(1)
          UTURN(3)=UTURN(1)
          UTURN(2)=UTURN(1)
          BMAXPU=1.
          BMINPU=1.
       END IF
*
C... Calculate UN
*
C*SBBL Start of the UN block
*
C... For each tap
*
       DO 799 JTAP=1,4
*
C... For each terminal
*
       DO 899 ITML=1,NG
       UN(ITML,JTAP)=(TM(ITML)+TR(ITML,JTAP))*UTURN(JTAP)
  899  CONTINUE
*
       IF(BBAUTO)
     &       UN(NSG,JTAP)=UN(NSG,JTAP)+TM(NPG)*UTURN(JTAP)
       IF(BBAUTO.AND.BBVFR)
     &       UN(NSG,JTAP)=UN(NSG,JTAP)+TR(NPG,JTAP)*UTURN(JTAP)
*
  799  CONTINUE
*
C*SEBL End of the UN block
*
       RETURN
       END
