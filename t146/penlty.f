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
*      Subroutine PENLTY
*      -----------------
*
C...   Title:  Calculation of penalty costs for the
C...           optimisation routine
*
*      Written: XX-XX-XX by A.N.Other  , XXXX
*      Revised: 83-06-13 by H.Westberg , ZKAB
*      Revised: 85-10-18 by Ron Bell   , TRAFO/IK
*      Revised: 88-02-10 by Ron Bell   , TRAFO/IK
*
************************************************************************
*
       SUBROUTINE PENLTY(G)
*
C... Declarations
*
CC*SBBL Start of the declarations block
*
       include'com1.h'
*
       EQUIVALENCE
     &  (XZ0681(1)    ,  BPART  (1)    ),
     &  (XZ0871(1)    ,  HPART  (1)    ),
     &  (XZ0701(1)    ,  CURDEN (1)    ),
     &  (XZ0711(1)    ,  CURDM  (1)    ),
     &  (XZ0971(1)    ,  PRESSM (1)    ),
     &  (XZ1041(1)    ,  STRWMM (1)    ),
     &  (XZ1051(1)    ,  STRWMP (1)    )
       EQUIVALENCE
     &  (XZ1081(1)    ,  TENSM  (1)    ),
     &  (XZ1405       ,  BLIMB         ),
     &  (XZ1406       ,  BLTOPM        ),
     &  (XZ1410       ,  BTANK         ),
     &  (XZ1411       ,  BTANKM        )
       EQUIVALENCE
     &  (XZ1424       ,  DCOMAX        ),
     &  (XZ1425       ,  DCOMIN        ),
     &  (XZ1426       ,  DCORE         ),
     &  (XZ1466       ,  GTRP          ),
     &  (XZ1467       ,  GTRPM         )
       EQUIVALENCE
     &  (XZ1472       ,  HLIMB         ),
     &  (XZ1473       ,  HLMBMA        ),
     &  (XZ1474       ,  HTANK         ),
     &  (XZ1475       ,  HTANKM        ),
     &  (XZ1478       ,  HLMBMI        ),
     &  (XZ1508       ,  NWILI         )
       EQUIVALENCE
     &  (XZ1516       ,  PLOADM        ),
     &  (XZ1520       ,  PNOLOM        ),
     &  (XZ1528       ,  RLTANK        ),
     &  (XZ1529       ,  RLTNKM        ),
     &  (XZ1543       ,  SOUNDM        )
       EQUIVALENCE
     &  (XZ1561       ,  USHORE        ),
     &  (XZ1571       ,  ZWOULI        ),
     &  (XZ1650(1)    ,  P00    (1)    ),
     &  (XZ1686(1)    ,  SOUND0 (1)    ),
     &  (XZ1689(1,1,1),  URC    (1,1,1))
       EQUIVALENCE
     &  (XZ1690(1,1,1),  UXC    (1,1,1)),
     &  (XZ2501       ,  VALUEM        ),
     &  (XZ2502       ,  VALUEO        )
       EQUIVALENCE
     &  (XZ2597(1)    ,  HPRTMN (1)    ),
     &  (XZ2598(1)    ,  HPRTMX (1)    ),
     &  (XZ2599(1)    ,  BPRTMN (1)    ),
     &  (XZ2600(1)    ,  BPRTMX (1)    )
*
       REAL      CURDM(9),CURDEN(9),URC(4,4,3),UXC(4,4,3),SOUND0(3),
     &           PRESSM(9),TENSM(9),STRWMM(9),STRWMP(9),P00(3),G(*),
     &           BPART(9),HPART(9),HPRTMX(9),HPRTMN(9),BPRTMX(9),
     &           BPRTMN(9),BLIMB,BLMIN,BLTOPM,BTANK,BTANKM,GTRP,GTRPM,
     &           DCOMAX,DCOMIN,DCORE,HLIMB,HLMBMA,HTANK,HTANKM,PLOADM,
     &           PNOLOM,RLTANK,RLTNKM,SOUNDM,USHORE,VALUEM,VALUEO,
     &           ZWOULI,HLMBMI
*
       INTEGER   IWDG,NWILI
*
CC*SEBL End of the declarations block
*
C... Allocate values to the G-vector
*
CC*SBBL Start of the first G-block
*
       G( 1)=URC(1,1,1)*ZWOULI -PLOADM
       G( 2)=P00(1)    -PNOLOM
       G( 3)=(DCORE-DCOMAX)*(DCORE-DCOMIN)/(DCOMAX-DCOMIN)
       G( 4)=VALUEO    -VALUEM
       G( 5)=HTANK     -HTANKM
       G( 6)=(HLIMB-HLMBMA)*(HLIMB-HLMBMI)/(HLMBMA-HLMBMI)
       G( 7)=BTANK     -BTANKM
       G( 8)=RLTANK    -RLTNKM
       G( 9)=GTRP      -GTRPM
       G(10)=SOUND0(2) -SOUNDM
       G(11)=UXC(1,1,1)-USHORE
*
C--- BLMIN is the minimum flux density practicable.
*
       BLMIN=0.8
*
       G(12)=(BLIMB-BLTOPM)*(BLIMB-BLMIN)/(BLTOPM-BLMIN)
*
CC*SEBL End of the first G-block
*
C... Allocate values to the G-vector for each winding
*
C,,, PRESSM values are negative
*
       write(*,*) "penltyC.CURDEN",CURDEN
       write(*,*) "penltyC.CURDM",CURDM

       DO 299 IWDG=1,NWILI
       G(13+3*(IWDG-1))=CURDEN(IWDG)-CURDM(IWDG)
       G(14+3*(IWDG-1))=PRESSM(IWDG)-STRWMM(IWDG)
       G(15+3*(IWDG-1))=STRWMP(IWDG)-TENSM(IWDG)
  299  CONTINUE
*
       write(*,*)"G-PENLTY",(G(i),i=1,9)
       RETURN
       END
