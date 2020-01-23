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
*      Subroutine PRNLD
*      ----------------
*
C...   Title:  Output of the no-load characteristics.
*
*      Written: XX-XX-XX by A.N.Other  , XXXX
*      Revised: 85-10-18 by Ron Bell   , TRAFO/IK
*      Revised: 88-02-10 by Ron Bell   , TRAFO/IK
*
************************************************************************
*
       SUBROUTINE PRNLD(BBRES)
*
C... Declarations
*
CC*SBBL Start of the declarations block.
*
       include'com1.h'
*
       EQUIVALENCE
     &  (XZ1259(1)    ,  SRATE  (1)    ),
     &  (XZ1405       ,  BLIMB         ),
     &  (XZ1486       ,  JFC           ),
     &  (XZ1650(1)    ,  P00    (1)    ),
     &  (XZ1651(1)    ,  PNOLO  (1)    ),
     &  (XZ1653(1)    ,  Q00    (1)    ),
     &  (XZ1654(1)    ,  RINOLO (1)    )
       EQUIVALENCE
     &  (XZ1686(1)    ,  SOUND0 (1)    ),
     &  (XZ1687(1)    ,  SOUND  (1)    ),
     &  (XZ2763       ,  SPRED         )
*
       REAL    P00(3),Q00(3),SOUND0(3),SRATE(4),BLIMB,BLIMB9,
     &         PNOLO(5),QNOLO(5),SOUND(5),RINOLO(5),SPRED
*
       INTEGER JCOUNT(5),ILEVEL,JLEVEL,JFC
*
       LOGICAL BBRESC,BBRES
*
CC*SEBL End of the declarations block.
*
       DATA JCOUNT / 1, 2, 4, 5, 3 /
*
C... Print a new page heading
*
       CALL  NPAGE(0)
*
       WRITE (JFC,10) '     NO-LOAD CONDITIONS  (Principal tap)'
*
       BBRESC=.FALSE.
       BBRES=.FALSE.
       BLIMB9=BLIMB
*
C... Calculate no-load losses for 5 flux densities
*
       DO 299 JLEVEL=1,5
       ILEVEL=JCOUNT(JLEVEL)
       BLIMB=BLIMB9*(0.85+0.05*FLOAT(ILEVEL))
*
C--- BBRESC=.TRUE. because the resonance calculation is needed only
C---            after the last loop (See FUNK)
*
       IF (JLEVEL.EQ.5) BBRESC=.TRUE.
*
C... Calculate no-load losses
*
       write(*,*)"PRNLD>>NLOADN"
       CALL NLOADN(BBRESC,BBRES)
*
       PNOLO(ILEVEL)=P00(1)
       QNOLO(ILEVEL)=Q00(1)
       SOUND(ILEVEL)=SOUND0(1)
  299  RINOLO(ILEVEL)=Q00(1)/SRATE(1)
*
       BLIMB=BLIMB9
*
C--- Index = 3 corresponds to the nominal position
*
       WRITE (JFC,11) BLIMB,(ILEVEL,ILEVEL=90,110,5)
       WRITE (JFC,12) (PNOLO(ILEVEL)/1.E+3,ILEVEL=1,5)
       WRITE (JFC,13) (1.E+2*RINOLO(ILEVEL),ILEVEL=1,5)
       WRITE (JFC,14) (SOUND(ILEVEL),ILEVEL=1,5)
       WRITE (JFC,15) (SOUND(ILEVEL)-SPRED,ILEVEL=1,5)
       RETURN
*
   10  FORMAT(1X,A)
   11  FORMAT(1X/6X,'Induction (% of ',F5.3,' T)',5I9)
   12  FORMAT(6X,'No-load losses......(kW)',5F9.1)
   13  FORMAT(6X,'Magnetizing current..(%)',5F9.2)
   14  FORMAT(6X,'Sound Power Level.. (dB)',5F9.1)
   15  FORMAT(6X,'Sound Pressure Level(dB)',5F9.1)
*
       END
