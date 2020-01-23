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
*      Subroutine PRCONS
*      -----------------
*
C...   Title:  Prints concentrated output (PG0) in time-sharing
C...           environment. This is printed before Page 2.
*
*      Written: XX-XX-XX by A.N.Other  , XXXX
*      Revised: 85-10-18 by Ron Bell   , TRAFO/IK
*      Revised: 88-02-10 by Ron Bell   , TRAFO/IK
*
************************************************************************
*
       SUBROUTINE PRCONS
*
C... Declarations
*
CC*SBBL Start of the declarations block.
*
       include'com1.h'
*
       EQUIVALENCE
     &  (XZ0651(1)    ,  ACOND  (1)    ),
     &  (XZ0731(1)    ,  CURRUT (1)    ),
     &  (XZ0741(1)    ,  DWIND  (1)    ),
     &  (XZ0881(1)    ,  HWIND  (1)    ),
     &  (XZ1001(1)    ,  RRWDG  (1)    )
       EQUIVALENCE
     &  (XZ1041(1)    ,  STRWMM (1)    ),
     &  (XZ1051(1)    ,  STRWMP (1)    ),
     &  (XZ1191(1)    ,  ZWIND  (1)    ),
     &  (XZ1371       ,  BB132         )
       EQUIVALENCE
     &  (XZ1379       ,  BBERR1        ),
     &  (XZ1393       ,  BBVFR         ),
     &  (XZ1405       ,  BLIMB         ),
     &  (XZ1407       ,  BMAXPU        )
       EQUIVALENCE
     &  (XZ1408       ,  BMINPU        ),
     &  (XZ1410       ,  BTANK         ),
     &  (XZ1414       ,  CLOSSV        ),
     &  (XZ1415       ,  COST          ),
     &  (XZ1422       ,  CTROVN        )
       EQUIVALENCE
     &  (XZ1426       ,  DCORE         ),
     &  (XZ1466       ,  GTRP          ),
     &  (XZ1472       ,  HLIMB         ),
     &  (XZ1474       ,  HTANK         )
       EQUIVALENCE
     &  (XZ1486       ,  JFC           ),
     &  (XZ1508       ,  NWILI         ),
     &  (XZ1509       ,  NWOULI        ),
     &  (XZ1515       ,  PLIMB         )
       EQUIVALENCE
     &  (XZ1528       ,  RLTANK        ),
     &  (XZ1587       ,  CHCOST        ),
     &  (XZ1650(1)    ,  P00    (1)    ),
     &  (XZ1686(1)    ,  SOUND0 (1)    )
       EQUIVALENCE
     &  (XZ2245(1)    ,  WLOSSE (1)    ),
     &  (XZ2254(1)    ,  WLOSSR (1)    ),
     &  (XZ2298(1)    ,  NCOL   (1)    ),
     &  (XZ2562       ,  BBFREQ        )
*
       REAL      ACOND(9),CURDUT(9),P00(3),DIAI(9),RRWDG(9),
     &           STRWMM(9),STRWMP(9),ZWIND(9),CURRUT(9),
     &           DWIND(9),HWIND(9),WLOSSE(9),WLOSSR(9),SOUND0(3),
     &           BLIMB,BMAXPU,BMINPU,BTANK,CLOSSV,COST,CTROVN,DCORE,
     &           GTRP,HLIMB,HTANK,PL,PLIMB,RLTANK
*
       INTEGER   I,IWDG,JFC,NWILI,NWOULI
*
       LOGICAL   BBVFR,BBERR1,BB132,BBFREQ
*
       DIMENSION NCOL(9),WTYPE1(9)
       CHARACTER CHCOST*6,NCOL*1,CHSOUN*16,WTYPE1*4
*
CC*SEBL End of the declarations block.
*
C... Create the required character string for each winding
*
       DO 699 IWDG=1,NWILI
       WTYPE1(IWDG)='    '
*
C... Create the string
*
       CALL CONCAT(WTYPE1(IWDG),1,NCOL(IWDG),1,1)
       DIAI(IWDG)=DWIND(IWDG)-RRWDG(IWDG)
  699  CURDUT(IWDG)=CURRUT(IWDG)/ACOND(IWDG)
*
C... Print a new page heading
*
       CALL NPAGE(0)
*
C... Print the object identification
*
       WRITE(JFC,81)
*
C... Print the errors , if any
*
       IF (BBERR1.OR.BBFREQ) CALL ERRRUT(BBERR1,BBFREQ,JFC)
*
C... Print CHARACTER indata
*
       CALL  PRWT('Winding  ',WTYPE1,JFC,NWILI)
*
C... Print REAL indata (3rd parameter defines No. of decimals)
*
       CALL  PRWA('No. Turns',ZWIND ,1,JFC,NWILI)
*
C... Print REAL indata (3rd parameter defines No. of decimals)
C...                   (4th parameter defines a multiplication factor)
*
       CALL  PRWR('Cond.Area',ACOND ,1,1.E+6,JFC,NWILI)
*
CC*SOFF Switch off structure analyser (Not all calls need be shown)
*
       CALL  PRWR('A/mm2    ',CURDUT,2,1.E-6,JFC,NWILI)
       CALL  PRWR('InnerDiam',DIAI  ,0,1.E+3,JFC,NWILI)
       CALL  PRWR('Width    ',RRWDG ,0,1.E+3,JFC,NWILI)
       CALL  PRWR('Height   ',HWIND ,0,1.E+3,JFC,NWILI)
       CALL  PRWR('N/mm2+   ',STRWMP,0,1.E-6,JFC,NWILI)
       CALL  PRWR('N/mm2-   ',STRWMM,0,-1.E-6,JFC,NWILI)
       CALL  PRWR('RII  Loss',WLOSSR,1,1.E-3,JFC,NWILI)
       CALL  PRWR('EDDY Loss',WLOSSE,1,1.E-3,JFC,NWILI)
*
CC*SON  Switch on structure analyser (Not all calls need be shown)
*
       PL=PLIMB
       IF (NWOULI.EQ.1) PL=0.
       CHSOUN='Sound Level'
       IF (BB132) CHSOUN='Sound(SCRN 10dB)'
*
       WRITE(JFC,82) 1.E+3*BTANK,NINT(1.E+3*DCORE),1.E+3*RLTANK,GTRP
*
C... Print further details
*
C,,, Constant flux regulation
*
       IF (.NOT.BBVFR) THEN
          WRITE(JFC,83)
     &    'Tank Height',1.E+3*HTANK,' mm',
     &    'Flux Density    ',BLIMB,' T',
     &    'Limb Height',1.E+3*HLIMB,' mm',
     &    'No-load Losses  ',P00(1)/1.E+3,' kW',
     &    'Limb Pitch ',1.E+3*PL,' mm',CHSOUN,SOUND0(1),' dB'
*
C... Variable flux regulation
*
       ELSE
          WRITE(JFC,84) 1.E+3*HTANK,BLIMB*BMAXPU,
     &                  BLIMB*BMINPU,BLIMB,
     &                  1.E+3*HLIMB,(P00(I)/1.E+3,I=2,3),P00(1)/1.E+3,
     &                  1.E+3*PL,CHSOUN,SOUND0(2),SOUND0(3),SOUND0(1)
       END IF
*
C... Print voltages, S/C voltages & losses
*
       CALL PRUKPK
       WRITE(JFC,85) CTROVN,CHCOST,CLOSSV+COST,CHCOST
       RETURN
*
   81  FORMAT('0','PG0  ','S U M M A R Y   O F   R E S U L T S',/,'0')
   82  FORMAT(1X/ 6X,'Tank Width ',F10.0,' mm',
     &                        4X,'Core Diameter',19X,I7,' mm'/
     &            6X,'Tank Length',F10.0,' mm',
     &                        4X,'Transport    ',19X,F7.0,' kg')
   83  FORMAT(6X,A11,F10.0,A3,4X,A16,16X,F7.4,A3,/
     &        6X,A11,F10.0,A3,4X,A16,16X,F7.1,A3,/
     &        6X,A11,F10.0,A3,4X,A16,16X,F7.1,A3,/)
   84  FORMAT(6X,'Tank Height',F10.0,' mm',
     &                    4X,'Flux Density      ',3F7.4,'  T'/
     &        6X,'Limb Height',F10.0,' mm',
     &                    4X,'No-load Loss      ',3F7.1,' kW'/
     &        6X,'Limb Pitch ',F10.0,' mm',
     &                    4X,A16,2X,3F7.1,' dB',/)
   85  FORMAT (6X,'OVN/Comp.Price ',10X,F14.0,1X,A4,F14.0,1X,A4)
       END
