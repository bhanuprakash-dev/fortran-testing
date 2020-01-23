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
*      Subroutine PAGE01
*      - - - - - - - - -
*
C...   Title:  Results output for Page 1
*
*      Written: XX-XX-XX by A.N.Other      , XXXX
*      Revised: 85-10-18 by Ron Bell       , TRAFO/IK
*      Revised: 88-02-10 by Ron Bell       , TRAFO/IK
*      Revised: 89-10-02 by B-G Bladh      , SETFO/K1
*      Revised: 90-02-12 by Per Sãderberg  , SETFO/IK
*      Revised: 91-09-06 by B-G Bladh      , SETFO/TS
*      Revised: 91-12-10 by B-G Bladh      , SETFO/TS
*              Printout order changed. Pagebreak changed.
************************************************************************
*
       SUBROUTINE PAGE01
*
C... Declarations
*
CC*SBBL Start of the declarations block.
*
       include'com1.h'
*
       EQUIVALENCE
     &  (XZ0001(1)    ,  AARR   (1)    ),
     &  (XZ0401(1)    ,  TARR   (1)    ),
     &  (XZ0581(1)    ,  UARR   (1)    ),
     &  (XZ1227(1)    ,  NMSTEP (1)    ),
     &  (XZ1231(1)    ,  NPSTEP (1)    ),
     &  (XZ1239(1)    ,  PUSTEP (1)    ),
     &  (XZ1255(1)    ,  SLINE  (1)    )
       EQUIVALENCE
     &  (XZ1259(1)    ,  SRATE  (1)    ),
     &  (XZ1283(1)    ,  UNLINE (1)    ),
     &  (XZ1287(1)    ,  USURG  (1)    ),
     &  (XZ1366(1)    ,  BBVR   (1)    ),
     &  (XZ1378       ,  BBERR         ),
     &  (XZ1311(1)    ,  BBHELP (1)    ),
     &  (XZ1371       ,  BB132         )
       EQUIVALENCE
     &  (XZ1390       ,  BBSLIM        ),
     &  (XZ1391       ,  BBTS          ),
     &  (XZ1393       ,  BBVFR         )
       EQUIVALENCE
     &  (XZ1396       ,  ACORE         ),
     &  (XZ1405       ,  BLIMB         ),
     &  (XZ1407       ,  BMAXPU        ),
     &  (XZ1408       ,  BMINPU        ),
     &  (XZ1426       ,  DCORE         ),
     &  (XZ1533       ,  DN            )
       EQUIVALENCE
     &  (XZ1446       ,  FREQ          ),
     &  (XZ1483       ,  MNLY          ),
     &  (XZ1486       ,  JFC           ),
     &  (XZ1491       ,  KCORE         ),
     &  (XZ1495       ,  NCONST        ),
     &  (XZ1496       ,  NCOOLC        )
       EQUIVALENCE
     &  (XZ1499       ,  NG            ),
     &  (XZ1501       ,  NPHAS         ),
     &  (XZ1509       ,  NWOULI        ),
     &  (XZ1527       ,  ISTGRD        ),
     &  (XZ1553       ,  TTOILC        ),
     &  (XZ1554       ,  TTOILM        )
       EQUIVALENCE
     &  (XZ1556       ,  TWINDM        ),
     &  (XZ1559       ,  U0            ),
     &  (XZ1567       ,  YOKAMP        ),
     &  (XZ1572       ,  CHCORE        )
       EQUIVALENCE
     &  (XZ1650(1)    ,  P00    (1)    ),
     &  (XZ1653(1)    ,  Q00    (1)    ),
     &  (XZ1686(1)    ,  SOUND0 (1)    ),
     &  (XZ2145(1)    ,  SAVE1  (1)    ),
     &  (XZ2295       ,  CORBND        ),
     &  (XZ2701       ,  EXTCOR        ),
     &  (XZ2726       ,  LAMID         ),
     &  (XZ2781       ,  NCOOLW        )
****
       REAL      PUSTEP(4),P00(3),Q00(3),ACORE,BLIMB,BMAXPU,BMINPU,
     &           SLINE(4),SRATE(4),SOUND0(3),UNLINE(4),USURG(4),
     &           DCORE,FREQ,TTOILM,TTOILC,TWINDM,U0,YOKAMP,AARR(200),
     &           DN
*
       INTEGER   NMSTEP(4),NPSTEP(4),I,ITML,JFC,KCORE,NCONST,NCOOLC,
     &           NG,NLIMB,NPHAS,NWOULI,MNLY,ISTGRD,NCOOLW
*
       DIMENSION SAVE1(50), TARR(100), UARR(100)
       CHARACTER STEEL*8,SAVE1*7,CHSOUN*16,
     &           TARR*8,CORBND*8,BAND*8,CHCORE*60,
     &           TEXT67*70,PRTYP*5,UARR*4, BLADN*4, LAMID*44
*
       LOGICAL   BBSLIM,BBTS,BBVFR,BBVR(4),BB132,
     &           BBERR,BBHELP(10), EXTCOR
*
CC*SEBL End of the declarations block.
*
       DATA TEXT67
     & /'PAGE01:Ind.<0.8T : No-load losses and magn.power not correct'/
*
C... Calculation of winding currents, current densities & voltages
*
       print*,'inside page01'
       CALL CURDNS
       print*,'after curdns'
       CALL NPAGE(0)
       print*,'after npage'
*
C... Page 2
*
*
CC*SOFF Switch off structure analyser (Not all calls need be shown)
*
       WRITE(JFC,102)'R A T I N G   P L A T E   D A T A: ',
     &   '- - - - - - - - - - - - - - - - - - -'
*
       NLIMB=NWOULI
       IF(BBSLIM) NLIMB=2+NWOULI
       WRITE(JFC,83) NPHAS,NLIMB,INT(FREQ),TARR(56),
     &             INT(TWINDM),INT(TTOILM)
*
       WRITE(JFC,84)
*
CC*SON Switch on structure analyser (Not all calls need be shown)
*
C... Print data
*
       DO 399 ITML=1,NG
*
C... For each terminal
*
C,,, If regulated
*
       IF (BBVR(ITML)) THEN
          WRITE(JFC,86) SRATE(ITML)/1.E+6,UNLINE(ITML)/1.E+3,
     &    NPSTEP(ITML),NMSTEP(ITML),1.E+2*PUSTEP(ITML),TARR(20+ITML),
     &    NINT(USURG(ITML)/1.E+3),SLINE(ITML)/1.E+9
*
C... If not regulated
*
       ELSE
          WRITE(JFC,85) SRATE(ITML)/1.E+6,UNLINE(ITML)/1.E+3,
     &       TARR(20+ITML),NINT(USURG(ITML)/1.E+3),SLINE(ITML)/1.E+9
       END IF
  399  CONTINUE
*
       WRITE(JFC,102)'C O R E   D A T A:',
     &   ' - - - - - - - - - - - - - - - - - - - - - - - - - - -'
       WRITE(JFC,87) CHCORE
*
C... Print error message if required
*
       IF(BLIMB*BMINPU.LT.0.8) CALL FPRINT(1,BBERR,BBHELP,BBTS,JFC,
     &                                     TEXT67)
       STEEL='Normal'
       IF(ISTGRD.EQ.2) STEEL='Super '
       IF(ISTGRD.EQ.3) STEEL=' ZDKH '
       IF(ISTGRD.EQ.4) STEEL=' PLJT '
*
       WRITE(JFC,88) STEEL,' Oriented Steel',
     &             'Volts/Turn ',U0*50./FREQ,BLIMB,FREQ/50.,U0*BLIMB
*
C... Print details
*
C,,, If variable flux regulation
*
       IF(BBVFR) THEN
          WRITE(JFC,90) 'Diameter    ',NINT(1.E+3*DCORE),' mm ',
     &    'Flux Density',BLIMB*BMAXPU,BLIMB*BMINPU,BLIMB,' T    '
*
C... If constant flux regulation
*
       ELSE
          WRITE(JFC,89) 'Diameter    ',NINT(1.E+3*DCORE),' mm ',
     &    'Flux Density',BLIMB,' T    '
       END IF

C... Show also net diameter for TA1 core

       IF(KCORE .EQ. 101) THEN
          WRITE(JFC,89) 'Diameter NET',NINT(1.E+3*DN),' mm '
       ENDIF
*
       CHSOUN           ='Sound Power lev.'
       IF (BB132) CHSOUN='SndPw(SCRN 10dB)'
*
C... Print further details
*
C,,, If variable flux regulation
*
       IF (BBVFR) THEN
          WRITE(JFC,92) 'Area        ',NINT(1.E+4*ACORE),' cm2',
     &    CHSOUN,(SOUND0(I),I=2,3),SOUND0(1),' dB(A)'
*
C... If constant flux regulation
*
       ELSE
          WRITE(JFC,91) 'Area        ',NINT(1.E+4*ACORE),' cm2',
     &    CHSOUN,SOUND0(1),' dB(A)'
       END IF
*
C... Print further details
*
C... If variable flux regulation
*
       IF (BBVFR) THEN
          WRITE(JFC,94) 'Yoke Reinfo ',(YOKAMP*100.-100.),' %  ',
     &    'No-load Loss',(P00(I)/1.E+3,I=2,3),P00(1)/1.E+3,' kW   '
*
C... If constant flux regulation
*
       ELSE
          WRITE(JFC,93) 'Yoke Reinfo ',(YOKAMP*100.-100.),' %  ',
     &    'No-load Loss',P00(1)/1.E+3,' kW   '
       END IF
*
       BAND='ASECOND '
       IF(CORBND(1:1).EQ.'S') BAND='Steel   '
       IF(CORBND(1:1).EQ.'G') BAND='Glued   '
       IF(CORBND(1:1).EQ.'P') BAND='Presspan'
       WRITE(JFC,95) BAND,Q00(1)/1.E+3,1.E+2*Q00(1)/SRATE(1)
       WRITE(JFC,96) UARR(23)
       IF(AARR(140).GT.3.) THEN
             BLADN = '4/4 '
          ELSE
             BLADN = '2/2 '
       ENDIF
       IF (EXTCOR) THEN
          WRITE(JFC,100) BLADN
          WRITE(JFC,101) NINT(TTOILC), LAMID
       ELSE
          WRITE(JFC,98) BLADN
          WRITE(JFC,99) NINT(TTOILC)
       ENDIF
*
C...      Cooling ducts in core
       WRITE(JFC,103) NCOOLW, NCOOLC
*
C... Cost report
*
       IF(MNLY.GT.91) THEN
           CALL PAGC92
       ELSE
               print*,'before pagecr'
           CALL PAGECR
       ENDIF
*
C... Limits
*
       WRITE(JFC,102) 'N O T E S:',
     & ' - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -'
       print*,'before plimit'
       CALL PLIMIT(2)
*
C... Winding data & diverse masses
*
       print*,'before npage'
       CALL NPAGE(0)
*
       print*,'before prwida'
       CALL PRWIDA
*
       WRITE(JFC,97)
*
C... Operating characteristics
*
       print*,'before prupk'
       CALL PRUKPK
       print*,'after prupk'
*
       print *,'end of page01'
       RETURN
*
   81  FORMAT(I8)
   83  FORMAT(6X,I1,'-phase',I5,'-limbs',I6,' Hz',5X,'Cooling ',A4,
     &             4X,'WT=',I3,' deg    TO=',I3,' deg')
   84  FORMAT(6X,'Rating MVA',5X,'Voltage kV',14X,
     &        'Connection   BIL kV',4X,'SC-Pwr GVA')
   85  FORMAT(6X,2F10.1,19X,A6,I13,F14.1)
   86  FORMAT(6X,2F10.1,'+',I2,'-',I2,'*',F5.2,'%',6X,A6,I13,F14.1)
   87  FORMAT(6X,'Core series: ',A60)
   88  FORMAT(6X,A6,A15,6X,A11,F7.2,'*',F6.4,'*',F4.1,'=',F7.2,' V')
   89  FORMAT(6X,A12,I8,A4,3X,A12,F26.4,A6)
   90  FORMAT(6X,A12,I8,A4,3X,A12,4X,2F7.4,F8.4,A6)
   91  FORMAT(6X,A12,I8,A4,3X,A16,F22.1,A6)
   92  FORMAT(6X,A12,I8,A4,3X,A16,2F7.1,F8.1,A6)
   93  FORMAT(6X,A12,F8.1,A4,3X,A12,F26.1,A6)
   94  FORMAT(6X,A12,F8.1,A4,3X,A12,4X,2F7.1,F8.1,A6)
   95  FORMAT(6X,'Banding',9X,A8,
     &            3X,'Exiting current',F8.0,' kVA  =',F7.2,' %')
   96  FORMAT(6X,'Step lap joint: ',A4)
   97  FORMAT(' '/'      O P E R A T I N G')
   98  FORMAT(6X,'Core blading :  ',A4)
   99  FORMAT(6X,'TO Temp.rise : ',I3,' deg')
  100  FORMAT(6X,'Core blading :  ',A4,7X,
     &   'External core lamination ident:')
  101  FORMAT(6X,'TO Temp.rise : ',I3,' deg', 5X,A44)
  102  FORMAT(' '/6X,A, A/1X)
  103  FORMAT(6X, 'Cooling ducts in core, required............:',I3,
     &        /6X,'Cooling ducts, selected (input or required):',I3)
*
       END
