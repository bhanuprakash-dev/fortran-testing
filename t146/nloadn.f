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
*      Subroutine NLOADN
*      -----------------
*
C...   Title: Calculation of no-load losses,
C...          Magnetisation power and core sound-level
*
*      Written: XX-XX-XX by A.N.Other  , XXXX
*      Revised: 85-10-18 by Ron Bell   , TRAFO/IK
*      Revised: 88-02-10 by Ron Bell   , TRAFO/IK
*
************************************************************************
*
       SUBROUTINE NLOADN(BBRESC,BBRES)
*
C... Declarations
*
CC*SBBL Start of the declarations block
*
       include'com1.h'
*
       EQUIVALENCE
     &  (XZ0001(1)    ,  AARR   (1)    ),
     &  (XZ1371       ,  BB132         ),
     &  (XZ1390       ,  BBSLIM        ),
     &  (XZ1393       ,  BBVFR         ),
     &  (XZ1405       ,  BLIMB         )
       EQUIVALENCE
     &  (XZ1407       ,  BMAXPU        ),
     &  (XZ1408       ,  BMINPU        ),
     &  (XZ1426       ,  DCORE         ),
     &  (XZ1446       ,  FREQ          ),
     &  (XZ1456       ,  GCORN         ),
     &  (XZ1410       ,  BTANK         ),
     &  (XZ1528       ,  RLTANK        ),
     &  (XZ1474       ,  HTANK         )
       EQUIVALENCE
     &  (XZ1459       ,  GLIMBM        ),
     &  (XZ1460       ,  GLIMBY        ),
     &  (XZ1470       ,  GYOKE         ),
     &  (XZ1472       ,  HLIMB         ),
     &  (XZ1486       ,  JFC           )
       EQUIVALENCE
     &  (XZ1509       ,  NWOULI        ),
     &  (XZ1515       ,  PLIMB         ),
     &  (XZ1527       ,  ISTGRD        ),
     &  (XZ1537       ,  SBF           )
       EQUIVALENCE
     &  (XZ1539       ,  SEQU2W        ),
     &  (XZ1558       ,  TYPCOR        ),
     &  (XZ1567       ,  YOKAMP        ),
     &  (XZ1650(1)    ,  P00    (1)    ),
     &  (XZ1653(1)    ,  Q00    (1)    )
       EQUIVALENCE
     &  (XZ1686(1)    ,  SOUND0 (1)    )
       EQUIVALENCE
     &  (XZ1491       ,  KCORE         ),
     &  (XZ1533       ,  DN            )
*
       EQUIVALENCE
     &  (XZ2701       ,  EXTCOR        ),
     &  (XZ2702(1)    ,  PSPL1(1)      ),
     &  (XZ2703(1)    ,  PSPL2(1)      ),
     &  (XZ2704(1)    ,  PSPL3(1)      ),
     &  (XZ2705(1)    ,  PSPHD1(1)     ),
     &  (XZ2706(1)    ,  PSPHD2(1)     ),
     &  (XZ2707(1)    ,  PSPHD3(1)     ),
     &  (XZ2708(1)    ,  PSPHT1(1)     ),
     &  (XZ2709(1)    ,  PSPHT2(1)     ),
     &  (XZ2710(1)    ,  PSPHT3(1)     ),
     &  (XZ2711(1)    ,  PSPTY1(1)     ),
     &  (XZ2712(1)    ,  PSPTY2(1)     ),
     &  (XZ2713(1)    ,  PSPTY3(1)     )
       EQUIVALENCE
     &  (XZ2714(1)    ,  SSPL1(1)      ),
     &  (XZ2715(1)    ,  SSPL2(1)      ),
     &  (XZ2716(1)    ,  SSPL3(1)      ),
     &  (XZ2717(1)    ,  SSPHD1(1)     ),
     &  (XZ2718(1)    ,  SSPHD2(1)     ),
     &  (XZ2719(1)    ,  SSPHD3(1)     ),
     &  (XZ2720(1)    ,  SSPHT1(1)     ),
     &  (XZ2721(1)    ,  SSPHT2(1)     ),
     &  (XZ2722(1)    ,  SSPHT3(1)     ),
     &  (XZ2723(1)    ,  SSPTY1(1)     ),
     &  (XZ2724(1)    ,  SSPTY2(1)     ),
     &  (XZ2725(1)    ,  SSPTY3(1)     ),
     &  (XZ2763       ,  SPRED         )

C---------------------------------------------------------------------
         REAL      PSPL1(3),   PSPL2(5),   PSPL3(3),
     &             PSPHD1(3),  PSPHD2(5),  PSPHD3(3),
     &             PSPHT1(3),  PSPHT2(5),  PSPHT3(3),
     &             PSPTY1(3),  PSPTY2(5),  PSPTY3(3)
         REAL      SSPL1(3),   SSPL2(5),   SSPL3(3),
     &             SSPHD1(3),  SSPHD2(5),  SSPHD3(3),
     &             SSPHT1(3),  SSPHT2(5),  SSPHT3(3),
     &             SSPTY1(3),  SSPTY2(5),  SSPTY3(3)
C
       REAL      BLIM(3),AARR(200),P00(3),Q00(3),SOUND0(3),BLIMB,
     &           BMAXPU,BMINPU,DCORE,FREQ,FR1,FR2,GCORN,GLIMBM,
     &           GLIMBY,GYOKE,HLIMB,PLIMB,SBF,SEQ,SEQU2W,
     &           TYPCOR,YOKAMP, CORR, DN, HTANK, RLTANK, BTANK
       REAL      LPA03, PCORR,QCORR, SPRED
*
       INTEGER   I,ISTGRD,JFC,NWOULI,KCORE
*
       LOGICAL   BB132,BBVFR,BBSTOP,BBRES,BBRESC,BBSLIM,EXTCOR
*
       CHARACTER CORRES*50
*
CC*SEBL End of the declarations block
*
       BLIM(1)=BLIMB
       BLIM(2)=BLIMB*BMAXPU
       BLIM(3)=BLIMB*BMINPU
       SEQ=SEQU2W*FLOAT(NWOULI)
       BLADN=AARR(140)
       PCORR = 1.
       QCORR = 1.
*
          BBRES=.FALSE.
*
C... Calculate resonant frequencies
*
C,,, If required
*
       IF (BBRESC) THEN
*
C... Calculate
*
C,,, NO Side-limbs
*
          IF (TYPCOR.EQ.1..OR.TYPCOR.EQ.3.) THEN
*
C... Calculate
*
             CALL FRNAT(TYPCOR,FREQ,DCORE,PLIMB,HLIMB,BBRES,FR1,FR2)
*
                   CORRES='Core resonance NOT likely(ZK-TA 4567-101E)'
             IF(BBRES)
     &             CORRES='>> Core resonance likely (ZK-TA 4567-101E)'
             WRITE(JFC,1885) 'Natural frequencies (F1,F2)=',
     &                     NINT(FR1),NINT(FR2),' Hz',CORRES
*
C... Side-limbs
*
C,,, No calc.
*
          ELSE
             CORRES='No check on core resonance done (ZK-TA 4567-101E)'
             WRITE(JFC,1886) CORRES
          END IF
       END IF
*
       BBSTOP=.FALSE.
*
C... For each tap
*
       DO 200 I=1,3
*
C... Calculate sound level and loss values
*
C,,, If not BBSTOP
*
       IF(.NOT.BBSTOP) THEN
C...TA1 Core
        IF(KCORE .EQ. 101) THEN
          IF(XZ0581(23) .EQ. 'YES ') THEN
             TSTP = 1.
          ELSE
             TSTP = 0.
          ENDIF
*
CCC       P00(I) = P0TA1(BLIM(I), FREQ, ISTGRD,
CCC  &                   GLIMBM, GYOKE, GCORN, TSTP,PCORR,JFC)
CCC       Q00(I) = S0TA1(BLIM(I), FREQ, ISTGRD,
CCC  &                   GLIMBM, GYOKE, GCORN, TSTP, JFC)

C,,,Other Cores
        ELSE
          IF(EXTCOR) THEN
            CORR = 1.33
            P00(I) = RNOLO( TYPCOR, BLIM(I), FREQ, YOKAMP, SBF,CORR,
     &                      GLIMBM, GLIMBY, GYOKE, GCORN,
     &                      PSPL1,  PSPL2,  PSPL3,
     &                      PSPHD1, PSPHD2, PSPHD3,
     &                      PSPHT1, PSPHT2, PSPHT3,
     &                      PSPTY1, PSPTY2, PSPTY3 )
            CORR = 1.25
            Q00(I) = RNOLO( TYPCOR, BLIM(I), FREQ, YOKAMP, SBF,CORR,
     &                      GLIMBM, GLIMBY, GYOKE, GCORN,
     &                      SSPL1,  SSPL2,  SSPL3,
     &                      SSPHD1, SSPHD2, SSPHD3,
     &                      SSPHT1, SSPHT2, SSPHT3,
     &                      SSPTY1, SSPTY2, SSPTY3 )
C
          ELSE
            P00(I)=PNOLO(TYPCOR,BLIM(I),FREQ,YOKAMP,SBF,ISTGRD,
     &                   GLIMBM,GLIMBY,GYOKE,GCORN,BLADN)
            Q00(I)=SNOLO(TYPCOR,BLIM(I),FREQ,YOKAMP,SBF,ISTGRD,
     &                   GLIMBM,GLIMBY,GYOKE,GCORN,BLADN)
          ENDIF
*
C
          BBSTOP=(.NOT.BBVFR)
        ENDIF
*
        SOUND0(I)=SOUNDP(TYPCOR,DCORE,PLIMB,
     &                   HLIMB,ISTGRD,BLIM(I),FREQ)
        IF(BB132     ) SOUND0(I)=SOUND0(I)-10.
       END IF
  200  CONTINUE
*
C... Adjust sound level and loss values
*
C,,, If not variable flux regulation
*
C... Reduction between sound power level and sound pressure level
*
       IF( ABS(HTANK) .LT. 1.E-6 ) THEN
             SPRED = 0.
       ELSE
             SPRED = 10.*ALOG10(1.25*HTANK*(2*BTANK+2*RLTANK+1.9))
       ENDIF
*
       IF(.NOT.BBVFR) THEN
          SOUND0(2)=SOUND0(1)
          P00(2)=P00(1)
          Q00(2)=Q00(1)
       END IF
*
       write(*,*)"P00-NLOADN",P00
       RETURN
 1885  FORMAT(1X/6X,A28,2I7,A3,/,30X,A42)
 1886  FORMAT(1X/6X,15X,A50)
       END
