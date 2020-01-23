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
*      Subroutine SCDIME
*      -----------------
*
C...   Title:  Calculate the actual short-circuit tangential
C...   stresses appearing for those cases which are dimensioning.
C...   The dimensioning cases will be determined in routine "DIMSCC".
*
*      Written: XX-XX-XX by A.N.Other  , XXXX
*      Revised: 85-10-18 by Ron Bell   , TRAFO/IK
*      Revised: 88-02-10 by Ron Bell   , TRAFO/IK
*
************************************************************************
*
       SUBROUTINE SCDIME(BBOUT)
*
C... Declarations
*
CC*SBBL Start of the declarations block.
*
       include'com1.h'
*
       EQUIVALENCE
     &  (XZ1041(1)    ,  STRWMM (1)    ),
     &  (XZ1051(1)    ,  STRWMP (1)    ),
     &  (XZ1227(1)    ,  NMSTEP (1)    ),
     &  (XZ1231(1)    ,  NPSTEP (1)    ),
     &  (XZ1351(1)    ,  BBSCI  (1)    )
       EQUIVALENCE
     &  (XZ1354(1)    ,  BBSCJ  (1)    ),
     &  (XZ1357(1)    ,  BBSCK  (1)    ),
     &  (XZ1366(1)    ,  BBVR   (1)    ),
     &  (XZ1393       ,  BBVFR         ),
     &  (XZ1499       ,  NG            )
       EQUIVALENCE
     &  (XZ1508       ,  NWILI         ),
     &  (XZ3010(1)    ,  NXSTEP (1)    )
*
       REAL      STRWMM(9),STRWMP(9)
*
       INTEGER   NMSTEP(4),NPSTEP(4),NXSTEP(3),ITAP,IWDG,I1,JTAP,
     &           JTML,J1,KTAP,K1,NG,NTAPS,NWILI
*
       LOGICAL   BBSCI(4),BBSCJ(4),BBSCK(4),BBVR(4),BBVFR,BBOUT
*
CC*SEBL End of the declarations block.
*
C... Initialisations
*
       DO 299  IWDG=1,NWILI
       STRWMM(IWDG)=0.
  299  STRWMP(IWDG)=0.
*
C... Determine No. of taps
*
       NTAPS=4
*
C... For each terminal
*
       DO 399 JTML=1,MIN(NG,3)
*
C... Adjust the No. of taps
*
C,,, Only for the regulated terminal.
*
       IF (BBVR(JTML)) THEN
          IF ((NXSTEP(JTML).EQ.NPSTEP(JTML))
     &    .OR.(NXSTEP(JTML).EQ.NMSTEP(JTML))
     &    .OR.(NXSTEP(JTML).EQ.0           )) NTAPS=3
       END IF
  399  CONTINUE
*
C... Calculate stresses
*
C,,, Non- regulated trafos and  "CFR"
*
       IF (.NOT.BBVFR) THEN
*
C... For each tap
*
          DO 499 ITAP=1,NTAPS
*
C... If BBSCI(ITAP)
*
C,,, Calculate stresses
*
          IF (BBSCI(ITAP)) THEN
*
C... For each tap
*
             DO 699 JTAP=1,NTAPS
*
C... If BBSCJ(ITAP)
*
C,,, Calculate stresses
*
             IF (BBSCJ(JTAP)) THEN
*
C... For each tap
*
                DO 899 KTAP=1,NTAPS
*
C... If BBSCK(ITAP)
*
C,,, Calculate stresses
*
                IF (BBSCK(KTAP)) THEN
                   I1=ITAP
                   J1=JTAP
                   K1=KTAP
*
C... Calculate stresses
*
                   CALL SCSTRE(I1,J1,K1,BBOUT)
                END IF
  899           CONTINUE
             END IF
  699        CONTINUE
          END IF
  499     CONTINUE
*
C... Voltage regulation type "VFR" and "MR"
*
       ELSE
          DO 1099 JTAP=1,NTAPS
          J1=JTAP
*
C... Calculate stresses
*
          IF(BBSCJ(JTAP)) CALL SCSTRE (1,J1,J1,BBOUT)
 1099     CONTINUE
       END IF
*
       RETURN
       END
