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
*      Subroutine WINDDI
*      -----------------
*
C...   Title:   Calculate the geometrical
C...            dimensions of the windings
C...            Calculate the cross-sectional areas of each
C...            winding and a mean-value of the
C...            winding height "HW"="HLIMB"-"DYOKE0".
*
*      Written: XX-XX-XX by A.N.Other  , XXXX
*      Revised: 85-10-18 by Ron Bell   , TRAFO/IK
*      Revised: 88-02-10 by Ron Bell   , TRAFO/IK
*
************************************************************************
*
       SUBROUTINE WINDDI
*
C... Declarations
*
CC*SBBL Start of the declarations block.
*
       include'com1.h'
*
       EQUIVALENCE
     &  (XZ0651(1)    ,  ACOND  (1)    ),
     &  (XZ0661(1)    ,  AWIND  (1)    ),
     &  (XZ0671(1)    ,  BDUCT  (1)    ),
     &  (XZ0681(1)    ,  BPART  (1)    )
       EQUIVALENCE
     &  (XZ0741(1)    ,  DWIND  (1)    ),
     &  (XZ0751(1)    ,  DYOKE  (1)    ),
     &  (XZ0761(1)    ,  EDDCO1 (1)    ),
     &  (XZ0771(1)    ,  EDDCON (1)    ),
     &  (XZ0791(1)    ,  FILLF  (1)    )
       EQUIVALENCE
     &  (XZ0851(1)    ,  FWIND  (1)    ),
     &  (XZ0861(1)    ,  GWINCO (1)    ),
     &  (XZ0881(1)    ,  HWIND  (1)    ),
     &  (XZ0981(1)    ,  RAACON (1)    ),
     &  (XZ1001(1)    ,  RRWDG  (1)    )
       EQUIVALENCE
     &  (XZ1011(1)    ,  RWIND  (1)    ),
     &  (XZ1031(1)    ,  SIGM   (1)    ),
     &  (XZ1191(1)    ,  ZWIND  (1)    ),
     &  (XZ1311(1)    ,  BBHELP (1)    ),
     &  (XZ1378       ,  BBERR         ),
     &  (XZ1391       ,  BBTS          ),
     &  (XZ1486       ,  JFC           ),
     &  (XZ1331(1)    ,  BBRR   (1)    )
       EQUIVALENCE
     &  (XZ1380       ,  BBEXAC        ),
     &  (XZ1384       ,  BBHLI         ),
     &  (XZ1389       ,  BBREA         ),
     &  (XZ1426       ,  DCORE         ),
     &  (XZ1428       ,  DOUTW         )
       EQUIVALENCE
     &  (XZ1472       ,  HLIMB         ),
     &  (XZ1476       ,  HWINDM        ),
     &  (XZ1508       ,  NWILI         ),
     &  (XZ2782       ,  ISTOP         ),
     &  (XZ1513       ,  PI            )
       EQUIVALENCE
     &  (XZ1555       ,  TURNRA        ),
     &  (XZ1561       ,  USHORE        )
*
       REAL      FWIND(9),ZWIND(9),FWIND0(9),ZWIND0(9),DWIND(9),
     &           RRWDG(9),HWIND(9),AWIND(9),DYOKE(9),FILLF(9),
     &           ACOND(9),EDDCON(9),EDDCO1(9),RWIND(9),
     &           SIGM(9),RAACON(9),BPART(9),GWINCO(9),BDUCT(9),
     &           BW1,DCORE,DCORE1,DCOR0,DOUTW,DW1,DYOKE0,
     &           HLIMB,HLP,HWINDM,PI,RLW0,TURNRA,TURNR0,
     &           USHORE,UX,RATDCO,ABSFW,ABSDFW
*
       INTEGER   IWDG,NWILI,JFC,ISTOP
*
       LOGICAL   BBRR(9),BBHLI,BBREA,BBEND,BBHELP(10),BBEXAC,
     &           BBERR,BBTS
*
       DIMENSION NCOL(9)
       CHARACTER NCOL*1
*
       DATA NCOL/'A','B','C','D','E','F','G','H','I'/
*
CC*SEBL End of the declarations block.
*
C... Calculate mean winding height
*
CC*SBBL Start of the mean winding height block
*
       ABSFW=0.
       ABSDFW=0.
       DCOR0=0.
       TURNR0=0.
*
       DO 3 IWDG=1,NWILI
       FWIND0(IWDG)=0.
    3  ZWIND0(IWDG)=0.
*
C... For each winding
*
       DO 1 IWDG=1,NWILI
       ABSFW=ABSFW+ABS(FWIND(IWDG))
    1  ABSDFW=ABSDFW+DYOKE(IWDG)*ABS(FWIND(IWDG))
*
       DYOKE0=ABSDFW/ABSFW
       HWINDM=HLIMB-DYOKE0
*      
CC*SEBL End of the mean winding height block
*
C... Calculate certain variables for each winding
*
C,,, If no limb height or reactance given
*
       IF (.NOT.BBHLI.AND..NOT.BBREA) THEN
          DCOR0=DCORE
          TURNR0=TURNRA
*
C... Calculate for each winding
*
          DO 2 IWDG=1,NWILI
          FWIND0(IWDG)=FWIND(IWDG)
    2     ZWIND0(IWDG)=ZWIND(IWDG)
*
       END IF
*
       BBEND=.FALSE.
*
C... Calculate the winding dimensions unless BBEND is true
*

  400  IF (BBEND) GOTO 499
*
       DW1=DCORE
       BW1=0.
*
C... Calculate for each winding
*
       DO 599  IWDG=1,NWILI
       HWIND(IWDG)=HLIMB-DYOKE(IWDG)
       write(*,*)HWIND(IWDG)
       write(*,*)'HLIMB and DYOKE',HLIMB,DYOKE(IWDG)
       IF (.NOT.BBRR(IWDG)) RRWDG(IWDG)=AWIND(IWDG)/HWIND(IWDG)
       write(*,*)AWIND(IWDG)
       write(*,*)DW1,BW1,BDUCT(IWDG)
       DWIND(IWDG)=DW1+BW1+2.*BDUCT(IWDG)+RRWDG(IWDG)
       write(*,*)DWIND(IWDG)
       write(*,*)'And RRWDWDG',RRWDG(IWDG)
       BW1=RRWDG(IWDG)
  599  DW1=DWIND(IWDG)
*
       DOUTW=DW1+BW1

*
C... Calculate reactance
*
C,,, If 'EXACT' calculation and no reactance specified
       IF (BBEXAC.AND..NOT.BBREA) THEN
*
C... Calculate short-circuit impedance
*
          UX=0.
          CALL UKX (UX)
*
C... Calculate certain variables
*
C,,, If ratio <> 1
*
	
          IF (ABS(USHORE/UX-1.).GE.1.E-5) THEN
*
C... Calculate certain variables
*
C,,, If no limb height was given
*
             IF (.NOT.BBHLI) THEN
                DCORE1=DCORE/((USHORE/UX)**0.25)
                HLP=ABS(DCORE1-DCORE)
                IF (HLP.LT.0.5E-3) BBEND=.TRUE.
*
C... Calculate certain variables
*
C,,, If not the final calculation
*
                IF (.NOT.BBEND) THEN
                   DCORE=DCORE1
                   RATDCO=(DCOR0/DCORE)*(DCOR0/DCORE)
                   TURNRA=TURNR0*RATDCO
*
C... Calculate for each winding
*
                   DO 19 IWDG=1,NWILI
                   FWIND(IWDG)=FWIND0(IWDG)*RATDCO
                   ZWIND(IWDG)=ZWIND0(IWDG)*RATDCO
                   IF (.NOT.BBRR(IWDG))
     &              AWIND(IWDG)=ACOND(IWDG)*ZWIND(IWDG)/FILLF(IWDG)
                   write(*,*)AWIND,'AWINDWWWW'
   19              CONTINUE
*
                END IF
*
C... If limb height was given
*
             ELSE
                HWINDM=HWINDM*AMAX1(0.5,1.-0.60*(USHORE/UX-1.))
                HLIMB=HWINDM+DYOKE0
                
             END IF
*
C... If ratio = 1
*
          ELSE
             BBEND=.TRUE.
          END IF
*
C,,, If no 'EXACT' calculation or reactance specified
*
       ELSE
          BBEND=.TRUE.
       END IF
*
       GOTO 400
*
  499  CONTINUE
*
C... Calculate for each winding
*
       DO 1199 IWDG=1,NWILI
       IF (BBRR(IWDG)) FILLF(IWDG)=ACOND(IWDG)*ZWIND(IWDG)/
     &                             (HWIND(IWDG)*RRWDG(IWDG))
*
C... Space factor is greater than 1
*
       IF (FILLF(IWDG).GE.1)THEN
           CALL FPRINT(2,BBERR,BBHELP,BBTS,JFC,
     &    'WINDDI:SPACE FACTOR FOR WINDING '//NCOL(IWDG)//
     &    ' IS GREATER THAN 1')
*....      Backtracking
           IF(ISTOP.EQ.1) RETURN
       ENDIF
*
       RLW0=PI*DWIND(IWDG)*ZWIND(IWDG)
       GWINCO(IWDG)=RLW0*ACOND(IWDG)*RAACON(IWDG)
*
C... Electrical properties
*
CC*SBBL Start of the electrical properties block
*
       RWIND(IWDG)=RLW0/(SIGM(IWDG)*ACOND(IWDG))
       EDDCON(IWDG)=EDDCO1(IWDG)*GWINCO(IWDG)*
     &              (BPART(IWDG)/HWINDM)*(BPART(IWDG)/HWINDM)
*
CC*SEBL End of the electrical properties block
*
 1199  CONTINUE
       RETURN
       END
