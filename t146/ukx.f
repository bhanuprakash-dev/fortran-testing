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
*      Subroutine UKX
*      --------------
*
C...   Title: Calculation of short-circuit impedance
*
*      Written: 83-04-12 by P.L-V.     , KYT
*      Revised: 86-04-03 by Ron Bell   , TRAFO/IK
*      Revised: 88-02-10 by Ron Bell   , TRAFO/IK
*
************************************************************************
*
       SUBROUTINE UKX(UK)
*
C... Declarations
*
CC*SBBL Start of the declarations block.
*
       include'com1.h'
*
       EQUIVALENCE
     &  (XZ0741(1)    ,  DWIND  (1)    ),
     &  (XZ0781(1)    ,  FACT   (1)    ),
     &  (XZ0851(1)    ,  FWIND  (1)    ),
     &  (XZ1001(1)    ,  RRWDG  (1)    ),
     &  (XZ1446       ,  FREQ          )
       EQUIVALENCE
     &  (XZ1476       ,  HWINDM        ),
     &  (XZ1508       ,  NWILI         ),
     &  (XZ1513       ,  PI            ),
     &  (XZ1542       ,  SNOML         )
*
       REAL      DWIND(9),RRWDG(9),FWIND(9),FACT(9),D1,D2,FREQ,
     &           HLP,HWINDM,PI,SNOML,T,T0,T1,UK,X1
*
       INTEGER   IWDG,JWDG,NWILI
*
CC*SEBL End of the declarations block.
*
       UK=0.   
       write(*,*) "FACT=",FACT
       write(*,*) "FWIND=",FWIND
       write(*,*) "RRWDG=",RRWDG
       write(*,*) "FACT=",FACT
       write(*,*) "HWINDM=",HWINDM
       write(*,*) "SNOML=",SNOML
*
C	write(*,*) DWIND,FACT,FWIND,RRWDG,FREQ,HWINDM,NWILI,PI,SNOML
C... Calculate for each winding ( except the outermost )
*      
       
       DO 170 IWDG=1,NWILI-1
*
C... Calculate
*
C,,, If FACT <> 0
*
          IF(FACT(IWDG).NE.0.) THEN
             D1=DWIND(IWDG)
             T1=RRWDG(IWDG)
*
C... Calculate for next winding
*
             DO 160 JWDG=IWDG+1,NWILI
*
C... Calculate
*
C,,, If FACT <> 0
*
                IF(FACT(JWDG).NE.0.) THEN
                   D2=DWIND(JWDG)
                   T0=RRWDG(JWDG)+T1
                   T=(D2-D1+T0)/2.
                   X1=PI*HWINDM/T
                   HLP=0.
                   IF(ABS(X1).LT.60.) HLP=EXP(-X1)
                   UK=UK-(T-0.66666667*T0)*(D1+D2)*
     &                FWIND(IWDG)*FWIND(JWDG)*(1.-(1.-HLP)/X1)
             END IF
  160        CONTINUE
          END IF
  170  CONTINUE
*
C... Calculate UK
*
CC*SBBL Start of the UK block
*
       UK=UK*2.*PI**3/10.*(FREQ/50.)/SNOML/HWINDM/1.E+4
*
CC*SEBL End of the UK block
*
       RETURN
       END
