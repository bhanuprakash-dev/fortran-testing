C*..* COST
CCOST
C
C     *******************    NYTT    *************************
C     ALL PARAMETERS PREVIOUSLY STARTING WITH THE PREFIX 'GSN'
C     HAVE BEEN CHANGED TO 'GS' TO ACCOMODATE THE FORTRAN 77
C     NAMING REQUIREMENT OF NOT MORE THAN 6 CHARACTERS.
C     ********************************************************
C
C
      SUBROUTINE COST (N,LC,LM,GS,BGSG,BCS,GG,BCG,BCM1,BCM2,BCM3,BCM4)
C
      DIMENSION C(15,146),R(241,6),R2(6),R3(6),R4(6)
C
      COMMON/KONSIN/C
      COMMON/KONSRE/R
CC* NYTT
      COMMON/KONTMP/R2,R3,R4
CC*
C
C***  INDATAKONTROLL
C
      IF(LC   .EQ. 0 ) GO TO 60
      AGSG = BGSG
      IF(AGSG .EQ. 0.) AGSG = 1.
      ACS  = BCS
      IF(ACS  .EQ. 0.) ACS  = 1.
      ACG  = BCG
      IF(ACG  .EQ. 0.) ACG  = 1.
      ACM1 = BCM1
      IF(ACM1 .EQ. 0.) ACM1 = 1.
      ACM2 = BCM2
      IF(ACM2 .EQ. 0.) ACM2 = 1.
      ACM3 = BCM3
      IF(ACM3 .EQ. 0.) ACM3 = 1.
      ACM4 = BCM4
      IF(ACM4 .EQ. 0.) ACM4 = 1.
C
C***  NET. WEIGHT  SHAPED MATERIAL
      R4(1)= GS
C
C***  GROSS WEIGHT SHAPED MATERIAL
      R4(2) =  C(1,LC)*GS* AGSG**C(2,LC)
C
C***  SHAPED MATERIAL COSTS
      R4(3) = C(3,LC)*R4(2)*ACS**C(4,LC)
C
C***  GOODS WEIGHT
      R4(4) = GG
C
C***  GOODS COSTS
      R4(5) =C(5,LC)*GG*ACG**C(6,LC)
C
C
C***  MANUFACTURING   COSTS
       R4(6) = C(7,LC)*C(8,LC) *
     * EXP(  C(9,LC)*ALOG(ACM1)  + C(10,LC)*ALOG(ACM2) +
     *       C(11,LC)*ALOG(ACM3) + C(12,LC)*ALOG(ACM4)    )
      IF(BCM1.LT.1.E-6) R4(6) = 0.
C
C***  COST ACCUMULATION
      DO 50 I=1,6
         R(LM,I) = R(LM,I) + R4(I)
         R3(I)   = R3(I)   + R4(I)
         R2(I)   = R2(I)   + R4(I)
C         write(*,*) "R4(I)=",R4(I)
C         write(*,*) "R(235,I)=",R(235,I)
         IF(LM.LE.235) R(235,I) = R(235,I) + R4(I)
   50    CONTINUE
C
C
C***  ACCUMULATION OF MANUF. PRICES IN THE COMP. PRICE.
      R(241,3) = R(241,3) + C(13,LC) * R4(3)
      R(241,5) = R(241,5) + C(14,LC) * R4(5)
      R(241,6) = R(241,6) + C(15,LC) * R4(6)
C
C
 60   CONTINUE
      GOTO (99,80,70,99,99),N
C
C...  PART LEVEL
 70   DO 74 I=1,6
         R(LM,I) = R3(I)
         R3(I)   = 0.
 74      CONTINUE
      GO TO 99
C
C...  SUBTOTAL LEVEL
 80   DO 86 I=1,6
         R(LM,I) = R2(I)
         R3(I)   = 0.
         R2(I)   = 0.
 86      CONTINUE
C
 99   RETURN
C--------------------------------------------------
C
C***  ACCUMULATING VARIABLES ARE SET TO   0.  .
C
      ENTRY  COSTZO
      DO 999 I=1,6
         R2(I) = 0.
         R3(I) = 0.
         R4(I) = 0.
 999     CONTINUE
      RETURN
      END
