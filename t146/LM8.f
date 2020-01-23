CLM8        LP-SOLUTION
C
      SUBROUTINE LM8 (NE,M,N,P,I1,I2,I3,I4,I5,I6,I7,I8,I9,LCOL)
       
      INTEGER P 
        print *,'in lm8',P,NE,M,N
      LCOL=M+N-P+1
      I1=1
      I2=I1+N+M
      I3=I2+LCOL
      I4=I3+LCOL
      I5=I4+LCOL
      I6=I5+NE
      I7=I6+NE
      I8=I7+NE
      I9=I8+NE

      RETURN
      END
C

