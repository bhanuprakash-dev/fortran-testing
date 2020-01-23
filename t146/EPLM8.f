      SUBROUTINE EPLM8(E,NE,M,N,P,RES,TD,X,Z,HV,
     & I1,I2,I3,I4,I5,I6,I7,I8,I9,LCOL)
C
      INTEGER P,I1,I2,I3,I4,I5,I6,I7,I8,I9,LCOL,NE,M,N,RES
      REAL TD,Z
      DIMENSION E(NE,*),X(1),HV(1)
      write(*,*) "M=",M
      write(*,*) "N=",N
      write(*,*) "P=",P
      write(*,*) "TD=",TD
c
      write(*,*) "HV(I1)=",HV(I1)
      write(*,*) "HV(I2)=",HV(I2)
      write(*,*) "HV(I3)=",HV(I3)
      CALL LLP1(M,N,P,E,NE,TD,RES,
     &HV(I1),HV(I2),HV(I3),HV(I4),
     &HV(I5),HV(I6),HV(I7),HV(I8))
      write(*,*) "HV=",(HV(I),I=1,50)
      Z=-E(1,LCOL)
      DO 100 I=1,N
  100 X(I)=HV(I)
      write(*,*) "XDELTA=",(X(I),I=1,10)
      RETURN
      END

