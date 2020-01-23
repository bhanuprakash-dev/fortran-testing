C*..* TAB79
CTAB79
          SUBROUTINE TAB79(R,JFC,MYNT,NWILI)
C   SPECIFICATION OF COSTS IN MNL79 IN 8 DIFFERENT COST GROUPS
C   ACCORDING TO FORMAT STATEMENTS BELOW
C
C
          DIMENSION R(241,6)
C
C...      INTERNAL FUNCTIONS
          SM(I) = R(I,3) + R(I,5)* 0.5
          SA(I) = R(I,6) + R(I,5)* 0.5
C
C...      INITIALIZATION
C
          S1 = 0.
          S2 = 0.
          S3 = 0.
          S4 = 0.
          S5 = 0.
          S6 = 0.
          S7 = 0.
          S8 = 0.
C
C...      ACCUMULATION OF WINDING DATA
          DO 10 I = 1, NWILI
             J = (I-1) * 19
             S1 = S1 + SM(J+1)  + SM(J+3)
             S6 = S6 + SA(J+1)  + SA(J+3)
             S2 = S2 + SM(J+2)
             S6 = S6 + SA(J+2)
             S5 = S5 + SM(J+4)  + SM(J+12)
             S6 = S6 + SA(J+4)  + SA(J+12)
             S5 = S5 + SM(J+18)
             S6 = S6 + SA(J+18)
 10       CONTINUE
C
C...      CORE DATA
          S3 = S3 + SM(156) + SM(159) + SM(165)
          S6 = S6 + SA(156) + SA(159) + SA(165)
          S4 = S4 + SM(155) - SM(154) + SM(158) - SM(156) - SM(157)
          S6 = S6 + SA(155) - SA(154) + SA(158) - SA(156) - SA(157)
          S5 = S5 + SM(154)
          S6 = S6 + SA(154)
          S5 = S5 + SM(157) + SM(166)
          S6 = S6 + SA(157) + SA(166)
C
C...      OTHER
          S1 = S1 + SM(172) + SM(225)
          S6 = S6 + SA(172) + SA(225)
          S3 = S3 + SM(188)
          S6 = S6 + SA(188)
          STAN = SM(197) + SM(198) + SM(199)
          STANA= SA(197) + SA(198) + SA(199)
          S4 = S4 + STAN    + SM(201) + SM(202) + SM(207) + SM(211)
     *            + SM(212) + SM(217) + SM(222) + SM(223) + SM(224)
     *            + SM(227) + SM(231) + SM(234)
          S6 = S6 + STANA   + SA(201) + SA(202) + SA(207) + SA(211)
     *            + SA(212) + SA(217) + SA(222) + SA(223) + SA(224)
     *            + SA(227) + SA(231) + SA(234)
          S5 = S5 + SM(177) + SM(184) + SM(189) + SM(190)
          S6 = S6 + SA(177) + SA(184) + SA(189) + SA(190)
          S8 = S8 + SM(238)
          S6 = S6 + SA(238)
          S5 = S5 + SM(228) + SM(200) - STAN
          S6 = S6 + SA(228) + SA(200) - STANA
          S5 = S5 + SM(178) + SM(185) + SM(195) + SM(229) + SM(232)
          S6 = S6 + SA(178) + SA(185) + SA(195) + SA(229) + SA(232)
          S7 =      SM(239) + SM(240) + SA(239) + SA(240)
C
          SUM = S1 + S2 + S3 + S4 + S5 + S6
          PP  = 100./SUM
          WRITE(JFC,800) MYNT,
     *                   S1, S1*PP, S2, S2*PP, S3, S3*PP, S4, S4*PP,
     *                   S5, S5*PP, S6, S6*PP,
     *                   SUM, SUM*PP
 800      FORMAT(1X/6X,'SPECIFICATION OF COSTS:',
     *                  /1X,49X,A4,8X,'%',
     *             /20X,'Copper                    ',F10.0,F10.1
     *             /20X,'Aluminium                 ',F10.0,F10.1
     *             /20X,'Core steel                ',F10.0,F10.1
     *             /20X,'Steel others              ',F10.0,F10.1
     *             /20X,'Other materials and goods ',F10.0,F10.1
     *             /20X,'Labour and labour overhead',F10.0,F10.1
     *             /20X,'*** TOTAL  OVNV ***       ',F10.0,F10.1)
C
          WRITE(JFC,802) S7,S8, SUM + S7 + S8
 802      FORMAT(1X/6X,'Excluded above:',
     *             /20X,'Separate coolers          ',F10.0,
     *             /20X,'Oil                       ',F10.0,
     *             /20X,'*** TOTAL ****            ',F10.0)
C
           SUMDAT= R(241,3)+R(241,5)+R(241,6)
        RETURN
        END
