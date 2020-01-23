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
*
*      Subroutine NODCON
*      -----------------
*
C...   Title:    Convert old node designations to new
*
*      Written: 91-11-21 by B-G Bladh      , SETFO/TS
*      Revised: ......................     , .........
*
************************************************************************
*
*
       SUBROUTINE NODCON(NODI,TOPI,BOTI,NWIND,NTERM,
     &            TOPU,BOTU,GWGLAB,EXTNOD,INTNOD,NEXT)
*
C... Declarations
*
       PARAMETER (MAXI=18, MXW=9)
*
       INTEGER
     &     NX, NINT, NEXT, NWIND, NTERM, I, J, K, L
*
       LOGICAL   FOUND
*
       CHARACTER
     &    NODI*4, TOPI*4, BOTI*4,
     &    OLDNOD*4, NEWNOD*4, OFIRST*1, OSECND*1,C1*1,C2*1,CX*1,CY*1,
     &    TOPU*4, BOTU*4, GWGLAB*4, EXTNOD*4, INTNOD*4,OFST*1,
     &    TOP*4, BOT*4, EXT*4
*
       DIMENSION
     &    NODI(*), TOPI(*), BOTI(*),
     &    TOPU(*), BOTU(*),
     &    GWGLAB(*), INTNOD(*), EXTNOD(*), OFST(MAXI),
     &    OLDNOD(MAXI), NEWNOD(MAXI), OFIRST(MAXI), OSECND(MAXI)
*
C... Input : Terminal nodes, old designations:
C...      NODI(I)    External node , terminal I
C... Input : Winding nodes, old designations:
C...      TOPI(J)    Top node from winding J
C...      BOTI(J)    Bottom node from winding J
C... Other input data
C...      NWIND      Number of physical windings
C...      NTERM      Number of terminals (General windings)
*
C... Internal help variables for the new internal nodes (index K)
C...      OLDNOD(K)  Input designation for TOP or BOTTOM node (TOPI / BOTI)
C...      NEWNOD(K)  New internal node
C...      OFIRST(K)  First character in old input node
C...      OSECND(K)  Second character in old input node
C...      NINT       Number of found internal nodes.
*
C... Output : New top / bottom nodes for the physical windings
C...      TOPU(J)    Top node name for winding J
C...      BOTU(J)    Bottom node name for winding J
*
C... Output : New external nodes
C...      GWGLAB(L)  General winding label for node nr L
C...      EXTNOD(L)  External node label
C...      INTNOD(L)  Internal node label
C...      OFST(L)    First char. of old node name. ONLY INTERLAN VARIABLE
*
C... Other output

C...      NEXT       Number of found external nodes.
*
*
C... Read input winding nodes and assign variables listed above
*
       NX = 0
*
       DO 299 J = 1, NWIND
*
C... Top nodes
*
          TOP = TOPI(J)(1:4)
          IF (TOP .NE. '    ') THEN
*
C... Check if input top node already is registrated
*
              CALL SSEARC(TOP, OLDNOD, NX, L, FOUND)
*
              IF (FOUND) THEN
                 TOPU(J) = NEWNOD(L)
              ELSE
                 NX = NX + 1
                 OLDNOD(NX) = TOP
                 OFIRST(NX) = TOP(1:1)
                 OSECND(NX) = TOP(2:2)
                 IF (NX.LT.10) THEN
                    WRITE(NEWNOD(NX), 80) NX
                 ELSE
                    WRITE(NEWNOD(NX), 81) NX
                 ENDIF

                 TOPU(J) = NEWNOD(NX)
  80             FORMAT(I1)
  81             FORMAT(I2)
              ENDIF
*
C... Bottom node
*
              BOT = BOTI(J)(1:4)
*
C... Check if input bottom node already is registrated
*
              CALL SSEARC(BOT, OLDNOD, NX, L, FOUND)
*
              IF (FOUND) THEN
                 BOTU(J) = NEWNOD(L)
              ELSE
                 NX = NX + 1
                 OLDNOD(NX) = BOT
                 OFIRST(NX) = BOT(1:1)
                 OSECND(NX) = BOT(2:2)
                 WRITE(NEWNOD(NX), 80) NX
                 BOTU(J) = NEWNOD(NX)
              ENDIF
*
          ENDIF
*
C... End of block handling defined TOP (and BOT) nodes
*
 299   CONTINUE
*
       NINT = NX
*
C... Identify, and rename external nodes from input external nodes (NODI)
*
       NX = 0
*
       DO 599 I=1, NTERM
          EXT = NODI(I)(1:4)
*
          CALL SSEARC(EXT,OLDNOD,NINT,K,FOUND)
*
          IF (FOUND) THEN
             NX = NX+1
             WRITE(GWGLAB(NX), 80) NX
             INTNOD(NX) = NEWNOD(K)
             WRITE(EXTNOD(NX), 82) NX
 82          FORMAT(I1,'U',2X)
             OFST(NX) = OFIRST(K)
          ENDIF
*
 599   CONTINUE
*
C... Reseach the winding nodes.
C... Rename and register all external nodes in the output fields, which
C... are not handled yet (All non-entry external nodes for general windings)
*
       DO 799 K= 1, NINT
          CX = OFIRST(K)
*
C... External node ?
*
          IF (CX.LT.'0' .OR. CX.GT.'9') THEN
*
C... Check if registrated
*
             CALL SSEARC(NEWNOD(K),INTNOD, NTERM, L, FOUND)
*
             IF (.NOT. FOUND) THEN
                NX = NX + 1
                CALL SSEARC(CX,OFST,NTERM,L,FOUND)
                C1 = EXTNOD(L)(1:1)
                CY = OSECND(K)
                IF (CY .EQ. '0') THEN
                   C2 = 'N'
                ELSE IF (CY .EQ. '1') THEN
                   C2 = 'U'
                ELSE IF (CY .EQ. '2') THEN
                   C2 = 'V'
                ELSE
                   C2 = 'W'
                ENDIF
                GWGLAB(NX) = GWGLAB(L)
                INTNOD(NX) = NEWNOD(K)
                EXTNOD(NX) = C1//C2//'  '
             ENDIF
       ENDIF
*
 799   CONTINUE
*
       NEXT = NX
       RETURN
       END
