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
*      Subroutine PRTOT
*      ----------------
*
C...   Title: Calculation & output of the cost specification
*
*      Written: XX-XX-XX by A.N.Other  , XXXX
*      Revised: 85-10-18 by Ron Bell   , TRAFO/IK
*      Revised: 88-02-10 by Ron Bell   , TRAFO/IK
*
************************************************************************
*
       SUBROUTINE PRTOT(NWILI,NG,CHCOST,JFC)
*
C... Declarations
*
CC*SBBL Start of the declarations block.
*
       COMMON  /CTOTAL/  CTOS11,CTOS21,CTOS5X,CTOSXX,CTOGXX,CTOMXX
       COMMON
     & /CCOST1/  CS011(9),CM011A(9),CM011B(9),CM011C(9),CM011D(9),
     &           CM011(9),CS012(9),CM012(9),CS013(9),CM013(9),
     &           CS014(9),CG014(9),CM014(9),CS015(9),CM015(9),
     &           CS016(9),CM016(9),CS017(9),CM017(9),
     &           CS042(9),CM042(9),CM047(9)
       COMMON
     & /CCOST2/  CS021,CG021,CM021,
     &           CS022S,CS022W,CG022,CM022,CS023,CM023
       COMMON
     & /CCOST3/  CS031(4),CM031(4),CS032(4),
     &           CS033(4),CG033(4),CM033(4),
     &           CM034(4)
       COMMON
     & /CCOST4/  CS041,CG041,CM041,CS043,CM043,
     &           CS044,CM044,CS045,CM045,CM046,CG045
       COMMON
     & /CCOST5/  CS051,CG051,CM051,CP051,CS052,CG052,CM052,CP052,CG0521,
     &           CS053,CG053,CM053,CS054,CG054,CM054,
     &           CS06,CG06,CM06,CS07,CG07,CM07,CP07
       COMMON
     & /CCOST6/  CS08,CG08,CM08,CP08,CS111,CG111,CM111,
     &           CS112,CG112,CM112,CS12,CG12,CM12
       COMMON
     & /CCOST7/  CS131,COIL,CG131,CM131,
     &           CS132S,CS132A,CG132,CM132,CM133,CG14,CM14,COILF,COILA
       COMMON
     &   /CSUB00/ CSUB01(9),CSUB02,CSUB03,CSUB04,CSUB05,
     &            CSUB06,CSUB07,CSUB08,CSUB11,CSUB12,CSUB13,CSUB14
*
       REAL      CTOTOT,PERCNT
*
       INTEGER   IWDG,JFC,JTML,NG,NWILI
*
       CHARACTER CHCOST*6
*
CC*SEBL End of the declarations block.
*
       CTOS11=0.
       CTOSXX=0.
       CTOGXX=0.
       CTOMXX=0.
*
C... Summate costs for all windings
*
       DO 100 IWDG=1,NWILI
       CTOS11=CTOS11+CS011(IWDG)
       CTOSXX=CTOSXX+CS012(IWDG)+CS013(IWDG)+CS014(IWDG)+CS015(IWDG)
     &         +CS016(IWDG)+CS017(IWDG)+CS042(IWDG)
       CTOMXX=CTOMXX+CM011(IWDG)+CM012(IWDG)+CM013(IWDG)+CM014(IWDG)+
     &          CM015(IWDG)+CM016(IWDG)+CM017(IWDG)+CM042(IWDG)+
     &          CM047(IWDG)
  100  CTOGXX=CTOGXX+CG014(IWDG)
*
       CTOS21=       CS021
       CTOSXX=CTOSXX+      CS022W+CS023
       CTOGXX=CTOGXX+CG021+CG022
       CTOMXX=CTOMXX+CM021+CM022+CM023
*
C... Summate costs for all terminals
*
       DO 200 JTML=1,NG
       CTOSXX=CTOSXX+CS031(JTML)+CS032(JTML)+CS033(JTML)
       CTOGXX=CTOGXX+CG033(JTML)
  200  CTOMXX=CTOMXX+CM031(JTML)+CM033(JTML)+CM034(JTML)
*
       CTOS5X=CS051+CS07+CS08+CS022S
       CTOSXX=CTOSXX+CS041+CS043+CS044+CS045+
     &               CS052+CS053+CS054+CS06+CS111+CS112+CS12+
     &               CS131+CS132S+CS132A
       CTOGXX=CTOGXX+CG041+CG045+CG051+CG052+CG0521+CG053+
     &               CG054+CG06+CG07+CG08+CG111+CG112+CG12+
     &               CG131+CG132+CG14
       CTOMXX=CTOMXX+CM041+CM043+CM044+CM045+CM046+
     &               CM051+CM052+CM053+CM054+CM06+CM07+
     &               CM08+CM111+CM112+CM12+
     &               CM131+CM132+CM133+CM14+
     &               CP051+CP052+CP07+CP08
       CTOTOT=CTOS11+CTOS21+CTOS5X+CTOSXX+CTOGXX+CTOMXX
       PERCNT=100./CTOTOT
       WRITE(JFC,510)  'SPECIFICATION OF COSTS  ',CHCOST,'%'
       WRITE(JFC,520)  'Conductor  Al + Cu     ',CTOS11,CTOS11*PERCNT
       WRITE(JFC,520)  'Core steel laminated   ',CTOS21,CTOS21*PERCNT
       WRITE(JFC,520)  'Steel (Tank,Coreclamps)',CTOS5X,CTOS5X*PERCNT
       WRITE(JFC,520)  'Other shaped material  ',CTOSXX,CTOSXX*PERCNT
       WRITE(JFC,520)  'Goods (Premanufactured)',CTOGXX,CTOGXX*PERCNT
       WRITE(JFC,520)  'Labour+labour overhead ',CTOMXX,CTOMXX*PERCNT
       WRITE(JFC,520)  '     TOTAL (OVNV-DATOR)',CTOTOT,100.
       RETURN
*
  510  FORMAT ('0'/15X,A24,3X,A6,4X,A1/15X,21('*'))
  520  FORMAT (12X,A23,F11.0,F8.1)
*
       END
