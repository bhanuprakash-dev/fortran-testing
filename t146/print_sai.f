       SUBROUTINE PRINT_SAI

       include'cominp.h'
       include'com1.h'

        print*,'*********INSIDE PRINT_SAI****************'

c       write(*,*)'Reactance T 1-3',XZ0001(1)
c       write(*,*)'Frequency (Hz)',XZ0001(2)
c       write(*,*)'Reactance T 2-3',XZ0001(3)
c       write(*,*)'Reactance T 1-2',XZ0001(4)
c       write(*,*)'Overvoltage (%)',XZ0001(5)
c       write(*,*)'Eval.Noload loss',XZ0001(6)
c       write(*,*)'Eval.Load loss',XZ0001(7)
c       write(*,*)'XZ0001(8)= ',XZ0001(8)
c       write(*,*)'Eval.Sum loss',XZ0001(9)
c       write(*,)'XZ0001(10)= ',XZ0001(10)
c       write(*,)'Rated power T1',XZ0001(11)
c       write(*,)'Rated power T2',XZ0001(12)
c       write(*,)'Rated power T3',XZ0001(13)
c       write(*,)'Rated power T4',XZ0001(14)
c       write(*,)'Loss ratio',XZ0001(15)
c       write(*,)'Main voltage T1',XZ0001(16)
c       write(*,)'Main voltage T2',XZ0001(17)
c       write(*,)'Main voltage T3',XZ0001(18)
c       write(*,)'Main voltage T4',XZ0001(19)
c       write(*,)'Max comp.price',XZ0001(20)
c       write(*,)'Reg.volt.T1 (%)',XZ0001(21)
c       write(*,)'Reg.volt.T2 (%)',XZ0001(22)
c       write(*,)'Reg.volt.T3 (%)',XZ0001(23)
c       write(*,)'Product class',XZ0001(24)
c       write(*,)'Monthly Infl.(%)',XZ0001(25)
c       write(*,)'Impulse volt. T1',XZ0001(26)
c       write(*,)'Impulse volt. T2',XZ0001(27)
c       write(*,)'Impulse volt. T3',XZ0001(28)
c       write(*,)'Impulse volt. T4',XZ0001(29)
c       write(*,)'Trnsp.dist in SW',XZ0001(30)
c       write(*,)'Scpower T1 (GVA)',XZ0001(31)
c       write(*,)'Scpower T2 (GVA)',XZ0001(32)
c       write(*,)'Scpower T3 (GVA)',XZ0001(33)
c       write(*,)'Loss Scale fact',XZ0001(34)
c       write(*,)'Cost Scale fact',XZ0001(35)
c       write(*,)'Int.reactor (%)',XZ0001(36)
c       write(*,)'Int.reactor (%)',XZ0001(37)
c       write(*,)'Int.reactor (%)',XZ0001(38)
c       write(*,)'K1 in PT calc.',XZ0001(39)
c       write(*,)'K2 in PT calc.',XZ0001(40)
c       write(*,)'Fraction W1 (pu)',XZ0001(41)
c       write(*,)'Fraction W2 (pu)',XZ0001(42)
c       write(*,)'Fraction W3 (pu)',XZ0001(43)
c       write(*,)'Fraction W4 (pu)',XZ0001(44)
c       write(*,)'Fraction W5 (pu)',XZ0001(45)
c       write(*,)'Fraction W6 (pu)',XZ0001(46)
c       write(*,)'Fraction W7 (pu)',XZ0001(47)
c       write(*,)'Fraction W8 (pu)',XZ0001(48)
c       write(*,)'Fraction W9 (pu)',XZ0001(49)
c       write(*,)'XZ0001(50)= ',XZ0001(50)
c       write(*,)'Current dens. W1',XZ0001(51)
c       write(*,)'Current dens. W2',XZ0001(52)
c       write(*,)'Current dens. W3',XZ0001(53)
c       write(*,)'Current dens. W4',XZ0001(54)
c       write(*,)'Current dens. W5',XZ0001(55)
c       write(*,)'Current dens. W6',XZ0001(56)
c       write(*,)'Current dens. W7',XZ0001(57)
c       write(*,)'Current dens. W8',XZ0001(58)
c       write(*,)'Current dens. W9',XZ0001(59)
c       write(*,)'Mass-factor',XZ0001(60)
c       write(*,)'Conduct area W1',XZ0001(61)
c       write(*,)'Conduct area W2',XZ0001(62)
c       write(*,)'Conduct area W3',XZ0001(63)
c       write(*,)'Conduct area W4',XZ0001(64)
c       write(*,)'Conduct area W5',XZ0001(65)
c       write(*,)'Conduct area W6',XZ0001(66)
c       write(*,)'Conduct area W7',XZ0001(67)
c       write(*,)'Conduct area W8',XZ0001(68)
c       write(*,)'Conduct area W9',XZ0001(69)
c       write(*,)'XZ0001(70)= ',XZ0001(70)
c       write(*,)'Main duct ins.W1',XZ0001(71)
c       write(*,)'Main duct ins.W2',XZ0001(72)
c       write(*,)'Main duct ins.W3',XZ0001(73)
c       write(*,)'Main duct ins.W4',XZ0001(74)
c       write(*,)'Main duct ins.W5',XZ0001(75)
c       write(*,)'Main duct ins.W6',XZ0001(76)
c       write(*,)'Main duct ins.W7',XZ0001(77)
c       write(*,)'Main duct ins.W8',XZ0001(78)
c       write(*,)'Main duct ins.W9',XZ0001(79)
c       write(*,)'Fillf.n.ext.varn',XZ0001(80)
c       write(*,)'Space factor W1',XZ0001(81)
c       write(*,)'Space factor W2',XZ0001(82)
c       write(*,)'Space factor W3',XZ0001(83)
c       write(*,)'Space factor W4',XZ0001(84)
c       write(*,)'Space factor W5',XZ0001(85)
c       write(*,)'Space factor W6',XZ0001(86)
c       write(*,)'Space factor W7',XZ0001(87)
c       write(*,)'Space factor W8',XZ0001(88)
c       write(*,)'Space factor W9',XZ0001(89)
c       write(*,)'Fillf.ext.varn',XZ0001(90)
c       write(*,)'Winding width W1',XZ0001(91)
c       write(*,)'Winding width W2',XZ0001(92)
c       write(*,)'Winding width W3',XZ0001(93)
c       write(*,)'Winding width W4',XZ0001(94)
c       write(*,)'Winding width W5',XZ0001(95)
c       write(*,)'Winding width W6',XZ0001(96)
c       write(*,)'Winding width W7',XZ0001(97)
c       write(*,)'Winding width W8',XZ0001(98)
c       write(*,)'Winding width W9',XZ0001(99)
c       write(*,)'Temprise topoil',XZ0001(100)



       write(*,*)'No of Windings=',RWILI
       write(*,*)'IDENT=',IDENT
       write(*,*)'TNODE(1),TNODE(2),TNODE(3),TNODE(4) ',(TNODE(i),i=1,4)
       write(*,*)'SRATE(1),SRATE(2),SRATE(3),SRATE(4) ',(SRATE(i),i=1,4)
       write(*,*)'Loss calculation at step=',CALCT
        write(*,*)'KCON(1),KCON(2),KCON(3),KCON(4) ',(KCON(i),i=1,4)
        write(*,*)'RegT(1),RegT(2),RegT(3),RegT(4) ',(KTYPRW(i),i=1,4)
        write(*,*)'PSTP(1),PSTP(2),PSTP(3),PSTP(4)',(NPSTEP(i),i=1,4)
        write(*,*)'NSTP(1),NSTP(2),NSTP(3),NSTP(4)',(NMSTEP(i),i=1,4)
        write(*,*)'STPS(1),STPS(2),STPS(3),STPS(4)',(PUSTEP(i),i=1,4)
        write(*,*)'USURG(1),USURG(2),USURG(3),USURG(4)',
     &             (USURG(i),i=1,4)
        write(*,*)'SLINE(1),SLINE(2),SLINE(3),SLINE(4)',
     &                      (SLINE(i),i=1,4)
        write(*,*)'CGRAD(1),CGRAD(2),CGRAD(3),CGRAD(4)',
     &                      (CGRAD(i),i=1,4)
        write(*,*)'FREQUENCY=',FREQ
        write(*,*)'No. of Phases=',NPHAS
        write(*,*)'TYPREG=',TYPREG
       write(*,*)'No load loss=',EVALP0
       write(*,*)'Load Loss=',EVALPK
       write(*,*)'MNLCOD=',MNLCOD
       write(*,*)'CPUIND=',CPUIND
       write(*,*)'CPUFIX=',CPUFIX
       write(*,*)'KCOOL=',KCOOL
       write(*,*)'FONAN=',FONAN
       write(*,*)'OPERAT=',OPERAT
       write(*,*)'DETPRE=',DETPRE
       write(*,*)'XREA(1),XREA(2),XREA(3)',
     &                      (XREA(i),i=1,3)


       write(*,*)
     & 'WTO(1),WTO(2),WTO(3),WTO(4),WTO(5),WTO(6),WTO(7),WTO(8),WTO(9)'
     &   ,(WNOD1(i),i=1,9)
       write(*,*)
     & 'WBM(1),WBM(2),WBM(3),WBM(4),WBM(5),WBM(6),WBM(7),WBM(8),WBM(9)'
     &   ,(WNOD2(i),i=1,9)
       write(*,*)
     & 'FCD(1),FCD(2),FCD(3),FCD(4),FCD(5),FCD(6),FCD(7),FCD(8),FCD(9)'
     &   ,(KWIFUN(i),i=1,9)
        write(*,*)
     & 'YTO(1),YTO(2),YTO(3),YTO(4),YTO(5),YTO(6),YTO(7),YTO(8),YTO(9)'
     &,(DUYOKE(i),i=1,9)
        write(*,*)
     & 'YBM(1),YBM(2),YBM(3),YBM(4),YBM(5),YBM(6),YBM(7),YBM(8),YBM(9)'
     &,(DLYOKE(i),i=1,9)
        write(*,*)
     & 'DWT(1),DWT(2),DWT(3),DWT(4),DWT(5),DWT(6),DWT(7),DWT(8),DWT(9)'
     &,(XZ0671(i),i=1,9)
        write(*,*)
     & 'WFR(1),WFR(2),WFR(3),WFR(4),WFR(5),WFR(6),WFR(7),WFR(8),WFR(9)'
     &,(FRACT(i),i=1,9)
       write(*,*)
     & 'CTY(1),CTY(2),CTY(3),CTY(4),CTY(5),CTY(6),CTY(7),CTY(8),CTY(9)'
     &   ,(CBLTYP(i),i=1,9)
        write(*,*)
     & 'BPT(1),BPT(2),BPT(3),BPT(4),BPT(5),BPT(6),BPT(7),BPT(8),BPT(9)'
     &,(XZ0681(i),i=1,9)
        write(*,*)
     & 'MCD(1),MCD(2),MCD(3),MCD(4),MCD(5),MCD(6),MCD(7),MCD(8),MCD(9)'
     &,(MXCURD(i),i=1,9)
        write(*,*)
     & 'MTE(1),MTE(2),MTE(3),MTE(4),MTE(5),MTE(6),MTE(7),MTE(8),MTE(9)'
     &,(TENSM(i),i=1,9)
        write(*,*)
     & 'MPR(1),MPR(2),MPR(3),MPR(4),MPR(5),MPR(6),MPR(7),MPR(8),MPR(9)'
     &,(PRESSM(i),i=1,9)
       write(*,*)
     & 'WOP(1),WOP(2),WOP(3),WOP(4),WOP(5),WOP(6),WOP(7),WOP(8),WOP(9)'
     &   ,(WNDOPT(i),i=1,9)
        write(*,*)
     & 'SPF(1),SPF(2),SPF(3),SPF(4),SPF(5),SPF(6),SPF(7),SPF(8),SPF(9)'
     &,(FILLF(i),i=1,9)
        write(*,*)
     & 'BWD(1),BWD(2),BWD(3),BWD(4),BWD(5),BWD(6),BWD(7),BWD(8),BWD(9)'
     &,(BWIND(i),i=1,9)
        write(*,*)
     & 'ACD(1),ACD(2),ACD(3),ACD(4),ACD(5),ACD(6),ACD(7),ACD(8),ACD(9)'
     &,(XZ0651(i),i=1,9)
       write(*,*)'HLIMB=',HLIMB
       write(*,*)'DCORE=',DCORE
       write(*,*)'BLIMB=',BLIMB
       write(*,*)'USHORE=',USHORE
       write(*,*)'LAYOUT=',LAYOUT
       write(*,*)'ROPT(1),ROPT(2),ROPT(3),ROPT(4)= ',(REOPT(i),i=1,4)
       write(*,*)
     & 'KTP(1),KTP(2),KTP(3),KTP(4),KTP(5),KTP(6),KTP(7),KTP(8),KTP(9)'
     &   ,(KWITYP(i),i=1,9)
        write(*,*)
     & 'NGP(1),NGP(2),NGP(3),NGP(4),NGP(5),NGP(6),NGP(7),NGP(8),NGP(9)'
     &,(NGROUP(i),i=1,9)
        write(*,*)
     & 'CMT(1),CMT(2),CMT(3),CMT(4),CMT(5),CMT(6),CMT(7),CMT(8),CMT(9)'
     &,(CONMAT(i),i=1,9)
        write(*,*)
     & 'HCL(1),HCL(2),HCL(3),HCL(4),HCL(5),HCL(6),HCL(7),HCL(8),HCL(9)'
     &,(HCLAC(i),i=1,9)
        write(*,*)
     & 'CVS(1),CVS(2),CVS(3),CVS(4),CVS(5),CVS(6),CVS(7),CVS(8),CVS(9)'
     &,(COVSTR(i),i=1,9)
        write(*,*)
     & 'CVC(1),CVC(2),CVC(3),CVC(4),CVC(5),CVC(6),CVC(7),CVC(8),CVC(9)'
     &,(COVCBL(i),i=1,9)
        write(*,*)
     & 'COR(1),COR(2),COR(3),COR(4),COR(5),COR(6),COR(7),COR(8),COR(9)'
     &,(CONVAR(i),i=1,9)
        write(*,*)
     & 'TPX(1),TPX(2),TPX(3),TPX(4),TPX(5),TPX(6),TPX(7),TPX(8),TPX(9)'
     &,(TCEPOX(i),i=1,9)
        write(*,*)
     & 'EXR(1),EXR(2),EXR(3),EXR(4),EXR(5),EXR(6),EXR(7),EXR(8),EXR(9)'
     &,(EXTRAR(i),i=1,9)
        write(*,*)
     & 'HPT(1),HPT(2),HPT(3),HPT(4),HPT(5),HPT(6),HPT(7),HPT(8),HPT(9)'
     &,(HPART(i),i=1,9)
        write(*,*)
     & 'NDT(1),NDT(2),NDT(3),NDT(4),NDT(5),NDT(6),NDT(7),NDT(8),NDT(9)'
     &,(NCDUCT(i),i=1,9)
        write(*,*)
     & 'XOG(1),XOG(2),XOG(3),XOG(4),XOG(5),XOG(6),XOG(7),XOG(8),XOG(9)'
     &,(XOILGR(i),i=1,9)
        write(*,*)
     & 'CBI(1),CBI(2),CBI(3),CBI(4),CBI(5),CBI(6),CBI(7),CBI(8),CBI(9)'
     &,(COMBWI(i),i=1,9)
	write(*,*)'CKTANK=',CKTANK
	write(*,*)'OPTTNK=',OPTTNK
	write(*,*)'BTANK=',BTANK
	write(*,*)'LTANK=',LTANK
	write(*,*)'HTANK=',HTANK
	write(*,*)'DPHAS=',DPHAS
	write(*,*)'DPHSL=',DPHSL
	write(*,*)'DWITA=',DWITA
	write(*,*)'DCOCOV=',DCOCOV
	write(*,*)'EXTANK=',EXTANK
	write(*,*)'SNDSCR=',SNDSCR
	write(*,*)'ALSHLD=',ALSHLD
	write(*,*)'T146AC=',(T146AC(i),i=1,2)
	write(*,*)'CORESE=',CORESE
	write(*,*)'CORTYP=',CORTYP
	write(*,*)'TWSUP=',TWSUP
	write(*,*)'QMONT=',QMONT
	write(*,*)'COREMA=',COREMA
	write(*,*)'KBAND=',KBAND
	write(*,*)'STEPLA=',STEPLA
	write(*,*)'UMAXPU=',UMAXPU
	write(*,*)'UTURN=',UTURN
	write(*,*)'HYOKAD=',HYOKAD
	write(*,*) 'BLDING=',BLDING
	write(*,*) 'CVARN=',CVARN
	write(*,*)'TOPDTO=',TOPDTO
	write(*,*)'NCOOLX=',NCOOLX
	write(*,*)'TWINDM=',TWINDM
	write(*,*)'TTOILM=',TTOILM
	write(*,*)'BLTOPM=',BLTOPM
	write(*,*)'HLIMBM=',HLIMBM
	write(*,*)'HLIMBN=',HLIMBN
	write(*,*)'MAXP0=',MAXP0
	write(*,*)'MAXPK=',MAXPK
	write(*,*)'RLTANM=',RLTANM
	write(*,*)'BTANKM=',BTANKM
	write(*,*)'HTANKM',HTANKM
	write(*,*)'GTRPM=',GTRPM
	write(*,*)'SOUNDM=',SOUNDM
	write(*,*)'DLMBMA=',DLMBMA
	write(*,*)'DLMBMI=',DLMBMI
	write(*,*)'STACKF=', STACKF
	write(*,*)'TEMPLO=', TEMPLO
	write(*,*)'SPFADU=', SPFADU
		write(*,*)'BOOSMA=', BOOSMA
		write(*,*)'BOOSVO=', BOOSVO
		write(*,*)'BOOSRE=', BOOSRE
		write(*,*)'BOOSTR=', BOOSTR
	print*,'Coming out of printsai'
	RETURN
	END
