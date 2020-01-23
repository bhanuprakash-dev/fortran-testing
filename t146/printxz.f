        SUBROUTINE PRINTXZ
        include'com1.h'
        write(*,*) 'IARR'
        write(*,*) 'Number of phases    ',XZ0301( 01) 
        write(*,*) 'Trnsp.prof. code    ',XZ0301( 02) 
        write(*,*) 'Max No of calcs.    ',XZ0301( 03) 
        write(*,*) 'No.of + steps T1    ',XZ0301( 06) 
        write(*,*) 'No.of + steps T2    ',XZ0301( 07) 
        write(*,*) 'No.of + steps T3    ',XZ0301( 08) 
        write(*,*) 'No.of - steps T1    ',XZ0301( 11) 
        write(*,*) 'No.of - steps T2    ',XZ0301( 12) 
        write(*,*) 'No.of - steps T3    ',XZ0301( 13) 
        write(*,*) 'Term.connect. W1    ',XZ0301( 21) 
        write(*,*) 'Term.connect. W2    ',XZ0301( 22) 
        write(*,*) 'Term.connect. W3    ',XZ0301( 23) 
        write(*,*) 'Term.connect. W4    ',XZ0301( 24) 
        write(*,*) 'Term.connect. W5    ',XZ0301( 25) 
        write(*,*) 'Term.connect. W6    ',XZ0301( 26) 
        write(*,*) 'Term.connect. W7    ',XZ0301( 27) 
        write(*,*) 'Term.connect. W8    ',XZ0301( 28) 
        write(*,*) 'Term.connect. W9    ',XZ0301( 29) 
        write(*,*) 'Function code W1    ',XZ0301( 31) 
        write(*,*) 'Function code W2    ',XZ0301( 32) 
        write(*,*) 'Function code W3    ',XZ0301( 33) 
        write(*,*) 'Function code W4    ',XZ0301( 34) 
        write(*,*) 'Function code W5    ',XZ0301( 35) 
        write(*,*) 'Function code W6    ',XZ0301( 36) 
        write(*,*) 'Function code W7    ',XZ0301( 37) 
        write(*,*) 'Function code W8    ',XZ0301( 38) 
        write(*,*) 'Function code W9    ',XZ0301( 39) 
        write(*,*) 'No.Par.groups W1    ',XZ0301( 41) 
        write(*,*) 'No.Par.groups W2    ',XZ0301( 42) 
        write(*,*) 'No.Par.groups W3    ',XZ0301( 43) 
        write(*,*) 'No.Par.groups W4    ',XZ0301( 44) 
        write(*,*) 'No.Par.groups W5    ',XZ0301( 45) 
        write(*,*) 'No.Par.groups W6    ',XZ0301( 46) 
        write(*,*) 'No.Par.groups W7    ',XZ0301( 47) 
        write(*,*) 'No.Par.groups W8    ',XZ0301( 48) 
        write(*,*) 'No.Par.groups W9    ',XZ0301( 49) 
        write(*,*) 'No. Wound limbs     ',XZ0301( 51) 
        write(*,*) 'Tank type-code      ',XZ0301( 53) 

        write(*,*) 'AARR'
        write(*,*)'Reactance T 1-3     ',XZ0001(01) 
        write(*,*) 'Frequency (Hz)      ',XZ0001( 02) 
        write(*,*) 'Reactance T 2-3     ',XZ0001( 03) 
        write(*,*) 'Reactance T 1-2     ',XZ0001( 04) 
        write(*,*) 'Overvoltage (%)     ',XZ0001( 05) 
        write(*,*) 'Eval.Noload loss    ',XZ0001( 06) 
        write(*,*) 'Eval.Load loss      ',XZ0001( 07) 
        write(*,*) 'Eval.Sum loss       ',XZ0001( 09) 
        write(*,*) 'Rated power T1      ',XZ0001( 11) 
        write(*,*) 'Rated power T2      ',XZ0001( 12) 
        write(*,*) 'Rated power T3      ',XZ0001( 13) 
        write(*,*) 'Rated power T4      ',XZ0001( 14) 
        write(*,*) 'Loss ratio          ',XZ0001( 15) 
        write(*,*) 'Main voltage T1     ',XZ0001( 16) 
        write(*,*) 'Main voltage T2     ',XZ0001( 17) 
        write(*,*) 'Main voltage T3     ',XZ0001( 18) 
        write(*,*) 'Main voltage T4     ',XZ0001( 19) 
        write(*,*) 'Max comp.price      ',XZ0001( 20) 
        write(*,*) 'Reg.volt.T1 (%)     ',XZ0001( 21) 
        write(*,*) 'Reg.volt.T2 (%)     ',XZ0001( 22) 
        write(*,*) 'Reg.volt.T3 (%)     ',XZ0001( 23) 
        write(*,*) 'Product class       ',XZ0001( 24) 
        write(*,*) 'Monthly Infl.(%)    ',XZ0001( 25) 
        write(*,*) 'Impulse volt. T1    ',XZ0001( 26) 
        write(*,*) 'Impulse volt. T2    ',XZ0001( 27) 
        write(*,*) 'Impulse volt. T3    ',XZ0001( 28) 
        write(*,*) 'Impulse volt. T4    ',XZ0001( 29) 
        write(*,*) 'Trnsp.dist in SW    ',XZ0001( 30) 
        write(*,*) 'Scpower T1 (GVA)    ',XZ0001( 31) 
        write(*,*) 'Scpower T2 (GVA)    ',XZ0001( 32) 
        write(*,*) 'Scpower T3 (GVA)    ',XZ0001( 33) 
        write(*,*) 'Loss Scale fact     ',XZ0001( 34) 
        write(*,*) 'Cost Scale fact     ',XZ0001( 35) 
        write(*,*) 'Int.reactor (%)     ',XZ0001( 36) 
        write(*,*) 'Int.reactor (%)     ',XZ0001( 37) 
        write(*,*) 'Int.reactor (%)     ',XZ0001( 38) 
        write(*,*) 'K1 in PT calc.      ',XZ0001( 39) 
        write(*,*) 'K2 in PT calc.      ',XZ0001( 40) 
        write(*,*) 'Fraction W1 (pu)    ',XZ0001( 41) 
        write(*,*) 'Fraction W2 (pu)    ',XZ0001( 42) 
        write(*,*) 'Fraction W3 (pu)    ',XZ0001( 43) 
        write(*,*) 'Fraction W4 (pu)    ',XZ0001( 44) 
        write(*,*) 'Fraction W5 (pu)    ',XZ0001( 45) 
        write(*,*) 'Fraction W6 (pu)    ',XZ0001( 46) 
        write(*,*) 'Fraction W7 (pu)    ',XZ0001( 47) 
        write(*,*) 'Fraction W8 (pu)    ',XZ0001( 48) 
        write(*,*) 'Fraction W9 (pu)    ',XZ0001( 49) 
        write(*,*) 'Current dens. W1    ',XZ0001( 51) 
        write(*,*) 'Current dens. W2    ',XZ0001( 52) 
        write(*,*) 'Current dens. W3    ',XZ0001( 53) 
        write(*,*) 'Current dens. W4    ',XZ0001( 54) 
        write(*,*) 'Current dens. W5    ',XZ0001( 55) 
        write(*,*) 'Current dens. W6    ',XZ0001( 56) 
        write(*,*) 'Current dens. W7    ',XZ0001( 57) 
        write(*,*) 'Current dens. W8    ',XZ0001( 58) 
        write(*,*) 'Current dens. W9    ',XZ0001( 59) 
        write(*,*) 'Mass-factor         ',XZ0001( 60) 
        write(*,*) 'Conduct area W1     ',XZ0001( 61) 
        write(*,*) 'Conduct area W2     ',XZ0001( 62) 
        write(*,*) 'Conduct area W3     ',XZ0001( 63) 
        write(*,*) 'Conduct area W4     ',XZ0001( 64) 
        write(*,*) 'Conduct area W5     ',XZ0001( 65) 
        write(*,*) 'Conduct area W6     ',XZ0001( 66) 
        write(*,*) 'Conduct area W7     ',XZ0001( 67) 
        write(*,*) 'Conduct area W8     ',XZ0001( 68) 
        write(*,*) 'Conduct area W9     ',XZ0001( 69) 
        write(*,*) 'Main duct ins.W1    ',XZ0001( 71) 
        write(*,*) 'Main duct ins.W2    ',XZ0001( 72) 
        write(*,*) 'Main duct ins.W3    ',XZ0001( 73) 
        write(*,*) 'Main duct ins.W4    ',XZ0001( 74) 
        write(*,*) 'Main duct ins.W5    ',XZ0001( 75) 
        write(*,*) 'Main duct ins.W6    ',XZ0001( 76) 
        write(*,*) 'Main duct ins.W7    ',XZ0001( 77) 
        write(*,*) 'Main duct ins.W8    ',XZ0001( 78) 
        write(*,*) 'Main duct ins.W9    ',XZ0001( 79) 
        write(*,*) 'Fillf.n.ext.varn    ',XZ0001( 80) 
        write(*,*) 'Space factor W1     ',XZ0001( 81) 
        write(*,*) 'Space factor W2     ',XZ0001( 82) 
        write(*,*) 'Space factor W3     ',XZ0001( 83) 
        write(*,*) 'Space factor W4     ',XZ0001( 84) 
        write(*,*) 'Space factor W5     ',XZ0001( 85) 
        write(*,*) 'Space factor W6     ',XZ0001( 86) 
        write(*,*) 'Space factor W7     ',XZ0001( 87) 
        write(*,*) 'Space factor W8     ',XZ0001( 88) 
        write(*,*) 'Space factor W9     ',XZ0001( 89) 
        write(*,*) 'Fillf.ext.varn      ',XZ0001( 90) 
        write(*,*) 'Winding width W1    ',XZ0001( 91) 
        write(*,*) 'Winding width W2    ',XZ0001( 92) 
        write(*,*) 'Winding width W3    ',XZ0001( 93) 
        write(*,*) 'Winding width W4    ',XZ0001( 94) 
        write(*,*) 'Winding width W5    ',XZ0001( 95) 
        write(*,*) 'Winding width W6    ',XZ0001( 96) 
        write(*,*) 'Winding width W7    ',XZ0001( 97) 
        write(*,*) 'Winding width W8    ',XZ0001( 98) 
        write(*,*) 'Winding width W9    ',XZ0001( 99) 
        write(*,*) 'Temprise topoil     ',XZ0001(100) 
        write(*,*) 'Yoke-distance W1    ',XZ0001(101) 
        write(*,*) 'Yoke-distance W2    ',XZ0001(102) 
        write(*,*) 'Yoke-distance W3    ',XZ0001(103) 
        write(*,*) 'Yoke-distance W4    ',XZ0001(104) 
        write(*,*) 'Yoke-distance W5    ',XZ0001(105) 
        write(*,*) 'Yoke-distance W6    ',XZ0001(106) 
        write(*,*) 'Yoke-distance W7    ',XZ0001(107) 
        write(*,*) 'Yoke-distance W8    ',XZ0001(108) 
        write(*,*) 'Yoke-distance W9    ',XZ0001(109) 
        write(*,*) 'Temprise MO ONAN    ',XZ0001(110) 
        write(*,*) 'Cond. width W1      ',XZ0001(111) 
        write(*,*) 'Cond. width W2      ',XZ0001(112) 
        write(*,*) 'Cond. width W3      ',XZ0001(113) 
        write(*,*) 'Cond. width W4      ',XZ0001(114) 
        write(*,*) 'Cond. width W5      ',XZ0001(115) 
        write(*,*) 'Cond. width W6      ',XZ0001(116) 
        write(*,*) 'Cond. width W7      ',XZ0001(117) 
        write(*,*) 'Cond. width W8      ',XZ0001(118) 
        write(*,*) 'Cond. width W9      ',XZ0001(119) 
        write(*,*) 'Temprise MO ONAF    ',XZ0001(120) 
        write(*,*) 'Tens.stress W1      ',XZ0001(121) 
        write(*,*) 'Tens.stress W2      ',XZ0001(122) 
        write(*,*) 'Tens.stress W3      ',XZ0001(123) 
        write(*,*) 'Tens.stress W4      ',XZ0001(124) 
        write(*,*) 'Tens.stress W5      ',XZ0001(125) 
        write(*,*) 'Tens.stress W6      ',XZ0001(126) 
        write(*,*) 'Tens.stress W7      ',XZ0001(127) 
        write(*,*) 'Tens.stress W8      ',XZ0001(128) 
        write(*,*) 'Tens.stress W9      ',XZ0001(129) 
        write(*,*) 'Temprise MO OFAF    ',XZ0001(130) 
        write(*,*) 'Max. Press. W1      ',XZ0001(131) 
        write(*,*) 'Max. Press. W2      ',XZ0001(132) 
        write(*,*) 'Max. Press. W3      ',XZ0001(133) 
        write(*,*) 'Max. Press. W4      ',XZ0001(134) 
        write(*,*) 'Max. Press. W5      ',XZ0001(135) 
        write(*,*) 'Max. Press. W6      ',XZ0001(136) 
        write(*,*) 'Max. Press. W7      ',XZ0001(137) 
        write(*,*) 'Max. Press. W8      ',XZ0001(138) 
        write(*,*) 'Max. Press. W9      ',XZ0001(139) 
        write(*,*) 'Core blading        ',XZ0001(140) 
        write(*,*) 'Dist.betw.phases    ',XZ0001(141) 
        write(*,*) 'Dist.to.sidelimb    ',XZ0001(142) 
        write(*,*) 'Core diameter       ',XZ0001(143) 
        write(*,*) 'Turn-voltage        ',XZ0001(145) 
        write(*,*) 'Flux density        ',XZ0001(146) 
        write(*,*) 'Max.flux density    ',XZ0001(147) 
        write(*,*) 'Yokeht.incr.TCA     ',XZ0001(150) 
        write(*,*) 'Dist.wind.-tank     ',XZ0001(151) 
        write(*,*) 'Extens. of tank     ',XZ0001(152) 
        write(*,*) 'Dist.core-cover     ',XZ0001(153) 
        write(*,*) 'Self-cool.ratio     ',XZ0001(154) 
        write(*,*) 'Max.windtemprise    ',XZ0001(155) 
        write(*,*) 'Max.TO.temprise     ',XZ0001(156) 
        write(*,*) 'Yokeht .red. TBA    ',XZ0001(160) 
        write(*,*) 'Re-opt.index(%)     ',XZ0001(161) 
        write(*,*) 'Fixed Price (%)     ',XZ0001(168) 
        write(*,*) 'Min.Core diam.      ',XZ0001(170) 
        write(*,*) 'Max.Core diam.      ',XZ0001(171) 
        write(*,*) 'Max.Trnsp.mass      ',XZ0001(173) 
        write(*,*) 'Max.tank height     ',XZ0001(174) 
        write(*,*) 'Max,tank width      ',XZ0001(175) 
        write(*,*) 'Max.tank length     ',XZ0001(176) 
        write(*,*) 'Max.sound level     ',XZ0001(177) 

        write(*,*) 'TARR'
        write(*,*) 'Core-steel          ',XZ0401( 01) 
        write(*,*) 'Var.flux regul.     ',XZ0401( 02) 
        write(*,*) 'Highcond.Fld.Scr    ',XZ0401( 03) 
        write(*,*) 'Dshell H.cur.wdg    ',XZ0401( 04) 
        write(*,*) 'Regulation T1       ',XZ0401( 06) 
        write(*,*) 'Regulation T2       ',XZ0401( 07) 
        write(*,*) 'Regulation T3       ',XZ0401( 08) 
        write(*,*) 'Type of regul.T1    ',XZ0401( 11) 
        write(*,*) 'Type of regul.T2    ',XZ0401( 12) 
        write(*,*) 'Type of regul.T3    ',XZ0401( 13) 
        write(*,*) 'Connection T1       ',XZ0401( 21) 
        write(*,*) 'Connection T2       ',XZ0401( 22) 
        write(*,*) 'Connection T3       ',XZ0401( 23) 
        write(*,*) 'Connection T4       ',XZ0401( 24) 
        write(*,*) 'Winding type W1     ',XZ0401( 31) 
        write(*,*) 'Winding type W2     ',XZ0401( 32) 
        write(*,*) 'Winding type W3     ',XZ0401( 33) 
        write(*,*) 'Winding type W4     ',XZ0401( 34) 
        write(*,*) 'Winding type W5     ',XZ0401( 35) 
        write(*,*) 'Winding type W6     ',XZ0401( 36) 
        write(*,*) 'Winding type W7     ',XZ0401( 37) 
        write(*,*) 'Winding type W8     ',XZ0401( 38) 
        write(*,*) 'Winding type W9     ',XZ0401( 39) 
        write(*,*) 'Cond.material W1    ',XZ0401( 41) 
        write(*,*) 'Cond.material W2    ',XZ0401( 42) 
        write(*,*) 'Cond.material W3    ',XZ0401( 43) 
        write(*,*) 'Cond.material W4    ',XZ0401( 44) 
        write(*,*) 'Cond.material W5    ',XZ0401( 45) 
        write(*,*) 'Cond.material W6    ',XZ0401( 46) 
        write(*,*) 'Cond.material W7    ',XZ0401( 47) 
        write(*,*) 'Cond.material W8    ',XZ0401( 48) 
        write(*,*) 'Cond.material W9    ',XZ0401( 49) 
        write(*,*) 'Side limb (Y/N)     ',XZ0401( 51) 
        write(*,*) 'Type of cooling     ',XZ0401( 56) 
        write(*,*) 'Type of cable W1    ',XZ0401( 61) 
        write(*,*) 'Type of cable W2    ',XZ0401( 62) 
        write(*,*) 'Type of cable W3    ',XZ0401( 63) 
        write(*,*) 'Type of cable W4    ',XZ0401( 64) 
        write(*,*) 'Type of cable W5    ',XZ0401( 65) 
        write(*,*) 'Type of cable W6    ',XZ0401( 66) 
        write(*,*) 'Type of cable W7    ',XZ0401( 67) 
        write(*,*) 'Type of cable W8    ',XZ0401( 68) 
        write(*,*) 'Type of cable W9    ',XZ0401( 69) 
        write(*,*) 'Tank mass stated    ',XZ0401( 73) 
        write(*,*) 'Sound screens       ',XZ0401( 75) 
        write(*,*) 'Detailed precalc    ',XZ0401( 78) 
        write(*,*) 'Core-banding        ',XZ0401( 80) 
        write(*,*) 'TAD cooling calc    ',XZ0401( 82) 
        write(*,*) 'Dshell H.cur.wdg    ',XZ0401( 95) 

        write(*,*) 'BARR'
        write(*,*) 'No.cool.ducts W1    ',XZ0181( 01) 
        write(*,*) 'No.cool.ducts W2    ',XZ0181( 02) 
        write(*,*) 'No.cool.ducts W3    ',XZ0181( 03) 
        write(*,*) 'No.cool.ducts W4    ',XZ0181( 04) 
        write(*,*) 'No.cool.ducts W5    ',XZ0181( 05) 
        write(*,*) 'No.cool.ducts W6    ',XZ0181( 06) 
        write(*,*) 'No.cool.ducts W7    ',XZ0181( 07) 
        write(*,*) 'No.cool.ducts W8    ',XZ0181( 08) 
        write(*,*) 'No.cool.ducts W9    ',XZ0181( 09) 
        write(*,*) 'Cond.height W1      ',XZ0181( 11) 
        write(*,*) 'Cond.height W2      ',XZ0181( 12) 
        write(*,*) 'Cond.height W3      ',XZ0181( 13) 
        write(*,*) 'Cond.height W4      ',XZ0181( 14) 
        write(*,*) 'Cond.height W5      ',XZ0181( 15) 
        write(*,*) 'Cond.height W6      ',XZ0181( 16) 
        write(*,*) 'Cond.height W7      ',XZ0181( 17) 
        write(*,*) 'Cond.height W8      ',XZ0181( 18) 
        write(*,*) 'Cond.height W9      ',XZ0181( 19) 
        write(*,*) 'Tot.paper cov.W1    ',XZ0181( 21) 
        write(*,*) 'Tot.paper cov.W2    ',XZ0181( 22) 
        write(*,*) 'Tot.paper cov.W3    ',XZ0181( 23) 
        write(*,*) 'Tot.paper cov.W4    ',XZ0181( 24) 
        write(*,*) 'Tot.paper cov.W5    ',XZ0181( 25) 
        write(*,*) 'Tot.paper cov.W6    ',XZ0181( 26) 
        write(*,*) 'Tot.paper cov.W7    ',XZ0181( 27) 
        write(*,*) 'Tot.paper cov.W8    ',XZ0181( 28) 
        write(*,*) 'Tot.paper cov.W9    ',XZ0181( 29) 
        write(*,*) 'Winding support     ',XZ0181( 30) 
        write(*,*) 'Assembly press      ',XZ0181( 31) 
        write(*,*) 'EPS  Opt.const      ',XZ0181( 32) 
        write(*,*) 'FEPS Opt.const      ',XZ0181( 33) 
        write(*,*) 'DRV  Opt.const      ',XZ0181( 34) 
        write(*,*) 'AF   Opt.const      ',XZ0181( 35) 
        write(*,*) 'P(3) Opt.const      ',XZ0181( 36) 
        write(*,*) 'P(13)Opt.const      ',XZ0181( 37) 

        write(*,*) 'UARR'
        write(*,*) 'Comb.wdg.prod W1    ',XZ0581( 01) 
        write(*,*) 'Comb.wdg.prod W2    ',XZ0581( 02) 
        write(*,*) 'Comb.wdg.prod W3    ',XZ0581( 03) 
        write(*,*) 'Comb.wdg.prod W4    ',XZ0581( 04) 
        write(*,*) 'Comb.wdg.prod W5    ',XZ0581( 05) 
        write(*,*) 'Comb.wdg.prod W6    ',XZ0581( 06) 
        write(*,*) 'Comb.wdg.prod W7    ',XZ0581( 07) 
        write(*,*) 'Comb.wdg.prod W8    ',XZ0581( 08) 
        write(*,*) 'Comb.wdg.prod W9    ',XZ0581( 09) 
        write(*,*) 'Transp.cable W1     ',XZ0581( 11) 
        write(*,*) 'Transp.cable W2     ',XZ0581( 12) 
        write(*,*) 'Transp.cable W3     ',XZ0581( 13) 
        write(*,*) 'Transp.cable W4     ',XZ0581( 14) 
        write(*,*) 'Transp.cable W5     ',XZ0581( 15) 
        write(*,*) 'Transp.cable W6     ',XZ0581( 16) 
        write(*,*) 'Transp.cable W7     ',XZ0581( 17) 
        write(*,*) 'Transp.cable W8     ',XZ0581( 18) 
        write(*,*) 'Transp.cable W9     ',XZ0581( 19) 
        write(*,*) 'Optimise tank       ',XZ0581( 20) 
        write(*,*) 'Core type           ',XZ0581( 22) 
        write(*,*) 'Step Lap core       ',XZ0581( 23) 
        write(*,*) 'Varnish copper W1   ',XZ0581( 31) 
        write(*,*) 'Varnish copper W2   ',XZ0581( 32) 
        write(*,*) 'Varnish copper W3   ',XZ0581( 33) 
        write(*,*) 'Varnish copper W4   ',XZ0581( 34) 
        write(*,*) 'Varnish copper W5   ',XZ0581( 35) 
        write(*,*) 'Varnish copper W6   ',XZ0581( 36) 
        write(*,*) 'Varnish copper W7   ',XZ0581( 37) 
        write(*,*) 'Varnish copper W8   ',XZ0581( 38) 
        write(*,*) 'Varnish copper W9   ',XZ0581( 39) 
        write(*,*) 'Epoxy glue on TC W1 ',XZ0581( 41) 
        write(*,*) 'Epoxy glue on TC W2 ',XZ0581( 42) 
        write(*,*) 'Epoxy glue on TC W3 ',XZ0581( 43) 
        write(*,*) 'Epoxy glue on TC W4 ',XZ0581( 44) 
        write(*,*) 'Epoxy glue on TC W5 ',XZ0581( 45) 
        write(*,*) 'Epoxy glue on TC W6 ',XZ0581( 46) 
        write(*,*) 'Epoxy glue on TC W7 ',XZ0581( 47) 
        write(*,*) 'Epoxy glue on TC W8 ',XZ0581( 48) 
        write(*,*) 'Epoxy glue on TC W9 ',XZ0581( 49) 

        write(*,*) 'DARR'
        write(*,*) 'No.guid.rings W1    ',XZ0231( 01) 
        write(*,*) 'No.guid.rings W2    ',XZ0231( 02) 
        write(*,*) 'No.guid.rings W3    ',XZ0231( 03) 
        write(*,*) 'No.guid.rings W4    ',XZ0231( 04) 
        write(*,*) 'No.guid.rings W5    ',XZ0231( 05) 
        write(*,*) 'No.guid.rings W6    ',XZ0231( 06) 
        write(*,*) 'No.guid.rings W7    ',XZ0231( 07) 
        write(*,*) 'No.guid.rings W8    ',XZ0231( 08) 
        write(*,*) 'No.guid.rings W9    ',XZ0231( 09) 
        write(*,*) 'Max.limb height     ',XZ0231( 10) 
        write(*,*) 'Tapered clack W1    ',XZ0231( 11) 
        write(*,*) 'Tapered clack W2    ',XZ0231( 12) 
        write(*,*) 'Tapered clack W3    ',XZ0231( 13) 
        write(*,*) 'Tapered clack W4    ',XZ0231( 14) 
        write(*,*) 'Tapered clack W5    ',XZ0231( 15) 
        write(*,*) 'Tapered clack W6    ',XZ0231( 16) 
        write(*,*) 'Tapered clack W7    ',XZ0231( 17) 
        write(*,*) 'Tapered clack W8    ',XZ0231( 18) 
        write(*,*) 'Tapered clack W9    ',XZ0231( 19) 
        write(*,*) 'Limb height         ',XZ0231( 20) 
        write(*,*) 'Strands/cable W1    ',XZ0231( 21) 
        write(*,*) 'Strands/cable W2    ',XZ0231( 22) 
        write(*,*) 'Strands/cable W3    ',XZ0231( 23) 
        write(*,*) 'Strands/cable W4    ',XZ0231( 24) 
        write(*,*) 'Strands/cable W5    ',XZ0231( 25) 
        write(*,*) 'Strands/cable W6    ',XZ0231( 26) 
        write(*,*) 'Strands/cable W7    ',XZ0231( 27) 
        write(*,*) 'Strands/cable W8    ',XZ0231( 28) 
        write(*,*) 'Strands/cable W9    ',XZ0231( 29) 
        write(*,*) 'Core-varnishing     ',XZ0231( 30) 
        write(*,*) 'Cover,strand W1     ',XZ0231( 31) 
        write(*,*) 'Cover,strand W2     ',XZ0231( 32) 
        write(*,*) 'Cover,strand W3     ',XZ0231( 33) 
        write(*,*) 'Cover,strand W4     ',XZ0231( 34) 
        write(*,*) 'Cover,strand W5     ',XZ0231( 35) 
        write(*,*) 'Cover,strand W6     ',XZ0231( 36) 
        write(*,*) 'Cover,strand W7     ',XZ0231( 37) 
        write(*,*) 'Cover,strand W8     ',XZ0231( 38) 
        write(*,*) 'Cover,strand W9     ',XZ0231( 39) 
        write(*,*) 'Min.limb height     ',XZ0231( 40) 
        write(*,*) 'Extra Ax.ins W1     ',XZ0231( 41) 
        write(*,*) 'Extra Ax.ins W2     ',XZ0231( 42) 
        write(*,*) 'Extra Ax.ins W3     ',XZ0231( 43) 
        write(*,*) 'Extra Ax.ins W4     ',XZ0231( 44) 
        write(*,*) 'Extra Ax.ins W5     ',XZ0231( 45) 
        write(*,*) 'Extra Ax.ins W6     ',XZ0231( 46) 
        write(*,*) 'Extra Ax.ins W7     ',XZ0231( 47) 
        write(*,*) 'Extra Ax.ins W8     ',XZ0231( 48) 
        write(*,*) 'Extra Ax.ins W9     ',XZ0231( 49) 
        write(*,*) 'Clack height W1     ',XZ0231( 51) 
        write(*,*) 'Clack height W2     ',XZ0231( 52) 
        write(*,*) 'Clack height W3     ',XZ0231( 53) 
        write(*,*) 'Clack height W4     ',XZ0231( 54) 
        write(*,*) 'Clack height W5     ',XZ0231( 55) 
        write(*,*) 'Clack height W6     ',XZ0231( 56) 
        write(*,*) 'Clack height W7     ',XZ0231( 57) 
        write(*,*) 'Clack height W8     ',XZ0231( 58) 
        write(*,*) 'Clack height W9     ',XZ0231( 59) 

        write(*,*) 'FNEXTV =',XZ0080
        write(*,*) 'FEXTV =',XZ0090
        write(*,*) 'U0IN =',XZ0145
        
        write(*,*) 'ACOND =',XZ0651
        write(*,*) 'AWIND =',XZ0661
        write(*,*) 'BDUCT =',XZ0671
        write(*,*) 'BPART =',XZ0681
        write(*,*) 'CUR =',XZ0691
        write(*,*) 'CURPRT =',XZ0692
        write(*,*) 'CURMNT =',XZ0693
        write(*,*) 'CURMXT =',XZ0694
        write(*,*) 'CURSPT =',XZ0695
        write(*,*) 'CURDEN =',XZ0701
        write(*,*) 'CURDM =',XZ0711
        write(*,*) 'IPISOL =',XZ0721
        write(*,*) 'CURRUT =',XZ0731
        write(*,*) 'DWIND =',XZ0741
        write(*,*) 'DYOKE =',XZ0751
        write(*,*) 'EDDCO1 =',XZ0761
        write(*,*) 'EDDCON =',XZ0771
        write(*,*) 'FACT =',XZ0781
        write(*,*) 'FILLF =',XZ0791
        write(*,*) 'FRACT =',XZ0801
        write(*,*) 'FRACTW =',XZ0811
        write(*,*) 'FRMPOS =',XZ0821
        write(*,*) 'FRPPOS =',XZ0831
        write(*,*) 'FRZPOS =',XZ0841
        write(*,*) 'FWIND =',XZ0851
        write(*,*) 'GWINCO =',XZ0861
        write(*,*) 'HPART =',XZ0871
        write(*,*) 'HWIND =',XZ0881
        write(*,*) 'KCODE =',XZ0891
        write(*,*) 'KGROUP =',XZ0901
        write(*,*) 'KWITYP =',XZ0911
        write(*,*) 'NGROUP =',XZ0921
        write(*,*) 'NLOOP =',XZ0931
        write(*,*) 'NOUCO =',XZ0941
        write(*,*) 'PCUW =',XZ0951
        write(*,*) 'PEDW =',XZ0961
        write(*,*) 'PRESSM =',XZ0971
        write(*,*) 'RAACON =',XZ0981
        write(*,*) 'RPART =',XZ0991
        write(*,*) 'RRWDG =',XZ1001
        write(*,*) 'RWIND =',XZ1011
        write(*,*) 'SHTCUR =',XZ1021
        write(*,*) 'SIGM =',XZ1031
        write(*,*) 'STRWMM =',XZ1041
        write(*,*) 'STRWMP =',XZ1051
        write(*,*) 'TCOV1 =',XZ1061
        write(*,*) 'TCOV2 =',XZ1071
        write(*,*) 'TENSM =',XZ1081
        write(*,*) 'TSPIN =',XZ1091
        write(*,*) 'VOLTUT =',XZ1101
        write(*,*) 'WLOSS =',XZ1111
        write(*,*) 'WLOSSM =',XZ1121
        write(*,*) 'X10 =',XZ1131
        write(*,*) 'X20 =',XZ1141
        write(*,*) 'X30 =',XZ1151
        write(*,*) 'ZCODU =',XZ1161
        write(*,*) 'ZTALMW =',XZ1171
        write(*,*) 'ZTALRW =',XZ1181
        write(*,*) 'ZWIND =',XZ1191
        write(*,*) 'ZWINDW =',XZ1201
        write(*,*) 'EXTX =',XZ1211
        write(*,*) 'KCON =',XZ1215
        write(*,*) 'KTYPRW =',XZ1219
        write(*,*) 'NLOOPG =',XZ1223
        write(*,*) 'NMSTEP =',XZ1227
        write(*,*) 'NPSTEP =',XZ1231
        write(*,*) 'PLSPOS =',XZ1235
        write(*,*) 'PUSTEP =',XZ1239
        write(*,*) 'RIDIMW =',XZ1243
        write(*,*) 'RIDIRW =',XZ1247
        write(*,*) 'RMIPOS =',XZ1251
        write(*,*) 'SLINE =',XZ1255
        write(*,*) 'SRATE =',XZ1259
        write(*,*) 'SRATEP =',XZ1263
        write(*,*) 'UDIM =',XZ1267
        write(*,*) 'UN 1 =',((XZ1271(i,j),i=1,1),j=1,4)
        write(*,*) 'UN 2 =',((XZ1271(i,j),i=2,2),j=1,4)
        write(*,*) 'UN 3 =',((XZ1271(i,j),i=3,3),j=1,4)
        write(*,*) 'UN 4 =',((XZ1271(i,j),i=4,4),j=1,4)
        write(*,*) 'UNTAP =',XZ1272
        write(*,*) 'UNLINE =',XZ1283
        write(*,*) 'USURG =',XZ1287
        write(*,*) 'XRINT =',XZ1291
        write(*,*) 'ZERPOS =',XZ1295
        write(*,*) 'BB03 =',XZ1299
        write(*,*) 'BB051 =',XZ1300
        write(*,*) 'BBCURD =',XZ1301
        write(*,*) 'BBHELP =',XZ1311
        write(*,*) 'BBRR =',XZ1331
        write(*,*) 'BBRW =',XZ1341
        write(*,*) 'BBSCI =',XZ1351
        write(*,*) 'BBSCJ =',XZ1354
        write(*,*) 'BBSCK =',XZ1357
        write(*,*) 'BBSCL =',XZ1360
        write(*,*) 'BBSCR =',XZ1363
        write(*,*) 'BBVR =',XZ1366
        write(*,*) 'BB08 =',XZ1370
        write(*,*) 'BB132 =',XZ1371
        write(*,*) 'BBAUTO =',XZ1373
        write(*,*) 'BBDLI =',XZ1375
        write(*,*) 'BBDSHE =',XZ1376
        write(*,*) 'BBEND =',XZ1377
        write(*,*) 'BBERR =',XZ1378
        write(*,*) 'BBERR1 =',XZ1379
        write(*,*) 'BBEXAC =',XZ1380
        write(*,*) 'BBFLDS =',XZ1381
        write(*,*) 'BBFLU =',XZ1382
        write(*,*) 'BBHLI =',XZ1384
        write(*,*) 'BBLT10 =',XZ1385
        write(*,*) 'BBOPTI =',XZ1386
        write(*,*) 'BBOUT =',XZ1387
        write(*,*) 'BBREA =',XZ1389
        write(*,*) 'BBSLIM =',XZ1390
        write(*,*) 'BBTS =',XZ1391
        write(*,*) 'BBUPTR =',XZ1392
        write(*,*) 'BBVFR =',XZ1393
        write(*,*) 'BBWISU =',XZ1395
        write(*,*) 'ACORE =',XZ1396
        write(*,*) 'ACOVER =',XZ1397
        write(*,*) 'AFIELD =',XZ1398
        write(*,*) 'AVALUE =',XZ1399
        write(*,*) 'ANDEL =',XZ1400
        write(*,*) 'APOED =',XZ1401
        write(*,*) 'ASECL =',XZ1402
        write(*,*) 'ASHELL =',XZ1403
        write(*,*) 'BDRAG =',XZ1404
        write(*,*) 'BLIMBX =',XZ1405
        write(*,*) 'BLTOPX =',XZ1406
        write(*,*) 'BMAXPU =',XZ1407
        write(*,*) 'BMINPU =',XZ1408
        write(*,*) 'BPOED =',XZ1409
        write(*,*) 'BTANK =',XZ1410
        write(*,*) 'BTANKM =',XZ1411
        write(*,*) 'CCOOL =',XZ1412
        write(*,*) 'CLOSSV =',XZ1414
        write(*,*) 'COST =',XZ1415
        write(*,*) 'COSTTP =',XZ1416
        write(*,*) 'CPUFIX =',XZ1417
        write(*,*) 'CPUIND =',XZ1419
        write(*,*) 'CPUPT =',XZ1420
        write(*,*) 'CTROVN =',XZ1422
        write(*,*) 'DCOCOV =',XZ1423
        write(*,*) 'DCOMAX =',XZ1424
        write(*,*) 'DCOMIN =',XZ1425
        write(*,*) 'DCORE =',XZ1426
        write(*,*) 'DKSL =',XZ1427
        write(*,*) 'DOUTW =',XZ1428
        write(*,*) 'DPHAS =',XZ1429
        write(*,*) 'DPHSL =',XZ1430
        write(*,*) 'DRV =',XZ1431
        write(*,*) 'DTSFF =',XZ1432
        write(*,*) 'DTSNF =',XZ1433
        write(*,*) 'DTSNN =',XZ1434
        write(*,*) 'DWITA =',XZ1435
        write(*,*) 'PKLPRM =',XZ1436
        write(*,*) 'EPS =',XZ1437
        write(*,*) 'EXTANK =',XZ1438
        write(*,*) 'F1TANK =',XZ1439
        write(*,*) 'F2TANK =',XZ1440
        write(*,*) 'F3TANK =',XZ1441
        write(*,*) 'FEPS =',XZ1442
        write(*,*) 'FLUXM =',XZ1443
        write(*,*) 'AF =',XZ1444
        write(*,*) 'FONAN =',XZ1445
        write(*,*) 'FREQ =',XZ1446
        write(*,*) 'GACTP =',XZ1452
        write(*,*) 'GCONS =',XZ1453
        write(*,*) 'GCOOL =',XZ1454
        write(*,*) 'GCORLA =',XZ1455
        write(*,*) 'GCORN =',XZ1456
        write(*,*) 'GCOVER =',XZ1457
        write(*,*) 'GFAN =',XZ1458
        write(*,*) 'GLIMBM =',XZ1459
        write(*,*) 'GLIMBY =',XZ1460
        write(*,*) 'GOFWF =',XZ1461
        write(*,*) 'GRAD =',XZ1464
        write(*,*) 'GTANK =',XZ1465
        write(*,*) 'GTRP =',XZ1466
        write(*,*) 'GTRPM =',XZ1467
        write(*,*) 'GVVAH =',XZ1468
        write(*,*) 'GYOKE =',XZ1470
        write(*,*) 'HCORE =',XZ1471
        write(*,*) 'HLIMB =',XZ1472
        write(*,*) 'HLMBMA =',XZ1473
        write(*,*) 'HTANK =',XZ1474
        write(*,*) 'HTANKM =',XZ1475
        write(*,*) 'HWINDM =',XZ1476
        write(*,*) 'HYOKE =',XZ1477
        write(*,*) 'HLMBMI =',XZ1478
        write(*,*) 'ISAM =',XZ1479
        write(*,*) 'ILACK =',XZ1480
        write(*,*) 'NCONDU =',XZ1481
        write(*,*) 'IPAGE =',XZ1482
        write(*,*) 'MNLY =',XZ1483
        write(*,*) 'ITR =',XZ1484
        write(*,*) 'IVERS =',XZ1485
        write(*,*) 'JFC =',XZ1486
        write(*,*) 'KCOOL =',XZ1488
        write(*,*) 'KCOOL1 =',XZ1489
        write(*,*) 'KCOOL2 =',XZ1490
        write(*,*) 'KCORE =',XZ1491
        write(*,*) 'KTAN79 =',XZ1492
        write(*,*) 'KTANK =',XZ1493
        write(*,*) 'NCLA =',XZ1494
        write(*,*) 'NCONST =',XZ1495
        write(*,*) 'NCOOLC =',XZ1496
        write(*,*) 'NFMX =',XZ1497
        write(*,*) 'NFREE =',XZ1498
        write(*,*) 'NG =',XZ1499
        write(*,*) 'NPG =',XZ1500
        write(*,*) 'NPHAS =',XZ1501
        write(*,*) 'NSG =',XZ1504
        write(*,*) 'NUMN =',XZ1506
        write(*,*) 'NUYR =',XZ1507
        write(*,*) 'NWILI =',XZ1508
        write(*,*) 'NWOULI =',XZ1509
        write(*,*) 'P0LOSS =',XZ1510
        write(*,*) 'PCU =',XZ1511
        write(*,*) 'PED =',XZ1512
        write(*,*) 'PI =',XZ1513
        write(*,*) 'PKLOSS =',XZ1514
        write(*,*) 'PLIMB =',XZ1515
        write(*,*) 'PLOADM =',XZ1516
        write(*,*) 'PLOMAX =',XZ1517
        write(*,*) 'PLSL =',XZ1518
        write(*,*) 'IOPT =',XZ1519
        write(*,*) 'PNOLOM =',XZ1520
        write(*,*) 'POED =',XZ1521
        write(*,*) 'QFINAL =',XZ1523
        write(*,*) 'RAAAL =',XZ1524
        write(*,*) 'RAACU =',XZ1525
        write(*,*) 'RDRAG =',XZ1526
        write(*,*) 'ISTGRD =',XZ1527
        write(*,*) 'RLTANK =',XZ1528
        write(*,*) 'RLTNKM =',XZ1529
        write(*,*) 'RLTRAN =',XZ1530
        write(*,*) 'RLYOKE =',XZ1531
        write(*,*) 'RMY0 =',XZ1532
        write(*,*) 'DN =',XZ1533
        write(*,*) 'SBF =',XZ1537
        write(*,*) 'SCCONS =',XZ1538
        write(*,*) 'SEQU2W =',XZ1539
        write(*,*) 'SIGMAL =',XZ1540
        write(*,*) 'SIGMCU =',XZ1541
        write(*,*) 'SNOML =',XZ1542
        write(*,*) 'SOUNDM =',XZ1543
        write(*,*) 'SQR2 =',XZ1544
        write(*,*) 'SQR3 =',XZ1545
        write(*,*) 'TANKDI =',XZ1547
        write(*,*) 'TASEC =',XZ1548
        write(*,*) 'TCORE =',XZ1550
        write(*,*) 'TDRAG =',XZ1551
        write(*,*) 'TTOILC =',XZ1553
        write(*,*) 'TTOILM =',XZ1554
        write(*,*) 'TURNRA =',XZ1555
        write(*,*) 'TWINDM =',XZ1556
        write(*,*) 'TWSUP =',XZ1557
        write(*,*) 'TYPCOR =',XZ1558
        write(*,*) 'U0 =',XZ1559
        write(*,*) 'UMAXPU =',XZ1560
        write(*,*) 'USHORE =',XZ1561
        write(*,*) 'VACTP =',XZ1562
        write(*,*) 'VTANK =',XZ1563
        write(*,*) 'YHADD =',XZ1565
        write(*,*) 'YHRED =',XZ1566
        write(*,*) 'YOKAMP =',XZ1567
        write(*,*) 'ZTRANS =',XZ1570
        write(*,*) 'ZWOULI =',XZ1571
        write(*,*) 'CHCORE =',XZ1572
        write(*,*) 'PCUT =',XZ1574
        write(*,*) 'PEDT =',XZ1575
        write(*,*) 'POEDT =',XZ1576
        write(*,*) 'PCEOT =',XZ1577
        write(*,*) 'CHCOST =',XZ1587
        write(*,*) 'DATE =',XZ1589
        write(*,*) 'FILECH =',XZ1596
        write(*,*) 'IDENT =',XZ1604
        write(*,*) 'MNL =',XZ1624
        write(*,*) 'OBJ =',XZ1626
        write(*,*) 'RUBCH =',XZ1632
        write(*,*) 'NPERM =',XZ1647
        write(*,*) 'P00 =',XZ1650
        write(*,*) 'PNOLO =',XZ1651
        write(*,*) 'Q00 =',XZ1653
        write(*,*) 'RINOLO =',XZ1654
        write(*,*) 'POSIT 1=',((XZ1656(i,j),i=1,1),j=1,4)
        write(*,*) 'POSIT 2=',((XZ1656(i,j),i=2,2),j=1,4)
        write(*,*) 'POSIT 3=',((XZ1656(i,j),i=3,3),j=1,4)
        write(*,*) 'POSIT 4=',((XZ1656(i,j),i=4,4),j=1,4)
        write(*,*) 'POSIT 5=',((XZ1656(i,j),i=5,5),j=1,4)
        write(*,*) 'POSIT 6=',((XZ1656(i,j),i=6,6),j=1,4)
        write(*,*) 'POSIT 7=',((XZ1656(i,j),i=7,7),j=1,4)
        write(*,*) 'POSIT 8=',((XZ1656(i,j),i=8,8),j=1,4)
        write(*,*) 'POSIT 9=',((XZ1656(i,j),i=9,9),j=1,4)
        write(*,*) 'SOUND0 =',XZ1686
        write(*,*) 'SOUND =',XZ1687
        write(*,*) 'URC =',XZ1689
        write(*,*) 'UXC =',XZ1690
        write(*,*) 'XA =',XZ1743
        write(*,*) 'XREL =',XZ1755
        write(*,*) 'ZTALWG =',XZ1767
        write(*,*) 'SAVE1 =',XZ2145
        write(*,*) 'WLOSEX =',XZ2245
        write(*,*) 'WLOSRX =',XZ2254
        write(*,*) 'KODL =',XZ2263
        write(*,*) 'KODP =',XZ2264
        write(*,*) 'CSTPSW =',XZ2266
        write(*,*) 'FLUXMD =',XZ2271
        write(*,*) 'FLUXMW =',XZ2281
        write(*,*) 'CORBND =',XZ2295
        write(*,*) 'PG =',XZ2296
        write(*,*) 'PK =',XZ2297
        write(*,*) 'NCOL =',XZ2298
        write(*,*) 'GREL =',XZ2307
        write(*,*) 'G =',XZ2346
        write(*,*) 'FLUXI =',XZ2401
        write(*,*) 'FLUXO =',XZ2411
        write(*,*) 'BBOOVN =',XZ2500
        write(*,*) 'VALUEM =',XZ2501
        write(*,*) 'VALUEO =',XZ2502
        write(*,*) 'VALUEF =',XZ2503
        write(*,*) 'DVERS =',XZ2551
        write(*,*) 'NLORR =',XZ2554
        write(*,*) 'KWIND =',XZ2555
        write(*,*) 'BBFREQ =',XZ2562
        write(*,*) 'BBADJU =',XZ2563
        write(*,*) 'BBEXON =',XZ2566
        write(*,*) 'BBLAY =',XZ2567
        write(*,*) 'BBISGR =',XZ2568
        write(*,*) 'ZLAG =',XZ2581
        write(*,*) 'ZNLAG =',XZ2582
        write(*,*) 'ZCOIAR =',XZ2583
        write(*,*) 'EXTRAR =',XZ2584
        write(*,*) 'HCLAC =',XZ2585
        write(*,*) 'ZDISC =',XZ2586
        write(*,*) 'SWIND =',XZ2589
        write(*,*) 'RINNER =',XZ2590
        write(*,*) 'ZAR =',XZ2591
        write(*,*) 'ZRR =',XZ2592
        write(*,*) 'ZPART =',XZ2593
        write(*,*) 'ZTUDI =',XZ2594
        write(*,*) 'ZTULO =',XZ2595
        write(*,*) 'TSPIN1 =',XZ2596
        write(*,*) 'HPRTMN =',XZ2597
        write(*,*) 'HPRTMX =',XZ2598
        write(*,*) 'BPRTMN =',XZ2599
        write(*,*) 'BPRTMX =',XZ2600
        write(*,*) 'EXTCOR =',XZ2701
        write(*,*) 'PSPL1 =',XZ2702
        write(*,*) 'PSPL2 =',XZ2703
        write(*,*) 'PSPL3 =',XZ2704
        write(*,*) 'PSPHD1 =',XZ2705
        write(*,*) 'PSPHD2 =',XZ2706
        write(*,*) 'PSPHD3 =',XZ2707
        write(*,*) 'PSPHT1 =',XZ2708
        write(*,*) 'PSPHT2 =',XZ2709
        write(*,*) 'PSPHT3 =',XZ2710
        write(*,*) 'PSPTY1 =',XZ2711
        write(*,*) 'PSPTY2 =',XZ2712
        write(*,*) 'PSPTY3 =',XZ2713
        write(*,*) 'SSPL1 =',XZ2714
        write(*,*) 'SSPL2 =',XZ2715
        write(*,*) 'SSPL3 =',XZ2716
        write(*,*) 'SSPHD1 =',XZ2717
        write(*,*) 'SSPHD2 =',XZ2718
        write(*,*) 'SSPHD3 =',XZ2719
        write(*,*) 'SSPHT1 =',XZ2720
        write(*,*) 'SSPHT2 =',XZ2721
        write(*,*) 'SSPHT3 =',XZ2722
        write(*,*) 'SSPTY1 =',XZ2723
        write(*,*) 'SSPTY2 =',XZ2724
        write(*,*) 'SSPTY3 =',XZ2725
        write(*,*) 'LAMID =',XZ2726
        write(*,*) 'MABOOS =',XZ2727
        write(*,*) 'VOBOOS =',XZ2728
        write(*,*) 'REBOOS =',XZ2729
        write(*,*) 'TRBOOS =',XZ2730
        write(*,*) 'FABOOS =',XZ2731
        write(*,*) 'BBOOS =',XZ2732
        write(*,*) 'BBOOSW =',XZ2733
        write(*,*) 'TELOSS =',XZ2734
        write(*,*) 'STKFAC =',XZ2735
        write(*,*) 'GPBLK =',XZ2736
        write(*,*) 'GOIL =',XZ2737
        write(*,*) 'CTRCMP =',XZ2738
        write(*,*) 'CUCOS =',XZ2739
        write(*,*) 'CUCOSP =',XZ2740
        write(*,*) 'FECOS =',XZ2741
        write(*,*) 'FECOSP =',XZ2742
        write(*,*) 'OLCOS =',XZ2743
        write(*,*) 'GCONDU =',XZ2744
        write(*,*) 'OLCOSP =',XZ2745
        write(*,*) 'PNFDUC =',XZ2746
        write(*,*) 'PNFWIN =',XZ2747
        write(*,*) 'PNFYOK =',XZ2748
        write(*,*) 'PNDUCT =',XZ2749
        write(*,*) 'PNWIND =',XZ2750
        write(*,*) 'PNYOKE =',XZ2751
        write(*,*) 'LAMTH =',XZ2752
        write(*,*) 'LAMSPF =',XZ2753
        write(*,*) 'LAMMW =',XZ2754
        write(*,*) 'LAMOVL =',XZ2755
        write(*,*) 'ACCLON =',XZ2756
        write(*,*) 'ACCTRA =',XZ2757
        write(*,*) 'ACCVER =',XZ2758
        write(*,*) 'AMBTEM =',XZ2759
        write(*,*) 'LAMSTP =',XZ2760
        write(*,*) 'LOSCOR =',XZ2761
        write(*,*) 'TA1HOL =',XZ2762
        write(*,*) 'SPRED =',XZ2763
        write(*,*) 'FLPLMA =',XZ2770
        write(*,*) 'NCOOLI =',XZ2780
        write(*,*) 'NCOOLW =',XZ2781
        write(*,*) 'ISTOP =',XZ2782
        write(*,*) 'SPFADJ =',XZ2799
        write(*,*) 'EXTPOS =',XZ3001
        write(*,*) 'FRXPOS =',XZ3004
        write(*,*) 'NXSTEP =',XZ3010
      
        write(*,*) 'FARR 1 - 9=',(XZ3289(i), i=1,9)
        write(*,*) 'FARR 10-18=',(XZ3289(i), i=10,18)
        write(*,*) 'FARR 19-27=',(XZ3289(i), i=19,27)
        write(*,*) 'FARR 28-36=',(XZ3289(i), i=28,36)
        write(*,*) 'FARR 37-45=',(XZ3289(i), i=37,45)
        write(*,*) 'FARR 46-54=',(XZ3289(i), i=46,54)
        write(*,*) 'FARR 55-63=',(XZ3289(i), i=55,63)
        write(*,*) 'FARR 64-72=',(XZ3289(i), i=64,72)
        write(*,*) 'FARR 73-81=',(XZ3289(i), i=73,81)
        write(*,*) 'FARR 82-90=',(XZ3289(i), i=82,90)
        write(*,*) 'FARR 91-100=',(XZ3289(i), i=91,100)
        write(*,*) 'WDGFUN =',XZ3390

        write(*,*) '----------------output----------'
        write(*,*) 'LIMB Pitch',XZ1515*1000
        write(*,*) 'LIMB Height ',XZ1472*1000
        write(*,*) 'CORE DIAMETER ',XZ1426*1000
        write(*,*) 'Limb Flux density ',XZ1405
        write(*,*) 'Reactance ',XZ1561*100
        write(*,*) 'SPACEf',(XZ0791(i), i=1,XZ1508)
        write(*,*) 'Winding Width',(XZ1001(i)*1000,i=1,XZ1508)
        write(*,*) 'Duct Width',(XZ0671(i)*1000,i=1,XZ1508)
        write(*,*) 'Conductor Area',(XZ0651(i)*1000000,i=1,XZ1508)
        write(*,*) 'Tank Width',XZ1410*1000
        write(*,*) 'Tank Length',XZ1528*1000
        write(*,*) 'Tank Height',XZ1474*1000
        CALL MOVEDB
        RETURN
        END
