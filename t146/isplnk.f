       subroutine isplnk(arg1,arg2,arg3)

       character arg1*8, arg2*8, arg3*8
       character*80 pan(32)
       character infld*500,otfld*500,mess*80
       character*8 dnami(100),dnamo(100)
       character*8 oldnam, help
       character*8 varnmi(200)
       character*500 valusi

       save oldnam
       save pan,nn,rad,kol,brd,orad,okol,obrd,rfr,afr,
     &      nnamei,dnami,nnameo,dnamo,
     &      ncomi,comtpi,varnmi,valusi,nvali,vallni,
     &      valsti

       integer nn(10),rad(300),kol(300),brd(300),frg(300),
     &         orad(300),okol(300),obrd(300),ofrg(300),
     &         rfr(500),kfr(500),afr(500),nnamei,nnameo,
     &         ncomi,comtpi(200),nvali(200),vallni(1000),
     &         valsti(2,200)
      
       character pname*20

c      write(*,*) '***write isplnk entry'
c      write(*,*) '***write isplnk arg1, arg2 ', arg1(1:8), arg2(1:8)

       if(arg1(1:7).eq.'control') then
          if(arg2(1:7).eq.'display') then
             if(arg3(1:7).eq.'refresh') then
                iret = clrscr
                oldnam=' '
             else
                write(*,*) 
     &           '***error isplnk: unknown argument 3, ',arg3(1:7)
             endif
          else
             write(*,*) 
     &        '***error isplnk: unknown argument 2, ',arg2(1:7)
          endif
       elseif(arg1(1:7).eq.'display') then

c      write(*,*) '***isplnk display'
c      write(*,*) '***isplnk calling index'
       k=index(arg2(1:8),' ')
       if(k.eq.0) k=index(arg2(1:8),char(0))
       if(k.eq.0) k=9
       karg2 = k
c      write(*,*) '***isplnk building pname'
       pname='./Panels/'//arg2(1:karg2 - 1)//'.p'
       help = arg2(1:karg2 - 1)//'       '
       arg2 = help
c      write(*,*) '***isplnk file ', pname

       nn(4)=0
       if(arg2(1:8).ne.oldnam) then
c      write(*,*) '***isplnk open'
          open(unit=10,form='unformatted',file=pname)
          read(10) pan
          read(10) nn
          read(10) rad
          read(10) kol
          read(10) brd
          read(10) frg
          read(10) orad
          read(10) okol
          read(10) obrd
          read(10) ofrg
          read(10) rfr
          read(10) kfr
          read(10) afr
          read(10) nnamei,dnami
          read(10) nnameo,dnamo
          read(10) ncomi,comtpi
          read(10) varnmi
          read(10) valusi
          read(10) nvali,vallni,valsti
          close(unit=10)
          nn(4)=1
       endif
c      write(*,*) '***isplnk nn(4) oldnam, arg2(1:8) ',
c    &            nn(4), oldnam, arg2(1:8)
c      read(*,*)idum
       if(ncomi.gt.0) 
     &    call rninpr(ncomi,comtpi,varnmi,nnamei,dnami,nn,
     &                valusi,nvali,vallni,valsti)

       k=scall('stty eof "^a" eol "^a" -icanon -echo'//char(0),icontr)

c      write(*,*) '***isplnk movdia dnami1 ', dnami(1)
       call diatos(nnamei,dnami,brd,infld)
       call diatos(nnameo,dnamo,obrd,otfld)

c      write(*,*) '***isplnk panman'
       k=panman(pan,nn,rad,kol,brd,frg,orad,okol,obrd,ofrg,
     &          rfr,kfr,afr,
     &          infld,otfld,mess)

       k=scall('stty icanon echo eof "^d"'//char(0),icontr)

       
       call diafrs(nnamei,dnami,brd,infld)

       oldnam=arg2(1:karg2 - 1)//'       '
    
       else
          
       endif

       write(*,*) '                                      '

       return
       end
