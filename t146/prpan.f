       subroutine prpan(ipage, filno, dummy, pnamei,iright)

C*************************************************
C 
C  Write a panle with current dialog variables to
C  the result file.
C
C  ipage      1 - new page
C  filno      file code for output file
C  dummy      not used
C  pnamei     name of panel
C  iright     shift right or output (no of positions)
C
C  Written 91-09-12   J Johansson SETFO/IK
C  Revised 92-02-11   B-G Bladh   SETFO/TS
C****************************************************

       integer ipage, filno, iright, maxrow
       character*8 dummy, pnamei, fmt
       character*16 env, help
       character*81 pan(32)

       character pname*8, pnl*100, pcont*100, lnin*100

       character label*1, text*80
       character dvari*8, dvaro*8, cin*81
       integer   row, col, width

       integer getdia, getpth, getprf

#ifdef SUNOS
       external getdia !$pragma C ( getdia )
       external getpth !$pragma C ( getpth )
       external getprf !$pragma C ( getprf )
#endif

C...Clear panel area

       do 99 i=1, 32
  99      pan(i) = " "

C... Add Control character

       if(ipage .eq. 1) then
          pan(1)(1:1) = ''
       else
          pan(1)(1:1) = ' '
       endif

       pname = pnamei

C... convert panel name from Capitals to small letters

       idist = ichar('A') - ichar('a')
       do 199 i =1, 8
          if(ichar(pname(i:i)) .ge. ichar('A') .and.
     &       ichar(pname(i:i)) .le. ichar('Z')) then  
                pname(i:i) = char(ichar(pname(i:i)) - idist)
          endif
 199   continue

C... Clean name from trailing blanks and search for the panel

       k=index(pname(1:8),' ')
       if(k.eq.0) k=index(pname(1:8),char(0))
       if(k.eq.0) k=9
       karg2 = k

       help = pname(1:karg2 - 1)//'       '
       pname = help

C... Where to search (in shell variable BAT_PNL_LIB)

       iret = getprf("BAT_PNL_LIB", 11, pcont, lcont)

       env = "BAT_PNL_LIB"//char(0)

C... Search for panelname.pb

       pnl = pname(1:karg2 - 1)//".pb"//char(0)
       iret = getpth(env, pnl, pcont, lcont)

C... lcont > 0 there is a Directory path to the panel in pcont

       if(lcont .gt. 0) then
          pnl=pcont(1:lcont)//'/'//pname(1:karg2 - 1)//".pb"

          open(10, file=pnl, err=999)

 200      read(10, 1002, end=299, err=299) lnin
CCC       write(*,'(a100)') lnin

C... 'l' body text to be shown on row, column 
C... put into panle area.

             if(lnin(1:1) .eq. 'l') then
                read(lnin, 1000, end=299, err=299) label, row, col,
     &                                             width, text
                pan(row)(col + 1:col + width) = text(1:width)

C... 'i', 'o' input or output fields.
C... get name in dialog variable from text, then get value and put into
C... panel area.

             elseif(lnin(1:1) .eq. 'i' .or. lnin(1:1) .eq. 'o') then

                read(lnin, 1000, end=299, err=299) label, row, col,
     &                                             width, text
                dvari = text(1:8)
                k=index(dvari,' ')
                if(k.eq.0) k=8
                dvaro=dvari(1:k)//'        '
                iret = getdia(cin,dvaro,width)
                pan(row)(col + 1:col + width) = cin(1:width)

             endif
             goto 200
 299      continue
C
          maxrow=33
 250      maxrow = maxrow - 1
          if(maxrow.gt.5 .and. pan(maxrow).eq." ") go to 250         
C
          write(fmt,1001) iright + 1
C
          do 399 i = 1,maxrow 
             write(filno,fmt) pan(i)
 399      continue
          write(filno,*) "  "

 999      continue
       endif

       close(10)

       return

 1000  format(a1, i4, i3, i3, tr5, a)
 1001  format("(",i2,"x,a)")
 1002  format(a100)
       end
