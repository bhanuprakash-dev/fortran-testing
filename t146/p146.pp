PROCSTART 
MSGPARAM  OPTRM111
PARAM     X
PARAM     4
PARAM     3
PARAM     2
PARAM     1
KEYPARAM  LIST
NBPARAM   NO
PARAM     &Q
CALL      CVR       9
PARAM     '2'
PARAM     &Q
CALL      CIFEQ     2         632
PARAM     ?
PARAM     &D91
CALL      CIFEQ     2         2
ASSGN     .MSG      OPTRM050
PARAM     ?
PARAM     &D92
CALL      CIFEQ     2         2
ASSGN     .MSG      OPTRM051
PARAM     ?
PARAM     &D93
CALL      CIFEQ     2         2
ASSGN     .MSG      OPTRM052
PARAM     ?
PARAM     &D94
CALL      CIFEQ     2         2
ASSGN     .MSG      OPTRM053
PARAM     ?
PARAM     &X21
CALL      CIFEQ     2         2
ASSGN     .MSG      OPTRM120
PARAM     ?
PARAM     &X22
CALL      CIFEQ     2         2
ASSGN     .MSG      OPTRM121
PARAM     ?
PARAM     &X23
CALL      CIFEQ     2         2
ASSGN     .MSG      OPTRM122
PARAM     ?
PARAM     &X24
CALL      CIFEQ     2         2
ASSGN     .MSG      OPTRM123
PARAM     ?
PARAM     &B35
CALL      CIFEQ     2         2
ASSGN     .MSG      OPTRM030
PARAM     ?
PARAM     &B14
CALL      CIFEQ     2         2
ASSGN     .MSG      OPTRM014
PARAM     ?
PARAM     &B15
CALL      CIFEQ     2         2
ASSGN     .MSG      OPTRM015
PARAM     ?
PARAM     &D11
CALL      CIFEQ     2         2
ASSGN     .MSG      OPTRM058
PARAM     ?
PARAM     &D21
CALL      CIFEQ     2         2
ASSGN     .MSG      OPTRM058
PARAM     ?
PARAM     &D31
CALL      CIFEQ     2         2
ASSGN     .MSG      OPTRM058
PARAM     ?
PARAM     &D41
CALL      CIFEQ     2         2
ASSGN     .MSG      OPTRM058
PARAM     ?
PARAM     &D51
CALL      CIFEQ     2         2
ASSGN     .MSG      OPTRM058
PARAM     ?
PARAM     &D61
CALL      CIFEQ     2         2
ASSGN     .MSG      OPTRM058
PARAM     ?
PARAM     &Y12
CALL      CIFEQ     2         2
ASSGN     .MSG      OPTRM059
PARAM     ?
PARAM     &Y22
CALL      CIFEQ     2         2
ASSGN     .MSG      OPTRM059
PARAM     ?
PARAM     &Y32
CALL      CIFEQ     2         2
ASSGN     .MSG      OPTRM059
PARAM     ?
PARAM     &Y42
CALL      CIFEQ     2         2
ASSGN     .MSG      OPTRM059
PARAM     ?
PARAM     &Y52
CALL      CIFEQ     2         2
ASSGN     .MSG      OPTRM059
PARAM     ?
PARAM     &Y62
CALL      CIFEQ     2         2
ASSGN     .MSG      OPTRM059
PARAM     ?
PARAM     &Y13
CALL      CIFEQ     2         2
ASSGN     .MSG      OPTRM060
PARAM     ?
PARAM     &Y23
CALL      CIFEQ     2         2
ASSGN     .MSG      OPTRM060
PARAM     ?
PARAM     &Y33
CALL      CIFEQ     2         2
ASSGN     .MSG      OPTRM060
PARAM     ?
PARAM     &Y43
CALL      CIFEQ     2         2
ASSGN     .MSG      OPTRM060
PARAM     ?
PARAM     &Y53
CALL      CIFEQ     2         2
ASSGN     .MSG      OPTRM060
PARAM     ?
PARAM     &Y63
CALL      CIFEQ     2         2
ASSGN     .MSG      OPTRM060
PARAM     ?
PARAM     &Y14
CALL      CIFEQ     2         2
ASSGN     .MSG      OPTRM045
PARAM     ?
PARAM     &Y24
CALL      CIFEQ     2         2
ASSGN     .MSG      OPTRM045
PARAM     ?
PARAM     &Y34
CALL      CIFEQ     2         2
ASSGN     .MSG      OPTRM045
PARAM     ?
PARAM     &Y44
CALL      CIFEQ     2         2
ASSGN     .MSG      OPTRM045
PARAM     ?
PARAM     &Y54
CALL      CIFEQ     2         2
ASSGN     .MSG      OPTRM045
PARAM     ?
PARAM     &Y64
CALL      CIFEQ     2         2
ASSGN     .MSG      OPTRM045
PARAM     ?
PARAM     &Y15
CALL      CIFEQ     2         2
ASSGN     .MSG      OPTRM061
PARAM     ?
PARAM     &Y25
CALL      CIFEQ     2         2
ASSGN     .MSG      OPTRM061
PARAM     ?
PARAM     &Y35
CALL      CIFEQ     2         2
ASSGN     .MSG      OPTRM061
PARAM     ?
PARAM     &Y45
CALL      CIFEQ     2         2
ASSGN     .MSG      OPTRM061
PARAM     ?
PARAM     &Y55
CALL      CIFEQ     2         2
ASSGN     .MSG      OPTRM061
PARAM     ?
PARAM     &Y65
CALL      CIFEQ     2         2
ASSGN     .MSG      OPTRM061
PARAM     1
PARAM     &D91
CALL      CTRNC     2         &ZZ0
ASSGN     &D91      &ZZ0
MSGPARAM  OPTRM050
PARAM     NO
PARAM     N
PARAM     YES
PARAM     Y
PARAM     ?
PARAM     ?
PARAM     &D91
CALL      CTRNS     8         &ZZ1
ASSGN     &D91      &ZZ1
PARAM     1
PARAM     &D92
CALL      CTRNC     2         &ZZ2
ASSGN     &D92      &ZZ2
MSGPARAM  OPTRM051
PARAM     NO
PARAM     N
PARAM     YES
PARAM     Y
PARAM     ?
PARAM     ?
PARAM     &D92
CALL      CTRNS     8         &ZZ3
ASSGN     &D92      &ZZ3
PARAM     1
PARAM     &D93
CALL      CTRNC     2         &ZZ4
ASSGN     &D93      &ZZ4
MSGPARAM  OPTRM052
PARAM     NO
PARAM     N
PARAM     YES
PARAM     Y
PARAM     ?
PARAM     ?
PARAM     &D93
CALL      CTRNS     8         &ZZ5
ASSGN     &D93      &ZZ5
PARAM     1
PARAM     &D94
CALL      CTRNC     2         &ZZ6
ASSGN     &D94      &ZZ6
MSGPARAM  OPTRM053
PARAM     NO
PARAM     N
PARAM     YES
PARAM     Y
PARAM     ?
PARAM     ?
PARAM     &D94
CALL      CTRNS     8         &ZZ7
ASSGN     &D94      &ZZ7
PARAM     NO
PARAM     &D91
CALL      CIFEQ     2         5
MSGPARAM  OPTRM120
NBPARAM   NO
PARAM     &X21
CALL      CVR       3
PARAM     NO
PARAM     &D92
CALL      CIFEQ     2         5
MSGPARAM  OPTRM121
NBPARAM   NO
PARAM     &X22
CALL      CVR       3
PARAM     NO
PARAM     &D93
CALL      CIFEQ     2         5
MSGPARAM  OPTRM122
NBPARAM   NO
PARAM     &X23
CALL      CVR       3
PARAM     NO
PARAM     &D91
CALL      CIFEQ     2         8
PARAM     NO
PARAM     &D92
CALL      CIFEQ     2         5
PARAM     NO
PARAM     &D93
CALL      CIFEQ     2         2
ASSGN     &D94      YES
PARAM     NO
PARAM     &D94
CALL      CIFEQ     2         5
MSGPARAM  OPTRM123
NBPARAM   NO
PARAM     &X24
CALL      CVR       3
PARAM     ' '
PARAM     &B14
CALL      CIFEQ     2         2
ASSGN     &B14      0
MSGPARAM  OPTRM014
NBPARAM   NO
PARAM     &B14
CALL      CVR       3
PARAM     ' '
PARAM     &B15
CALL      CIFEQ     2         2
ASSGN     &B15      0
MSGPARAM  OPTRM015
NBPARAM   NO
PARAM     &B15
CALL      CVR       3
PARAM     ' '
PARAM     &B35
CALL      CIFEQ     2         2
ASSGN     &B35      NO
PARAM     1
PARAM     &B35
CALL      CTRNC     2         &ZZ8
ASSGN     &B35      &ZZ8
MSGPARAM  OPTRM030
PARAM     NO
PARAM     N
PARAM     YES
PARAM     Y
PARAM     ?
PARAM     ?
PARAM     &B35
CALL      CTRNS     8         &ZZ9
ASSGN     &B35      &ZZ9
PARAM     6
PARAM     5
PARAM     4
PARAM     3
PARAM     2
PARAM     1
PARAM     &WILI
CALL      CIFEQ     7         52
MSGPARAM  OPTRM045
NBPARAM   NO
PARAM     &Y14
CALL      CVR       3
PARAM     1
PARAM     &D11
CALL      CTRNC     2         &ZZ10
ASSGN     &D11      &ZZ10
MSGPARAM  OPTRM058
PARAM     NO
PARAM     N
PARAM     YES
PARAM     Y
PARAM     ?
PARAM     ?
PARAM     &D11
CALL      CTRNS     8         &ZZ11
ASSGN     &D11      &ZZ11
PARAM     'YES'
PARAM     &D11
CALL      CIFEQ     2         6
MSGPARAM  OPTRM059
NBPARAM   NO
PARAM     &Y12
CALL      CVR       3
ASSGN     &Y13      &BLANK
PARAM     'NO'
PARAM     &D11
CALL      CIFEQ     2         23
PARAM     &BLANK
PARAM     &Y12
CALL      CIFEQ     2         5
MSGPARAM  OPTRM060
NBPARAM   NO
PARAM     &Y13
CALL      CVR       3
PARAM     &BLANK
PARAM     &Y13
CALL      CIFEQ     2         5
MSGPARAM  OPTRM059
NBPARAM   NO
PARAM     &Y12
CALL      CVR       3
PARAM     &BLANK
PARAM     &Y13
CALL      CIFNEQ    2         2
ASSGN     &Y12      &BLANK
PARAM     &BLANK
PARAM     &Y12
CALL      CIFNEQ    2         2
ASSGN     &Y13      &BLANK
PARAM     6
PARAM     5
PARAM     4
PARAM     3
PARAM     2
PARAM     &WILI
CALL      CIFEQ     6         52
MSGPARAM  OPTRM045
NBPARAM   NO
PARAM     &Y24
CALL      CVR       3
PARAM     1
PARAM     &D21
CALL      CTRNC     2         &ZZ12
ASSGN     &D21      &ZZ12
MSGPARAM  OPTRM058
PARAM     NO
PARAM     N
PARAM     YES
PARAM     Y
PARAM     ?
PARAM     ?
PARAM     &D21
CALL      CTRNS     8         &ZZ13
ASSGN     &D21      &ZZ13
PARAM     'YES'
PARAM     &D21
CALL      CIFEQ     2         6
MSGPARAM  OPTRM059
NBPARAM   NO
PARAM     &Y22
CALL      CVR       3
ASSGN     &Y23      &BLANK
PARAM     'NO'
PARAM     &D21
CALL      CIFEQ     2         23
PARAM     &BLANK
PARAM     &Y22
CALL      CIFEQ     2         5
MSGPARAM  OPTRM060
NBPARAM   NO
PARAM     &Y23
CALL      CVR       3
PARAM     &BLANK
PARAM     &Y23
CALL      CIFEQ     2         5
MSGPARAM  OPTRM059
NBPARAM   NO
PARAM     &Y22
CALL      CVR       3
PARAM     &BLANK
PARAM     &Y23
CALL      CIFNEQ    2         2
ASSGN     &Y22      &BLANK
PARAM     &BLANK
PARAM     &Y22
CALL      CIFNEQ    2         2
ASSGN     &Y23      &BLANK
PARAM     6
PARAM     5
PARAM     4
PARAM     3
PARAM     &WILI
CALL      CIFEQ     5         52
MSGPARAM  OPTRM045
NBPARAM   NO
PARAM     &Y34
CALL      CVR       3
PARAM     1
PARAM     &D31
CALL      CTRNC     2         &ZZ14
ASSGN     &D31      &ZZ14
MSGPARAM  OPTRM058
PARAM     NO
PARAM     N
PARAM     YES
PARAM     Y
PARAM     ?
PARAM     ?
PARAM     &D31
CALL      CTRNS     8         &ZZ15
ASSGN     &D31      &ZZ15
PARAM     'YES'
PARAM     &D31
CALL      CIFEQ     2         6
MSGPARAM  OPTRM059
NBPARAM   NO
PARAM     &Y32
CALL      CVR       3
ASSGN     &Y33      &BLANK
PARAM     'NO'
PARAM     &D31
CALL      CIFEQ     2         23
PARAM     &BLANK
PARAM     &Y32
CALL      CIFEQ     2         5
MSGPARAM  OPTRM060
NBPARAM   NO
PARAM     &Y33
CALL      CVR       3
PARAM     &BLANK
PARAM     &Y33
CALL      CIFEQ     2         5
MSGPARAM  OPTRM059
NBPARAM   NO
PARAM     &Y32
CALL      CVR       3
PARAM     &BLANK
PARAM     &Y33
CALL      CIFNEQ    2         2
ASSGN     &Y32      &BLANK
PARAM     &BLANK
PARAM     &Y32
CALL      CIFNEQ    2         2
ASSGN     &Y33      &BLANK
PARAM     6
PARAM     5
PARAM     4
PARAM     &WILI
CALL      CIFEQ     4         52
MSGPARAM  OPTRM045
NBPARAM   NO
PARAM     &Y44
CALL      CVR       3
PARAM     1
PARAM     &D41
CALL      CTRNC     2         &ZZ16
ASSGN     &D41      &ZZ16
MSGPARAM  OPTRM058
PARAM     NO
PARAM     N
PARAM     YES
PARAM     Y
PARAM     ?
PARAM     ?
PARAM     &D41
CALL      CTRNS     8         &ZZ17
ASSGN     &D41      &ZZ17
PARAM     'YES'
PARAM     &D41
CALL      CIFEQ     2         6
MSGPARAM  OPTRM059
NBPARAM   NO
PARAM     &Y42
CALL      CVR       3
ASSGN     &Y43      &BLANK
PARAM     'NO'
PARAM     &D41
CALL      CIFEQ     2         23
PARAM     &BLANK
PARAM     &Y42
CALL      CIFEQ     2         5
MSGPARAM  OPTRM060
NBPARAM   NO
PARAM     &Y43
CALL      CVR       3
PARAM     &BLANK
PARAM     &Y43
CALL      CIFEQ     2         5
MSGPARAM  OPTRM059
NBPARAM   NO
PARAM     &Y42
CALL      CVR       3
PARAM     &BLANK
PARAM     &Y43
CALL      CIFNEQ    2         2
ASSGN     &Y42      &BLANK
PARAM     &BLANK
PARAM     &Y42
CALL      CIFNEQ    2         2
ASSGN     &Y43      &BLANK
PARAM     6
PARAM     5
PARAM     &WILI
CALL      CIFEQ     3         52
MSGPARAM  OPTRM045
NBPARAM   NO
PARAM     &Y54
CALL      CVR       3
PARAM     1
PARAM     &D51
CALL      CTRNC     2         &ZZ18
ASSGN     &D51      &ZZ18
MSGPARAM  OPTRM058
PARAM     NO
PARAM     N
PARAM     YES
PARAM     Y
PARAM     ?
PARAM     ?
PARAM     &D51
CALL      CTRNS     8         &ZZ19
ASSGN     &D51      &ZZ19
PARAM     'YES'
PARAM     &D51
CALL      CIFEQ     2         6
MSGPARAM  OPTRM059
NBPARAM   NO
PARAM     &Y52
CALL      CVR       3
ASSGN     &Y53      &BLANK
PARAM     'NO'
PARAM     &D51
CALL      CIFEQ     2         23
PARAM     &BLANK
PARAM     &Y52
CALL      CIFEQ     2         5
MSGPARAM  OPTRM060
NBPARAM   NO
PARAM     &Y53
CALL      CVR       3
PARAM     &BLANK
PARAM     &Y53
CALL      CIFEQ     2         5
MSGPARAM  OPTRM059
NBPARAM   NO
PARAM     &Y52
CALL      CVR       3
PARAM     &BLANK
PARAM     &Y53
CALL      CIFNEQ    2         2
ASSGN     &Y52      &BLANK
PARAM     &BLANK
PARAM     &Y52
CALL      CIFNEQ    2         2
ASSGN     &Y53      &BLANK
PARAM     6
PARAM     &WILI
CALL      CIFEQ     2         52
MSGPARAM  OPTRM045
NBPARAM   NO
PARAM     &Y64
CALL      CVR       3
PARAM     1
PARAM     &D61
CALL      CTRNC     2         &ZZ20
ASSGN     &D61      &ZZ20
MSGPARAM  OPTRM058
PARAM     NO
PARAM     N
PARAM     YES
PARAM     Y
PARAM     ?
PARAM     ?
PARAM     &D61
CALL      CTRNS     8         &ZZ21
ASSGN     &D61      &ZZ21
PARAM     'YES'
PARAM     &D61
CALL      CIFEQ     2         6
MSGPARAM  OPTRM059
NBPARAM   NO
PARAM     &Y62
CALL      CVR       3
ASSGN     &Y63      &BLANK
PARAM     'NO'
PARAM     &D61
CALL      CIFEQ     2         23
PARAM     &BLANK
PARAM     &Y62
CALL      CIFEQ     2         5
MSGPARAM  OPTRM060
NBPARAM   NO
PARAM     &Y63
CALL      CVR       3
PARAM     &BLANK
PARAM     &Y63
CALL      CIFEQ     2         5
MSGPARAM  OPTRM059
NBPARAM   NO
PARAM     &Y62
CALL      CVR       3
PARAM     &BLANK
PARAM     &Y63
CALL      CIFNEQ    2         2
ASSGN     &Y62      &BLANK
PARAM     &BLANK
PARAM     &Y62
CALL      CIFNEQ    2         2
ASSGN     &Y63      &BLANK
PROCEND   
