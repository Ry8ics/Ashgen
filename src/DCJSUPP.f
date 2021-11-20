      SUBROUTINE DCJSUPP(CERR,CITEM,ISTAT,IOUT)
C=======================================================================
C     LATEST CORRECTION BY
C
C     PURPOSE
C         READS THE CONSTRAINTS TO BE APPLIED ON THE JOINT SPECIFIED
C
C     METHOD
C
C     CALLS FROM
C
C     CALLS TO
C
C
C     INPUT/OUTPUT
C
C          ISTAT => STATUS FLAG........................: OUT
C          
C
C     LOCAL
C
C
C     PROGRAMMED BY:  M. SARWAR  - KVAERNER
C
C     VERSION:
C
C
C
C     CREATED: 06.08.2019
C
C     OTHER
C
C=======================================================================
C
      INCLUDE 'ccash'
C
      INTEGER IFERR, IOUT, ISTAT, ISET,IEND,ILEN,IJOIN,IPSJNT,I
      CHARACTER*8 CJOIN,CPSJNT
      CHARACTER*(*) CERR, CITEM
      LOGICAL  MATCH
C=======================================================================
C
  100 CALL MOVEIP(IEND)
      IF ( IEND.EQ.0 ) THEN
C
          CALL DCSTRG(CJOIN,ILEN,3)
C         
          IF ( IFERR().LT.0 ) THEN
              WRITE (IOUT,CITEM) CJOIN
              WRITE (IOUT,CERR) '*** ERROR IN DECODING JOINTS'
              ISTAT = -1
              GOTO 200
          ENDIF
C         
          IJOIN = IPSJNT(CJOIN)
C         Store the joint with constraints
          NJSUPP = NJSUPP +1
          JSUPP(1,NJSUPP) = IJOIN
C         
          IF ( IJOIN.EQ.0 ) THEN
              CALL PRIERR()
              WRITE (IOUT,CITEM) CJOIN
              WRITE (IOUT,CERR) ' REQUESTED JOINT NOT FOUND IN DATABASE'
              ISTAT = -1
              GOTO 200
          ENDIF
C         
C         Read wether the support is fixed or pinned
          IF (MATCH('FIXED',3) ) THEN
              JSUPP(2,NJSUPP) = 1
          ELSEIF (MATCH('PINNED',3) ) THEN
              JSUPP(2,NJSUPP) = 2
          ELSEIF (MATCH('SPRING',3) ) THEN
              JSUPP(2,NJSUPP) = 3
          ELSE 
              WRITE (IOUT,Cerr) '/ERROR IN READING SUPPORT CONDITION ' ,
     &        'FIXED, PINNED OR SPRING ALLOWED', 'PROGRAM STOPPED'
              Istat = -1
              GOTO 200
          ENDIF
C          
C         
          GOTO 100
      ENDIF
C
  200 RETURN
      END