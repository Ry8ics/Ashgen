      SUBROUTINE DCMEMB(CERR,CITEM,IITEM,IOUT,NMEMB,ALLMEM,ISTAT)
C=======================================================================
C     LATEST CORRECTION BY
C
C     PURPOSE
C         READS THE MEMBERS THAT ARE TO BE USED IN THE ANALYSIS
C
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
C          NMEMB => NUMBER OF ACTIVATED MEMBERS........: OUT
C          ALLMEM=> LOGICAL FOR IF ALL MEMBERS IN THE
C                   STRUCTURE ARE ACTIVATED............: OUT
C          ISTAT => STATUS FLAG........................: OUT
C      From common ccash:
C          MMEMB => ACTIVATED MEMBERS..................: OUT
C
C     LOCAL
C
C
C     PROGRAMMED BY:  B. MELHUS  - AKER ENGINEERING
C
C     VERSION:
C
C
C
C     CREATED:
C
C     OTHER
C
C=======================================================================
C
      INCLUDE 'ccash'
C
      INTEGER IEND, IFERR, ILEN, IOUT, IPSMEM, ISTAT, NMEMB
      CHARACTER*8 CMEMB
      CHARACTER*(*) CERR, CITEM, IITEM
      LOGICAL ALLMEM, MATCH
C
      IF ( MATCH('ALL',3) ) THEN
         ALLMEM = .TRUE.
         GOTO 200
      ENDIF
C
  100 CALL MOVEIP(IEND)
      IF ( IEND.EQ.0 ) THEN
         ALLMEM = .FALSE.
         IF ( NMEMB.GT.MAXNOD ) THEN
            CALL PRIERR()
            WRITE (IOUT,IITEM) NMEMB
            WRITE (IOUT,CERR) 'TOO MANY MEMBERS', 'PROGRAM STOPPED'
            ISTAT = -1
            GOTO 200
         ENDIF
C
         CALL DCSTRG(CMEMB,ILEN,3)
C
         IF ( ILEN.GT.8 ) THEN
            CALL PRIERR()
            WRITE (IOUT,CITEM) CMEMB
            WRITE (IOUT,CERR) 'TOO MANY CHARACTERS IN INPUT ITEM', 
     +                        'PROGRAM STOPPED IN DCEMMB'
 
            ISTAT = -1
            GOTO 200
         ENDIF
C
         IF ( IFERR().LT.0 ) THEN
            WRITE (IOUT,CITEM) CMEMB
            WRITE (IOUT,CERR) '*** ERROR IN DECODING MEMBERS'
            ISTAT = -1
            GOTO 200
         ENDIF
C
         NMEMB = NMEMB + 1
C        fill in a table containing member internal numbers.
         MMEMB(NMEMB) = IPSMEM(CMEMB)
C
         IF ( MMEMB(NMEMB).EQ.0 ) THEN
            CALL PRIERR()
            WRITE (IOUT,CITEM) CMEMB
            WRITE (IOUT,CERR) ' REQUESTED MEMBER NOT FOUND IN DATABASE'
            ISTAT = -1
            GOTO 200
         ENDIF
C
         GOTO 100
      ENDIF
C
  200 RETURN
      END
