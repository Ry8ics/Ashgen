      SUBROUTINE DCMAIN(NMEMB,ALLMEM,ISTAT)
C=======================================================================
C     LATEST CORRECTION BY
C
C     PURPOSE
C         MAIN DECODING ROUTINE
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
C     CREATED: 05.05.95
C
C     OTHER
C
C=======================================================================
C
      INCLUDE 'ccinit'
      INCLUDE 'ccash'
C
      INTEGER IFERR, IINN, IOUT, ISTAT, NMEMB
      LOGICAL LUSER, ALLMEM
      LOGICAL MATCH
      CHARACTER*80 CBLANK
      CHARACTER*40 CITEM, IITEM, CERR
C
C     Variables used in print command ...
C     New variables for Postbase ( start )
      INTEGER ld(MAXNOD) , nld , md(MAXNOD) , nmd , id(MAXNOD)
c
      DATA LUSER/.FALSE./
C
      DATA CITEM/'(/'' INPUT ITEM...: '',A ,'' ?'')'/
      DATA IITEM/'(/'' INPUT ITEM...: '',I2,'' ?'')'/
      DATA CERR/'(/'' <prgnam> ERROR.: '',A/:9(16X,A/:))'/
C
      CBLANK(1:80) = ' '
      NMEMB = 0
C
C-----SET THE INPUT AND OUTPUT UNITS
C
      IINN = 5
      IOUT = 6
      CALL INITDC(IINN,IOUT)
C
C-----READ FIRST LINE CONTAINING DATABASE (create or attach)
C
      CALL DCDATA(CERR,CITEM,LUSER,CBASE,DMPFIL,CIDENT,MAXNOD,IOUT,
     +            ISTAT)
C
      IF ( ISTAT.EQ.-1 ) STOP
          IF ( LUSER ) THEN
              CALL READL
C
C             CHECK IF THE DATABASE NAME IS 'USER'. THE DATABASE NAME
C             ( CBASE ) IS A COMMON VARIABLE SET IN DCDATA
C
 
C             Start decoding user input. All data are read in from
C             input file. No database is attached.
 
              RETURN
          ENDIF
C
C
C         START DECODING THE INPUT FILE
C
   50     CALL READL
C
C----CHECK IF ANYTHING IS LEFT
C
          IF ( IFERR().GT.-1 ) THEN
C*********************************************************************
C
C
C         Start decoding user input. The program is now attached to
C         the database.
C
 
C         ex.: activate member
              IF ( MATCH('ACTIVE',3) ) THEN
                  IF ( MATCH('MEMBER',3) ) THEN
                      CALL DCMEMB(CERR,CITEM,IITEM,IOUT,NMEMB,ALLMEM,
     +                            ISTAT)
                      IF ( ISTAT.EQ.-1 ) GOTO 100
                  ENDIF
C
C---
              ELSEIF ( MATCH('SUPPORT',3)  ) THEN
                  CALL DCJSUPP(cerr,citem,Istat,Iout)
                  IF( ISTAT.EQ.-1) GOTO 100
C
C---
              ELSEIF ( MATCH('SUPPORT',3)  ) THEN
                  CALL DCJSUPP(cerr,citem,Istat,Iout)
                  IF( ISTAT.EQ.-1) GOTO 100
C
C---
              ELSEIF ( MATCH('MATRIX',3)  ) THEN
                  CALL DCSPRING(cerr,citem,Istat,Iout)
                  IF( ISTAT.EQ.-1) GOTO 100
C
C---
              ELSEIF ( MATCH('HYDRO',3)  ) THEN
                  CALL DCHYDR(cerr,citem,Iitem,Istat,Iout)
                  IF( ISTAT.EQ.-1) GOTO 100
C
C---
              ELSEIF ( MATCH('AERO',3)  ) THEN
                  CALL DCAERO(cerr,citem,Iitem,Istat,Iout)
                  IF( ISTAT.EQ.-1) GOTO 100
C
C---
              ELSEIF ( MATCH('MARINE',3)  ) THEN
                  IF ( MATCH('GROWTH',3)  ) THEN
                      CALL DCMGROW(cerr,citem,Iitem,Istat,Iout)
                      IF( ISTAT.EQ.-1) GOTO 100
                  ENDIF
C
            ELSEIF ( MATCH('PRINT',3) ) THEN
            CALL DCP(IOUT,id,MAXNOD,ld,nld,md,nmd)
C
C          In this section rest of the input file is decoded. Input that
C          have interaction to the database, should only contain
C          pointers to the requested data on the database. This is as shown
C          in DCMEMB where the array MMEMB contain pointers to activated
C          members for later access. All input items should be assembled in
C          a common block as for MMEMB. Number of each item should
C          be passed trough the subroutine call as for NMEMB.
C
C
C
C---     CHECK FOR LEGAL COMMENT SIGNS
C
              ELSEIF ( MATCH('%',1) ) THEN
              ELSEIF ( MATCH('$',1) ) THEN
              ELSEIF ( MATCH('#',1) ) THEN
              ELSEIF ( MATCH('!',1) ) THEN
              ELSEIF ( MATCH('*',1) ) THEN
C
C---     BLANK LINE ?
C
              ELSEIF ( .NOT.(MATCH(CBLANK,80)) ) THEN
                  CALL PRIERR()
                  WRITE (IOUT,CERR) 'UNKNOWN INPUT'
                  ISTAT = -1
                  GOTO 100
              ENDIF
              GOTO 50
          ENDIF
C
  100 RETURN
      END
