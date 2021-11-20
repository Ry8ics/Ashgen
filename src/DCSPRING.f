      SUBROUTINE DCSPRING(CERR,CITEM,ISTAT,IOUT)
C=======================================================================
C     LATEST CORRECTION BY
C
C     PURPOSE
C         READS THE CONSTRAINTS TO BE APPLIED ON THE JOINT SPECIFIED
C                     New code, B.Melhus 17.09.2021
C                     Read pile stiffness matrix ...
C                     Just read it as a string of numbers...
C                     The following numbers will be read, i.e. 21 elements.
C
C                     Kxx   Kxy  Kxz  Kxtx    Kxty    Kxtz  &             
C                           Kyy  Kyz  Kytx    Kyty    Kytz  &
C                                Kzz  Kztx    Kzty    Kztz  &
C                                     Ktxtx   Ktxty   Ktxtz &
C                                             Ktyty   Ktytz &                
C                                                     Ktztz
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
C     CREATED: 17.09.2021
C
C     OTHER
C
C=======================================================================
C
      INCLUDE 'ccash'
C
      INTEGER IFERR, IOUT, ISTAT, ISET,IEND,ILEN,IJOIN,IPSJNT,I,NITME,
     +        NSTIFF_ITEMS 
      CHARACTER*8 CJOIN,CPSJNT
      CHARACTER*(*) CERR, CITEM
      LOGICAL  MATCH
C=======================================================================
C
      NSTIFF_ITEMS=21
      CALL DCREAM(NSTIFF_ITEMS,SPRING,Nitem)
      IF ( IFERR().LT.0 ) THEN
          WRITE (*,'(A)')
     &    'ERROR READING PILE STIFFNESS MATRIX PROGRAM STOPPED'
          Istat = -1
          GOTO 100
      ENDIF
      IF (NITEM.NE.NSTIFF_ITEMS) THEN
          CALL PRIERR()
          WRITE (IOUT,CERR) 'Wrong number of items.'
          WRITE (IOUT,'(A,I0)')
     &    'Required number of items',NSTIFF_ITEMS
          ISTAT = -1
          GOTO 100
      ENDIF
C
  100 RETURN
      END