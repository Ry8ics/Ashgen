      PROGRAM MAIN
C=======================================================================
C     LATEST CORRECTION BY
C
C     PURPOSE
C         MAIN PROGRAM FOR SOMETHING
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
C
C     LOCAL
C
C          NMEMB => NUMBER OF ACTIVATED MEMBERS
C    From common ccash:
C          MMEMB => ACTIVATED MEMBERS
C
C     PROGRAMMED BY:  B. MELHUS  - AKER ENGINEERING
C
C     VERSION:
C
C
C
C     CREATED:  05.05.95
C
C     OTHER
C
C=======================================================================
C
C     Include the common block that will contain the user input.
      INCLUDE 'ccash'
C      
      INTEGER IMEMB, IPSMEM, IPSNUM, ISTAT, NMEMB, INI
      LOGICAL ALLMEM
      CHARACTER*8 CPSMEM
      DATA ALLMEM/.FALSE./
C=======================================================================
C
C     Initialize number off support points
      NJSUPP = 0
C
C     Initialize marine growth tables
      DO INI=1,MAXMEM
          MGROW(1,INI) = 0.0
          MGROW(2,INI) = 0.0
      ENDDO
C     Initialize hydrodynamic properties
      DO INI=1,MAXMEM
          DO I=1,4
              CHYDR(I,INI) = 1.0
          ENDDO
      ENDDO
C     Initialize aerodynamic properties
      DO INI=1,MAXMEM
          DO I=1,2
              CAERO(I,INI) = 0.65
          ENDDO
      ENDDO
C     Initialize spring components
      SPRING(:)=0.0
C-------------------
C
C     Call routine for decoding the input file
 
      CALL DCMAIN(NMEMB,ALLMEM,ISTAT)
      IF ( ISTAT.EQ.-1 ) STOP
C
      IF ( ALLMEM ) THEN
         NMEMB = IPSNUM('M')
         DO 50 IMEMB = 1, NMEMB
            MMEMB(IMEMB) = IPSMEM(CPSMEM(IMEMB))
   50    CONTINUE
      ENDIF
C
C
C   NB!!! If you by here are calling a routine for calcalation of something
C         based on input as given in the input file, only calculation shall
C         be performed. No output of results should be written. As the results
C         are calculated the results should be written to a common block for later
C         access in a separate output routine. The advantage of this is that the
C         calculations and the output are completely separated. When the results are
C         assembled into a common block it is easy to write results as based on
C         input from the user. For instance only max. values are written or the
C         the results are written in sorted order. If the amount of result data are
C         huge the results should be assembled into binary files instead of a common block.
C         The reason for this is that the internal storage capacity is limited so that
C         the computer will start to swap back and forth from the disk with the result
C         that trashing may occur and the program will be slowed down.
C
C     Call main routine for calculation.
      CALL CALC(NMEMB)
C
C     Call routine for output.
      CALL OUTPUT(NMEMB)
C
      END
