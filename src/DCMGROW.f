      SUBROUTINE DCMGROW(cerr,citem,Iitem,Istat,Iout)
      IMPLICIT NONE
C=======================================================================
C     LATEST CORRECTION BY       
C
C     PURPOSE
C       SPECIFY HYDRO CD AND CM PROPERTIES FOR MEMBERS
C
C
C
C     METHOD
C         MARINE GROWTH Z1 Z2 DENS1 THICK1...
C                       Z3 Z4 DENS2 THICK2...
C                       Z(1,K) Z(2,K) DENS(3,K) THICK(4,K)...
C     CALLS FROM
C
C     CALLS TO
C
C
C     INPUT/OUTPUT
C
C
C
C     LOCAL
C
C
C     PROGRAMMED BY:  M. Sarwar - Kvaerner
C
C     VERSION:
C
C
C
C     CREATED:  05.08.19
C
C     OTHER
C
C       The common variable should be filled with some default
C       values prior of using this routine.
C
C=======================================================================
C
      INCLUDE 'ccash'
C
      CHARACTER*(*) Cerr , Citem , Iitem
      LOGICAL MATCH
      REAL GROWTH(4,100), COOR1(3), COOR2(3)
      INTEGER IFERR , Istat , IPSNUM, iend, ilen , IOUT, i, Imemb,
     +        imem, IPSSNM, IPSSNO, ISET, NMEMB, MEM(MAXNOD), IPSMEM,
     +        NMGINPUT, J, ZMIN
      CHARACTER*8 Csets, CPSSNM, Cmemb         
C
C=======================================================================
C
C     Initialize number of rows given as marine growth input
      NMGINPUT=0
C
C--------------------------------------------------------

 120  CALL MOVEIP(iend)
      IF ( iend.EQ.0 ) THEN
C
          NMGINPUT = NMGINPUT + 1
C
C         Read first elevation value
          CALL DCREAL(GROWTH(1,NMGINPUT))
          IF ( IFERR().LT.0 ) THEN
              WRITE (IOUT,Cerr) '/ERROR IN READING Z1' ,
     &                          'PROGRAM STOPPED'
              Istat = -1
              GOTO 200
          ENDIF
C
C         Read second elevation value
          CALL DCREAL(GROWTH(2,NMGINPUT))
          IF ( IFERR().LT.0 ) THEN
              WRITE (IOUT,Cerr) '/ERROR IN READING Z2' ,
     &                          'PROGRAM STOPPED'
              Istat = -1
              GOTO 200
          ENDIF
C
C         Read marine growth density
          CALL DCREAL(GROWTH(3,NMGINPUT))
          IF ( IFERR().LT.0 ) THEN
              WRITE (IOUT,Cerr) '/ERROR IN READING DENSITY' ,
     &                          'PROGRAM STOPPED'
              Istat = -1
              GOTO 200
          ENDIF
C
C         Read marine growth thickness
          CALL DCREAL(GROWTH(4,NMGINPUT))
          IF ( IFERR().LT.0 ) THEN
              WRITE (IOUT,Cerr) '/ERROR IN READING THICKNESS' ,
     &                          'PROGRAM STOPPED'
              Istat = -1
              GOTO 200
          ENDIF
C
          GOTO 120
C
      ELSEIF(NMGINPUT<1) THEN
         CALL PRIERR()
         WRITE (IOUT,Cerr) '/MARINE GROWTH DATA EXPECTED' ,
     &                     'PROGRAM STOPPED'
         Istat = -1
         GOTO 200
      ENDIF
C--------------------------------------------------------
C
C     Store correct marine growth for each element
      NMEMB=IPSNUM('M')
      DO IMEMB=1,NMEMB
C         Retrieve coordinates of the member
          CALL PSMCOR(IMEMB,COOR1,COOR2)
C
C         Find the elevation of the lowest coordinate of the member
          ZMIN=MIN(COOR1(3),COOR2(3))
C
          DO J=1,NMGINPUT
C
              IF ( ZMIN>=GROWTH(1,NMGINPUT).AND.
     +             ZMIN<GROWTH(2,NMGINPUT)       ) THEN
                  DO I=1,2
                      MGROW(I,IMEMB) = GROWTH(I+2,NMGINPUT)
                  ENDDO
              ENDIF
C
          ENDDO
C
      ENDDO
C
C=======================================================================
 200  RETURN
      END