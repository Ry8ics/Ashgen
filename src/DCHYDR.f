      SUBROUTINE DCHYDR(cerr,citem,Iitem,Istat,Iout)
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
C                                      (    SET   ) ( set1, set2, ...)                                            
C         HYDR CD  cdx cdy CM cmx cmy  (   MEMBER ) ( ALL            )
C                                                   ( m1,m2,m3, ...  )
C
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
      REAL CHFAC(4)
      INTEGER IFERR , Istat , IPSNUM, iend, ilen , IOUT, i, Imemb,
     &        imem  , IPSSNM, IPSSNO, ISET, NMEMB  , MEM(MAXNOD), IPSMEM
      CHARACTER*8 Csets, CPSSNM, Cmemb         
C
C=======================================================================
C
      IF ( MATCH('CD',2) ) THEN
C         Read member list
C         Decode factor
          CALL DCREAL(CHFAC(1))
          IF ( IFERR().LT.0 ) THEN
              WRITE (IOUT,Cerr) '/ERROR IN READING CDX' ,
     &                          'PROGRAM STOPPED'
              Istat = -1
              GOTO 200
          ENDIF
          
C         Read member list
C         Decode factor
          CALL DCREAL(CHFAC(2))
          IF ( IFERR().LT.0 ) THEN
              WRITE (IOUT,Cerr) '/ERROR IN READING CDY' ,
     &                          'PROGRAM STOPPED'
              Istat = -1
              GOTO 200
          ENDIF
C
          IF ( MATCH('CM',2) ) THEN
C             Read member list
C             Decode factor
              CALL DCREAL(CHFAC(3))
              IF ( IFERR().LT.0 ) THEN
                  WRITE (IOUT,Cerr) '/ERROR IN READING CMX' ,
     &                             'PROGRAM STOPPED'
                  Istat = -1
                  GOTO 200
              ENDIF
              
C             Read member list
C             Decode factor
              CALL DCREAL(CHFAC(4))
              IF ( IFERR().LT.0 ) THEN
                  WRITE (IOUT,Cerr) '/ERROR IN READING CMY' ,
     &                             'PROGRAM STOPPED'
                  Istat = -1
                  GOTO 200
              ENDIF
          ENDIF
      ENDIF
C
C--------------------------------------------------------
C
C
      IF ( MATCH('MEMBER',3) ) THEN
C         Read member list
          IF ( MATCH('ALL',3) ) THEN
              NMEMB=IPSNUM('M')
              DO 80 Imemb=1,NMEMB
                  !CM(imemb) = fac
                  DO I=1,4
                      CHYDR(I,imemb) = CHFAC(I)
                  ENDDO
 80           CONTINUE
              GOTO 200
          ENDIF
 110      CALL MOVEIP(iend)
          IF ( iend.EQ.0 ) THEN
              CALL DCSTRG(Cmemb,ilen,3)
              Imemb=IPSMEM(Cmemb)
              IF (Imemb.EQ.0) THEN
                  CALL PRIERR()
                  WRITE (*,*) ' *** SEAWARE ERROR - ' , Cmemb ,
     &                        ' DOES NOT EXIST ' 
                  Istat = -1
                  GOTO 200
              ENDIF 
C
              !CM(imemb) = fac
              DO I=1,4
                  CHYDR(I,imemb) = CHFAC(I)
              ENDDO
C
              GOTO 110
          ENDIF
C
C--------------------------------------------------------
C
      ELSEIF ( MATCH('SET',3) ) THEN
C         Loop over sets
C         
 120      CALL MOVEIP(iend)
          IF ( iend.EQ.0 ) THEN
C             Read set name
              CALL DCSTRG(Csets,ilen,3)
C             
C             Get internal set number
C             
              ISET=IPSSNM(Csets)
              IF (ISET.EQ.0) THEN
                  CALL PRIERR()
                  WRITE (*,*) ' *** SEAWARE ERROR - ' , Csets ,
     &                        ' DOES NOT EXIST ' 
                  WRITE(*,*)' SETS THAT ARE DEFINED :'
                  do 130 i=1,ipsnum('T')
                      write(*,*) '   ',CPSSNM(i)
 130              continue
                  Istat = -1
                  GOTO 200
              ENDIF 
C             
C             Get number of members in the set
              NMEMB=IPSSNO(ISET) 
C             
C             Get a list of members in the set
              CALL PSSTME(ISET,MEM,NMEMB)
C             
C             Loop over number of members and fill in the list
              DO 40 IMEM=1,NMEMB
C             
C                 Get internal member id ...
C             
C                 Correction 30.6.2011 ,, skip if mem(imem)<0
                  IF (mem(imem).GT.0) THEN
                      !CM(MEM(IMEM)) = fac
                      DO I=1,4
                          CHYDR(I,MEM(IMEM)) = CHFAC(I)
                      ENDDO
                  ENDIF
 40           CONTINUE
C             
              GOTO 120
          ENDIF
C
C--------------------------------------------------------  
      ELSE
         CALL PRIERR()
         WRITE (IOUT,Cerr) '/MEMBER OR SET EXPECTED' ,
     &                     'PROGRAM STOPPED'
         Istat = -1
         GOTO 200       
      ENDIF     
C
 200  RETURN
      END