**==psditc.spg  processed by SPAG 5.00N  at 11:06 on 23 Mar 1999
      SUBROUTINE PSDITC2(Imemb,Diam,Thick,Istat)
      IMPLICIT NONE
C*** Start of declarations inserted by SPAG
      REAL PI
C*** End of declarations inserted by SPAG
C=======================================================================
C     LATEST CORRECTION BY   B.Melhus 11.10.1995
C                  If member propertie number 9 or 10 holding the diameter
C                  is eqaul to zero the diameter is calculated by using the
C                  moment of inertia and cross sectional area. This is done
C                  because if STRUSES is used for converting tubes to FASTRUDL
C                  they are converted to prismatics with no values on member
C                  propertie number 9 and 10.
C
C
C     PURPOSE
C          CALCULATES THE OUTER DIAMTER AND THICKNESS FOR A TUBE
C
C     METHOD
C
C
C     CALLS FROM
C
C     CALLS TO
C
C
C     INPUT/OUTPUT
C
C     IMEMB => INTERNAL MEMBER NUMBER....................: INPUT
C     DIAM  => DIAMETER OF MEMBER (TUBE).................: OUT
C     THICK => THICKNESS OF MEMBER (TUBE)................: OUT
C
C
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
C     CREATED:  01.06.94
C
C       NB!!  THE MEMBER MUST BE OF TYPE TUBE.
C
C=======================================================================
C
c      INCLUDE 'ccfat'
      PARAMETER (PI=3.141592654)
      REAL prop(14)
      CHARACTER*8 CPSMEM
      REAL ax , ax4pi , Diam , diam2 , fipiax , pidi2 , riy , Thick
      INTEGER Imemb , Istat
C
      Istat = 0
C
C---  GET THE PROPERTIES FOR THE MEMBER
C
      CALL PSPROP(Imemb,prop)
C
C     Fetch the diameter ( member proertie number 9 or 10 (they should be equal))
      Diam = MAX(prop(9),prop(10))
      ax = prop(1)
C
!      IF ( TCKmem(Imemb).GT.0 ) THEN
!C---     USER DEFINED THICKNESS
!         Thick = TCKmem(Imemb)
!      ELSE
         pidi2 = (PI*Diam)*(PI*Diam)
         fipiax = 4.0*PI*ax
         IF ( pidi2.GE.fipiax ) THEN
            Thick = 0.5*Diam - SQRT(pidi2-fipiax)/(2.0*PI)
C           Member propertie number 9 and 10 may be equal zero.
         ELSEIF ( Diam.LT.0.0001 ) THEN
            riy = prop(5)
            Diam = SQRT(8.0*riy/ax+2.0/PI*ax)
            diam2 = Diam*Diam
            ax4pi = 4.0*ax/PI
            IF ( diam2.GT.ax4pi ) THEN
               Thick = 0.5*(Diam-SQRT(diam2-ax4pi))
            ELSE
               Istat = -1
            ENDIF
         ELSE
            Istat = -1
         ENDIF
!      ENDIF
C
C---  DETERMINE THE OUTER DIAMETER AND THE THICKNESS BASED
C     ON MEMBER PROPERTIES OR AS PRESCRIBED BY USER.
C
!C--      USER DEFINED DIAMETER
!      IF ( DIAmem(Imemb).GT.0 ) Diam = DIAmem(Imemb)
C
C     Make a check to see of the diameter and thickness are
C     reasonable
C
      IF ( Diam.LE.0.0 ) THEN
         WRITE (*,*) ' SERIOUS WARNING: MEMBER...: ' , CPSMEM(Imemb)
         WRITE (*,*) ' THE DIAMETER IS FOUND TO BE LESS THAN ZERO'
         Istat = -1
      ENDIF
      IF ( Thick.LE.0.0 ) THEN
         WRITE (*,*) ' SERIOUS WARNING: MEMBER...: ' , CPSMEM(Imemb)
         WRITE (*,*) ' THE THICKNESS IS FOUND TO BE LESS THAN ZERO'
         Istat = -1
      ENDIF
C
      RETURN
      END
