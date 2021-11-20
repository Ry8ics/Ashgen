**==calc.f    processed by SPAG 4.55N  at 10:58 on  8 May 1995
      SUBROUTINE CALC(NMEMB)
C*** Start of declarations inserted by SPAG
      REAL AX, PSLEN, RHO, RLEN
      INTEGER IACT, IMEMB, NMEMB
      INTEGER Irele(6)
C*** End of declarations inserted by SPAG
C=======================================================================
C     LATEST CORRECTION BY
C
C     PURPOSE
C         MAIN ROUTINE FOR CALCULATION
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
C          NMEMB => NUMBER OF ACTIVATED MEMBERS........: IN
C      From common ccash:
C          MMEMB => ACTIVATED MEMBERS..................: IN
C          RMASS => MASS OF MEMBERS....................: OUT
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
C     CREATED:  05.05.95
C
C     OTHER
C
C=======================================================================
C
C     Include common block containing user input
      INCLUDE 'ccash'
C
C     NB!!! In this routine only calculations shall be performed.
C     No results are printed. The results shall be stored in common
C     variables or in binary files for later access.
C
C     NOTE!!! When this routine is entered everything shall be OK. The
C     program shall under no circumstance crash. Nothing is more annoying
C     than that a program abends after 17 hours of computing.
C
      REAL CONST(4), PROP(14)

      character*8 cpsjnt

C
C     Loop over activated members...
      DO 100 IACT = 1, NMEMB
C
C        As this is just an example the calculation performed here
C        is trivial. Let's just assume that the task was to calcualate
C        the mass of some user defined members.
C
C        Get internal member number...
         IMEMB = MMEMB(IACT)
C
C        Get density of member...
         CALL PSCONS(IMEMB,CONST)
         RHO = CONST(3)
C
C        Get lenght of member...
         RLEN = PSLEN(IMEMB)
C
C        Get cross sectional area...
         CALL PSPROP(IMEMB,PROP)
         AX = PROP(1)
C
C        Calculate mass of member...
         RMASS(IACT) = RHO*RLEN*AX
C
C        The mass of the member is now stored into the common table RMASS.
C
  100 CONTINUE

      nsup=ipsnum('B')
      do 101 isup=1,nsup
        ijoin=ipssup(isup)
        call psrele(isup,irele)
        ifx=irele(1)
        ify=irele(2)
        ifx=irele(3)
        imx=irele(4)
        imy=irele(5)
        imz=irele(6)   
        print*,' JOINT SUPPORT ',cpsjnt(ijoin)
        print*,' JOINT RELEASE ',ifx,ify,ifx,imx,imy,imz
  101 continue
C
      RETURN
      END
