      SUBROUTINE OUTPUT(NMEMB)
C=======================================================================
C     LATEST CORRECTION BY
C
C     PURPOSE
C         MAIN ROUTINE FOR OUTPUT
C
C     METHOD
C
C     CALLS FROM
C
C     CALLS TO
C
C
C     INPUT/OUTPUT
C          NMEMB => NUMBER OF ACTIVATED MEMBERS........: IN
C      From common cctst:
C          RMASS => MASS OF MEMBERS....................: IN
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
      INCLUDE 'ccash'
C
      INTEGER NMEMB,NJOINT,IMEMB,J1,J2,NSUP, U, I
      REAL CONST(4), PROP(14), COOR(3), DIAM, THICK, HEIGHT, WIDTH,
     +     UNIFAC , PROPME(14), DENS_L, EI1, EI2, GMOD, GJ,
     +     EA 
      CHARACTER*8 CPSMEM,CPSJNT,CSUPP(2)
      CHARACTER*8 Cleng , Cweigh , Ctim , Cangle
      CHARACTER*9 CSTEEL
      CHARACTER*11 CTYPE
      CHARACTER*14 CTAB
      CHARACTER*64 CPSNAM
      CHARACTER*19 CTIME
      LOGICAL LFIRST 
C
      ALLOCATABLE CTAB(:)
C
C=======================================================================
C     Unit number for the file written
      U=3218
C
C     Open file for writing
      OPEN(NEWUNIT=U,FILE='Ashes.txt',STATUS="REPLACE",ACTION="WRITE")
C
C=======================================================================
C
      LFIRST=.TRUE.
C
C     Get number of members and joints in the model
      NMEMB=IPSNUM('M')
      NJOINT=IPSNUM('J')
C
      ALLOCATE (CTAB(NMEMB),STAT=ISTAT)
      IF (ISTAT.NE.0) THEN
        WRITE(*,*) 'ALLOCATION ERROR FOR CTAB '
        STOP
      ENDIF 
C
C
      CALL PSUNI(Cleng,Cweigh,Ctim,Cangle)
      IF(CWEIGH=='    KTON') THEN
          UNIFAC=1.0E6
      ELSEIF(CWEIGH=='     TON') THEN
          UNIFAC=1.0E3
      ELSEIF(CWEIGH=='     KGS') THEN
          UNIFAC=1.0E0
      ELSEIF(CWEIGH=='    MNEW') THEN
          UNIFAC=1.0E6
      ELSEIF(CWEIGH=='    KNEW') THEN
          UNIFAC=1.0E3
      ELSEIF(CWEIGH=='     NEW') THEN
          UNIFAC=1.0E0
      ELSE
          UNIFAC=1.0E0
      ENDIF
C
C======================================================================      
C     Write a heading...
      CALL DATIM(CTIME)
      WRITE(U,'(A)')'###############################################'
      WRITE(U,'(A,A19)')
     +  '# Ashes foundation support input file created on ',CTIME
      WRITE(U,'(A,A)')  '# SIF FILE....: ',CPSNAM()
      WRITE(U,'(A)') '#'
      WRITE(U,'(A)')'###############################################'
C======================================================================      
      WRITE(U,'(A)') 'Name'
      WRITE(U,'(A)') 'Jacket'
      WRITE(U,*)
      WRITE(U,'(A)')'###############################################'
C======================================================================      
      WRITE(U,'(A)') 'Orientation'
      WRITE(U,'(A)') 'Heading 0'  
      WRITE(U,*)
      WRITE(U,'(A)')'###############################################'
C
C======================================================================      
      WRITE(U,'(A)') 'Tubular Tower nodes'
      WRITE(U,'(A)') '# Node name'
      WRITE(U,'(A)') 'Jt_Tower'
      WRITE(U,*)
      WRITE(U,'(A)')'###############################################'
C
C======================================================================      
C     Write material   
      WRITE(U,'(A)') 'Materials'
      DO 100 IMEMB = 1, NMEMB
         CALL PSCONS(IMEMB,CONST)
         CSTEEL='S'//CPSMEM(IMEMB)
!C                                 EMOD     POISSSON   DENSITY   DAMPING 
!         WRITE (*,900) CSTEEL,CONST(1)*UNIFAC,CONST(4), CONST(3)*UNIFAC,
!     +     CONST(2)
C                                 EMOD     POISSSON   DENSITY 
         WRITE(U,900) CSTEEL,CONST(1)*UNIFAC,CONST(4), CONST(3)*UNIFAC
  100 CONTINUE
      WRITE(U,*)
      WRITE(U,'(A)')'###############################################'
C
C======================================================================      
C     Circular hollow cross sections     
      DO 110 IMEMB = 1, NMEMB
C        Get type of member
          CALL PSPROF(IMEMB,igeo,Itype,Prop)
          !print*,'CPSMEM(IMEMB) itype',IMEMB,CPSMEM(IMEMB),itype
c          call psprop(imemb,prop)
c           print*,'CPSMEM(IMEMB) prop ',prop
C         Itype = 1     -> Bar   profile
C         Itype = 2     -> Box   profile
C         Itype = 3     ->  I    profile
C         Itype = 4     -> Pipe  profile
C         Itype = 5     ->  L    profile
C         Itype = 6     ->  C    profile (channel)
C         Itype = 7     ->  General
          IF (ITYPE.EQ.4) THEN
C             PROP(1) => DI    : Inner diameter of tube
C             PROP(2) => DY    : Outer diameter of tube
C             PROP(3) => T     : Thickness of tube (not necessary if DI is given)  
              IF (LFIRST) THEN 
                  WRITE(U,'(A)') 'Circular hollow cross sections'
                  LFIRST=.FALSE.
              ENDIF   
              DIAM=PROP(2)
              THICK=PROP(3)
CC            WRITE(CTYPE,'(A3,F5.3,A1,F5.3)') 'CH_',DIAM,'x',THICK
              CTYPE='CH_'//CPSMEM(IMEMB)
              WRITE(U,910) CTYPE,DIAM,THICK,'S'//CPSMEM(IMEMB),
     +        MGROW(1:2,IMEMB),CAERO(1,IMEMB),CHYDR(1,IMEMB),
     +        CHYDR(3,IMEMB)
C             Store CTYPE for later access
              CTAB(IMEMB)=CTYPE
C
!C         Converting I sections to pipe sections
!          ELSEIF (ITYPE.EQ.3) THEN
!C
!C             For I or H:
!C             ITYPE = 3
!C             PROP(1) => HZ    : Height of beam
!C             PROP(2) => TY    : Thickness of web
!C             PROP(3) => BT    : Width of top flange 
!C             PROP(4) => TT    : Thickness of top flange 
!C             PROP(5) => BB    : Width of bottom flange
!C             PROP(6) => TB    : Thickness of bottom flange 
!C             PROP(7) => SFY   : Factor modifying shear area 
!C             PROP(8) => SFZ   : Factor modifying shear area 
!C             PROP(9) => NLOBYT: Number of integration points 
!C             PROP(10)=> NLOBYB: Number of integration points 
!C             PROP(11)=> NLOBZ : Number of integration points 
!C
!              CALL PSDITC2(IMEMB,DIAM,THICK,ISTAT)
!C
!              IF (LFIRST) THEN 
!                  WRITE(U,'(A)') 'Circular hollow cross sections'
!                  LFIRST=.FALSE.
!              ENDIF   
!              !DIAM=PROP(2)
!              !THICK=PROP(3)
!CC            WRITE(CTYPE,'(A3,F5.3,A1,F5.3)') 'CH_',DIAM,'x',THICK
!              CTYPE='CH_'//CPSMEM(IMEMB)
!              WRITE(U,910) CTYPE,DIAM,THICK,'S'//CPSMEM(IMEMB),
!     +        MGROW(1:2,IMEMB),CAERO(1,IMEMB),CHYDR(1,IMEMB),
!     +        CHYDR(3,IMEMB)
!C             Store CTYPE for later access
!              CTAB(IMEMB)=CTYPE
C
          ELSE
C             Only tubulars for the time being...
              CTAB(IMEMB)=' ' 
         ENDIF
 110  CONTINUE      
      WRITE(U,*)
      WRITE(U,'(A)')'###############################################'
C======================================================================
C
C     Rectangular hollow cross sections     
      LFIRST=.TRUE.
      DO 111 IMEMB = 1, NMEMB
C        Get type of member
          CALL PSPROF(IMEMB,igeo,Itype,Prop)
C         Itype = 1     -> Bar   profile
C         Itype = 2     -> Box   profile
C         Itype = 3     ->  I    profile
C         Itype = 4     -> Pipe  profile
C         Itype = 5     ->  L    profile
C         Itype = 6     ->  C    profile (channel)
C         Itype = 7     ->  General
          IF (ITYPE.EQ.2) THEN
C             For Box:
C             ITYPE = 2
C             PROP(1) => HZ    : Height of beam
C             PROP(2) => TY    : Thickness of vertical walls (webs)
C             PROP(3) => TB    : Thickness of bottom flange
C             PROP(4) => TT    : Thickness of top flange 
C             PROP(5) => BY    : Width of box
              IF (LFIRST) THEN 
                  WRITE(U,'(A)') 'Rectangular hollow cross sections'
                  LFIRST=.FALSE.
              ENDIF   
              HEIGHT = PROP(1)
              WIDTH  = PROP(5)
              THICK  = PROP(2)
CC            WRITE(CTYPE,'(A3,F5.3,A1,F5.3)') 'CH_',DIAM,'x',THICK
              CTYPE='CH_'//CPSMEM(IMEMB)
              WRITE(U,911) CTYPE,HEIGHT,WIDTH,THICK,'S'//CPSMEM(IMEMB),
     +        MGROW(1:2,IMEMB),CAERO(1:2,IMEMB),CHYDR(1:4,IMEMB)
C             Store CTYPE for later access
              CTAB(IMEMB)=CTYPE
C
         ENDIF
 111  CONTINUE      
      WRITE(U,*)
      WRITE(U,'(A)')'###############################################'
C======================================================================
C
C     Rectangular shape cross sections     
      LFIRST=.TRUE.
      DO 112 IMEMB = 1, NMEMB
C        Get type of member
          CALL PSPROF(IMEMB,igeo,Itype,Prop)
C         Itype = 1     -> Bar   profile
C         Itype = 2     -> Box   profile
C         Itype = 3     ->  I    profile
C         Itype = 4     -> Pipe  profile
C         Itype = 5     ->  L    profile
C         Itype = 6     ->  C    profile (channel)
C         Itype = 7     ->  General
c
C         Converting I sections to rectangular shape cross section
          IF (ITYPE.EQ.3) THEN
C             For I or H:
C             ITYPE = 3
C             PROP(1) => HZ    : Height of beam
C             PROP(2) => TY    : Thickness of web
C             PROP(3) => BT    : Width of top flange 
C             PROP(4) => TT    : Thickness of top flange 
C             PROP(5) => BB    : Width of bottom flange
C             PROP(6) => TB    : Thickness of bottom flange 
C             PROP(7) => SFY   : Factor modifying shear area 
C             PROP(8) => SFZ   : Factor modifying shear area 
C             PROP(9) => NLOBYT: Number of integration points 
C             PROP(10)=> NLOBYB: Number of integration points 
C             PROP(11)=> NLOBZ : Number of integration points 
C
C             Retrieve elastic moduli and density of member:
C             CONST(1) =>  ELASTICITY MODULUS (E)
C             CONST(2) =>  Coefficient of thermal expansion (CTE)
C             CONST(3) =>  DENSITY (RHO)
C             CONST(4) =>  POISSON RATIO (POISSON)
              CALL PSCONS(IMEMB,CONST)
C
C             PSROP(1)  => AX : CROSS SECTIONAL AREA
C             PSROP(2)  => AY : SHEAR AREA WITH RESPECT TO THE LOCAL Y-AXIS
C             PSROP(3)  => AZ : SHEAR AREA WITH RESPECT TO THE LOCAL Z-AXIS
C             PSROP(4)  => IX : MOMENT OF INTERTIA ABOUT LOCAL X-AXIS ( TORSION )
C             PSROP(5)  => IY : MOMENT OF INTERTIA ABOUT LOCAL Y-AXIS
C             PSROP(6)  => IZ : MOMENT OF INTERTIA ABOUT LOCAL Z-AXIS
              CALL PSPROP(IMEMB,PROPME)
C
C             Density per length in kgs
              DENS_L = PROPME(1)*CONST(3)*UNIFAC
C
C             Bending stiffness per unit length in the height direction. 
C             Equal to the Young modulus of the material multiplied by the 
C             second moment of area in the height direction.
C             Also called flexural rigidity 
              !EI1 = CONST(1)*PROPME(5)*UNIFAC
              EI1 = CONST(1)*PROPME(6)*UNIFAC
C
C             Bending stiffness per unit length in the width direction. 
C             Equal to the Young modulus of the material multiplied by the 
C             second moment of area in the width direction. 
C             Also called flexural rigidity 
              !EI2 = CONST(1)*PROPME(6)*UNIFAC
              EI2 = CONST(1)*PROPME(5)*UNIFAC
C
C             Torsional stiffness per unit length. 
C             Equal to the shear modulus of the material multiplied 
C             by the torsional constant. Also called torsional rigidity
              GMOD = CONST(1)/(2.0*(1.0+CONST(4)))
              GJ   = GMOD*PROPME(4)*UNIFAC
C
C             Torsional stiffness per unit length. 
C             Equal to the shear modulus of the material multiplied
C             by the torsional constant. Also called torsional rigidity 
              EA = CONST(1)*PROPME(1)*UNIFAC
C
C
              IF (LFIRST) THEN 
                  WRITE(U,'(A)') 'Rectangular shape cross sections'
                  LFIRST=.FALSE.
              ENDIF   
              CTYPE='CH_'//CPSMEM(IMEMB)
              WRITE(U,912) CTYPE,HEIGHT,WIDTH,DENS_L,EI1,EI2,GJ,EA,
     +        MGROW(1:2,IMEMB),CAERO(1:2,IMEMB),CHYDR(1:4,IMEMB)
C             Store CTYPE for later access
              CTAB(IMEMB)=CTYPE
C
         ENDIF
 112  CONTINUE      
      WRITE(U,*)
      WRITE(U,'(A)')'###############################################'
C======================================================================
C     Joints...
      WRITE(U,'(A)') 'Nodes'
      DO 120 IJO=1,NJOINT
          CALL PSCOOR(IJO,COOR)
          WRITE(U,920) CPSJNT(IJO),COOR(1),COOR(2),COOR(3)
 120  CONTINUE  
      WRITE(U,*)
      WRITE(U,'(A)')'###############################################'
C======================================================================  
C     Member incidences
      WRITE(U,'(A)') 'Members'
      DO 130 IMEMB=1,NMEMB
        CALL PSINCI(IMEMB,J1,J2) 
        IF (CTAB(IMEMB).NE.' ') THEN
        WRITE(U,930) CPSMEM(IMEMB),CPSJNT(J1),CPSJNT(J2),CTAB(IMEMB),'1'
        ENDIF
 130  CONTINUE  
      WRITE(U,*)
      WRITE(U,'(A)')'###############################################'
C======================================================================
C     Supports
c      NSUP=IPSNUM('B')
c      DO 140 ISUP=1,NSUP
c        JOISUP=IPSSUP(ISUP)
c        PRINT*,' JOISUP ',CPSJNT(JOISUP)
c 140  CONTINUE
C======================================================================  
C     Supports
      WRITE(U,'(A)') 'Supports'
      WRITE(U,'(A)') '# Name   Type  Node   name'
      CSUPP(1) = 'Fixed   '
      CSUPP(2) = 'Pinned  '
      DO ISUP=1,NJSUPP
          IF (JSUPP(2,ISUP)/=3)THEN
              WRITE(U,940) 'Sp'//CPSJNT(JSUPP(1,ISUP)),
     +                  CSUPP(JSUPP(2,ISUP)),   CPSJNT(JSUPP(1,ISUP))
          ENDIF
      ENDDO
      WRITE(U,*)
      WRITE(U,'(A)')'###############################################'
C======================================================================  
C     Spring name and reference
      WRITE(U,'(A)') 'Springs'
      WRITE(U,'(3A)') '# Name  Type  Node  name  Stiffness x-dir (N/m ',
     +                ' or Nm/rad)  Stiffness y-dir. Stiffness z-dir. ',
     +                ' [IsPy? = 0 (-)] 	[Sensor = 0]'
      DO ISUP=1,NJSUPP
          IF (JSUPP(2,ISUP)==3)THEN
              WRITE(U,950) 'Spring'//CPSJNT(JSUPP(1,ISUP)),
     +         ' Table:SixBySixTable  ', CPSJNT(JSUPP(1,ISUP))
          ENDIF
      ENDDO
      WRITE(U,*)
      WRITE(U,'(A)')'###############################################'
C======================================================================  
C     Spring Table 6x6
      WRITE(U,'(A)') 'Table'
      WRITE(U,'(A)') 'SixBySixTable'
      WRITE(U,'(A)')'x       y       z       rx          ry          rz'
      WRITE(U,'(2A)') '[F L-1] [F L-1] [F L-1] [F L RAD-1] [F L RAD-1]',
     +              ' [F L RAD-1]'
      
      WRITE(U,960) spring(1:6)
      WRITE(U,960) spring(2),spring(7:11)
      WRITE(U,960) spring(3),spring(8),spring(12:15)
      
      WRITE(U,960) spring(4),spring(9),spring(13),spring(16:18)
      WRITE(U,960) spring(5),spring(10),spring(14),spring(17),
     +             spring(19:20)
      WRITE(U,960) spring(6),spring(11),spring(15),spring(18),
     +             spring(20),spring(21)
      WRITE(U,*)
      WRITE(U,'(A)')'###############################################'
C======================================================================      
      RETURN
C
 900  FORMAT (2X,A,2x,1P,E11.5,0P,2X,F4.2,2X,F14.7)
 910  FORMAT (A,2X, F6.3,2X, F5.3,2X,          A9,2X, F8.2, 4(2X,F5.3) )
 911  FORMAT (A,2X, F5.3,2X, F5.3,2X, F5.3,2X, A9,2X, F8.2, 7(2X,F5.3) )
 912  FORMAT (A,2X, F5.3,2X, F5.3,2X, E10.3, 4(2X,E14.6),
     +        F8.2, 7(2X,F5.3) )
 920  FORMAT (A8,3(2X,F11.6))    
 930  FORMAT (3(A8,2X),A14,2X,A1)
 940  FORMAT (A10,2(2X,A8))
 950  FORMAT (3A)
 960  FORMAT (6(E15.8,2X))
C
      END
