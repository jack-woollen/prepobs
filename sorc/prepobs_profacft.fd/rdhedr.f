      module  readac

      integer lunin,idate,idat(8),jdat(8),iret
      real(8) ACRN,RPID,QMAT,QMWN,TMDB,WDIR,WSPD
      real(8) prlc,acix,flvl,ialt,tcor,rsrd,rsrx
      real(8) moiq,rcts,heit,hmsl,flvlst,acns,psal
      real(8) mois,uwnd,vwnd,pcat,poaf,clon,clat
      real(8) prsx,elev,dhr,typ,t29,tsb,itp,rolfx
      real(8) year,mnth,days,hour,minu,seco,obtim
      real(8) rcyr,rcmn,rcdy,rchr,rcmi,rcse,rctim
      real(4) rinc(5)

      character(8) sid,subset
      equivalence (sid,rid)
      real(8) rid

      end module
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
      subroutine rddate
      use readac
      read(5,*)idate
      jdat(1)=idate/1000000
      jdat(2)=mod(idate/10000,100)
      jdat(3)=mod(idate/100,100)
      jdat(4)=0 
      jdat(5)=mod(idate/1,100)
      jdat(6)=0 
      jdat(7)=0 
      jdat(8)=0
      end subroutine
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
      subroutine rddump  

      use readac

      implicit none

      integer isb,ibfms
      real(4) HGTF_HI,HGTF_LO,AS,TFRMQP,ES,QFRMTP,PR,PRS,P,Q,T,Z,W
      real(4) QSPH,EMIX 
      real(8) getbmiss,bmiss

!-----------------------------------------------------------------------
!  FCNS HGTF_HI, HGTF_LO CALC. Z FROM P < 226.3MB AND P > 226.3MB; RESP
      HGTF_HI(P) = 11000 - ALOG(P/226.3)/1.576106E-4
      HGTF_LO(P) = (1.-(P/1013.25)**(1./5.256))*(288.15/.0065)

!  FCNS BELOW CONVERT SAT./SPEC. HUM.(KG/KG) & PRESS(MB) INTO TEMP/TD(K)
!     AS(Q,P) = ALOG((Q * P)/(6.1078 * ((0.378 * Q) + 0.622)))
!     TFRMQP(Q,P) = ((237.3 * AS(Q,P))/(17.269 - AS(Q,P)) + 273.16)

!  FCNS BELOW CONVERT TEMP/TD(K) & PRESS(MB) INTO SAT./SPEC. HUM.(KG/KG)
!     ES(T) = 6.1078 * EXP((17.269 * (T - 273.16))/((T - 273.16)+237.3))
!     QFRMTP(T,P) = (0.622 * ES(T))/(P - (0.378 * ES(T)))

!  Fcns below estimate pressure (mb) using indicated altitude (m) via
      PR(Z) = 1013.25 * (((288.15 - (.0065 * Z))/288.15)**5.256)
      PRS(Z) = 226.3 * EXP(1.576106E-4 * (11000. - Z))
!-----------------------------------------------------------------------

      bmiss=getbmiss()

!  read basic elements from the aircraft dumpfile
!  ----------------------------------------------

      call ufbint(lunin,RPID,1,1,iret,'RPID')
      call ufbint(lunin,ACIX,1,1,iret,'ACID')
      call ufbint(lunin,ACRN,1,1,iret,'ACRN')
      call ufbint(lunin,CLAT,1,1,iret,'CLAT')
      call ufbint(lunin,CLON,1,1,iret,'CLON')
      call ufbint(lunin,FLVL,1,1,iret,'FLVL')
      call ufbint(lunin,PRLC,1,1,iret,'PRLC')
      call ufbint(lunin,PSAL,1,1,iret,'PSAL')
      call ufbint(lunin,PCAT,1,1,iret,'PCAT')
      call ufbint(lunin,POAF,1,1,iret,'POAF')
      call ufbint(lunin,YEAR,1,1,iret,'YEAR')
      call ufbint(lunin,MNTH,1,1,iret,'MNTH')
      call ufbint(lunin,DAYS,1,1,iret,'DAYS')
      call ufbint(lunin,HOUR,1,1,iret,'HOUR')
      call ufbint(lunin,MINU,1,1,iret,'MINU')
      call ufbint(lunin,SECO,1,1,iret,'SECO')
      call ufbint(lunin,IALT,1,1,iret,'IALT')
      call ufbint(lunin,TCOR,1,1,iret,'TCOR')
      call ufbint(lunin,RCYR,1,1,iret,'RCYR')
      call ufbint(lunin,RCMN,1,1,iret,'RCMN')
      call ufbint(lunin,RCDY,1,1,iret,'RCDY')
      call ufbint(lunin,RCHR,1,1,iret,'RCHR')
      call ufbint(lunin,RCMI,1,1,iret,'RCMI')
      call ufbint(lunin,RCSE,1,1,iret,'RCSE')
      call ufbint(lunin,TMDB,1,1,iret,'TMDB')
      call ufbint(lunin,WDIR,1,1,iret,'WDIR')
      call ufbint(lunin,WSPD,1,1,iret,'WSPD')
      call ufbint(lunin,QMAT,1,1,iret,'QMAT')
      call ufbint(lunin,QMWN,1,1,iret,'QMWN')
      call ufbint(lunin,MOIS,1,1,iret,'MIXR')
      call ufbint(lunin,MOIQ,1,1,iret,'MSTQ')
      call ufbint(lunin,RSRD,1,1,iret,'RSRD')
      call ufbint(lunin,RSRX,1,1,iret,'EXPRSRD')

!     CLON CLAT ELEV DHR TYP T29 TSB ITP OBT RCT TCR RSRD RSRX    

!  TEMP TO CELSIUS, TEMP & WIND Q MARKS
!  ------------------------------------

      IF(IBFMS(TMDB)==0) then
         IF(IBFMS(QMAT)/=0) QMAT=2
         TMDB=TMDB-273.16
      ENDIF

      IF(IBFMS(MAX(WDIR,WSPD))==0) THEN
         CALL UV(WDIR,WSPD,UWND,VWND)
         IF(IBFMS(QMWN)/=0) QMWN=2
         WSPD=WSPD/.5144
      ELSE 
         WDIR=BMISS; WSPD=BMISS; QMWN=BMISS
      ENDIF
    
      T29 = 041 ! AIRCRAFT IS 0N29 TYPE 41

!  ASSIGN A REPORT TYPE EACH DUMPFILE SUBSET CATEGORY                          |
!  --------------------------------------------------

      READ(SUBSET,'(5x,I3)') ISB; TSB=ISB ! message subtype

      if(TSB==001) typ=30    ! MTYP 004-001  Manual AIREP & ADS (AIREP)
      if(TSB==002) typ=30    ! MTYP 004-002  Manual PIREP (PIREP)       
      if(TSB==003) typ=31    ! MTYP 004-003  Automated AMDAR (FM-42 AMDAR)
      if(TSB==004) typ=33    ! MTYP 004-004  Automated MDCRS (ARINC to NCEP) (BUFR)
      if(TSB==005) typ=32    ! MTYP 004-005  Flight level reconnaissance (RECCO)
      if(TSB==006) typ=31    ! MTYP 004-006  Automated European AMDAR (BUFR) 
      if(TSB==007) typ=33    ! MTYP 004-007  Auto MDCRS (ARINC to AFWA to NCEP)
      if(TSB==008) typ=34    ! MTYP 004-008  TAMDAR from MADIS (Mesaba) (NetCDF) 
      if(TSB==009) typ=35    ! MTYP 004-009  Automated Canadian AMDAR (BUFR)
      if(TSB==010) typ=34    ! MTYP 004-010  TAMDAR from Panasonic (BUFR)
      if(TSB==011) typ=31    ! MTYP 004-011  Automated Korean AMDAR (BUFR) 
      if(TSB==012) typ=34    ! MTYP 004-012  TAMDAR from MADIS (PenAir) (NetCDF)
      if(TSB==013) typ=34    ! MTYP 004-013  TAMDAR from MADIS (Chautauqua)(NetCDF)
      if(TSB==014) typ=31    ! MTYP 004-014  Automated French AMDAR (BUFR)
      if(TSB==015) typ=32    ! MTYP 004-015  High density recconnaissance obs (HDOB)
      if(TSB==210) typ=31    ! MTYP 004-103  All other automated AMDAR (BUFR)

!  IF LOW RES LAT/LON MISSING, REPORT LIKELY CONTAINS HI RES LAT/LON
!  -----------------------------------------------------------------
 
      CALL UFBINT(lunin,CLON,1,1,IRET,'CLON')
      CALL UFBINT(lunin,CLAT,1,1,IRET,'CLAT')

      IF(ibfms(CLON)/=0) CALL UFBINT(lunin,CLON,1,1,IRET,'CLONH')
      IF(ibfms(CLAT)/=0) CALL UFBINT(lunin,CLAT,1,1,IRET,'CLATH')

      if(clon<0.0) clon=clon+360.

      CALL UFBINT(lunin,HOUR,1,1,IRET,'HOUR')
      CALL UFBINT(lunin,MINU,1,1,IRET,'MINU')
      CALL UFBINT(lunin,SECO,1,1,IRET,'SECO')
      OBTIM=HOUR+MINU/60.+SECO/3600.

      CALL UFBINT(lunin,RCHR,1,1,IRET,'RCHR')
      CALL UFBINT(lunin,RCMI,1,1,IRET,'RCMI')
      CALL UFBINT(lunin,RCTS,1,1,IRET,'RCTS')
      RCTIM=RCHR+RCMI/60.+RCTS/3600.

!  TRY TO FIND THE FLIGHT LEVEL HEIGHT
!  -----------------------------------

      call ufbint(lunin,PSAL,1,1,iret,'PSAL')
      call ufbint(lunin,FLVL,1,1,iret,'FLVL')
      call ufbint(lunin,IALT,1,1,iret,'IALT')
      call ufbint(lunin,PRLC,1,1,iret,'PRLC')
      call ufbint(lunin,HEIT,1,1,iret,'HEIT')
      call ufbint(lunin,HMSL,1,1,iret,'HMSL')
      call ufbint(lunin,FLVLST,1,1,iret,'FLVLST')

      IF(PRLC.LT.BMISS)  THEN
         IF(PRLC.LT.22630) ELEV = HGTF_HI(PRLC*.01)
         IF(PRLC.GE.22630) ELEV = HGTF_LO(PRLC*.01)
      ELSEIF(IALT.LT.BMISS) THEN
         ELEV = IALT         
      ELSE
         ELEV = BMISS
      ENDIF

      IF(PSAL.LT.BMISS) THEN
         ELEV = PSAL + SIGN(0.0000001,PSAL)
      ELSEIF(FLVL.LT.BMISS)  THEN
         ELEV = FLVL + SIGN(0.0000001,FLVL)
!     ELSEIF(IALT.LT.BMISS.AND.PRLC.LT.BMISS)  THEN
!        ELEV = IALT  
      ELSEIF(HEIT.LT.BMISS)  THEN
         ELEV = HEIT + SIGN(0.0000001,HEIT)
      ELSEIF(HMSL.LT.BMISS)  THEN
         ELEV = HMSL + SIGN(0.0000001,HMSL)
      ELSEIF(FLVLST.LT.BMISS)  THEN
         ELEV = FLVLST + SIGN(0.0000001,FLVLST)
      END IF

!  CALCULATE PRESSURE IF PRLC IS MISSING
!  -------------------------------------

      IF(IBFMS(PRLC)==1) THEN
         IF(NINT(ELEV).LE.11000) PRSX = PR(ELEV)
         IF(NINT(ELEV).GT.11000) PRSX = PRS(ELEV)
      ELSE
         PRSX = PRLC*0.1
      END If

!  CALCULATE Q FROM MIXING RATIO
!  -----------------------------

      IF(IBFMS(MAX(MOIS,PRSX))==0) THEN
         P=PRSX; W=MOIS
         MOIS=QSPH(P,EMIX(P,W))*1.e6
         IF(IBFMS(MOIQ)==1) MOIQ=2 
         IF(MOIQ<=2) MOIQ=2
         IF(MOIQ>=3) MOIQ=9
      ELSE
         MOIS=BMISS
         MOIQ=BMISS
      ENDIF

!  ACFT NAVIGATION SYSTEM STORED IN INST TYPE LOCATION (AS WITH ON29)
!  ------------------------------------------------------------------

      ITP = 10e10
      CALL UFBINT(lunin,ACNS,1,1,IRET,'ACNS')
      IF(NINT(ACNS)==0)  ITP = 97 ! Inertial Navigation System
      IF(NINT(ACNS)==0)  ITP = 98 ! OMEGA

c Date/time for observations
c --------------------------

        idat(1) = year
        idat(2) = mnth
        idat(3) = days
        idat(4) = 0 ! time zone
        idat(5) = hour
        idat(6) = minu
        if(seco.gt.61) seco=0.0
        idat(7) = seco
        idat(8) = (seco-idat(7))*100.

c USE W3 ROUTINE W3DIFDAT TO GET OB-AN DIFFERENCE FOR PROFILES
c ------------------------------------------------------------

        call w3difdat(idat,jdat,2,rinc) ! diff in hours
        dhr = rinc(2)
       
      END SUBROUTINE
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
      SUBROUTINE UV(DD,FF,U,V)
      real(8) DD,FF,U,V
      DATA  CONV2R/0.017453293/,FACTOR/0.5148/

C IF WIND SPEED LESS THAN ZERO, WE HAVE A PROBLEM
      IF(FF.LE.0.0)  THEN
         U = 0.0
         V = 0.0
      ELSE
         U = -FF * SIN(DD*CONV2R)
         V = -FF * COS(DD*CONV2R)
      END IF
      RETURN

         ENTRY DF(U,V,DD,FF)
      IF(U.EQ.0.0)  THEN
         DD = 0.
         IF(V.GT.0.0)  DD = 180.
      ELSE
         IF(V.EQ.0.0)  THEN
            DD =  90.
            IF(U.GT.0.0)  DD = 270.
         ELSE
            DD = (ATAN2(U,V)/CONV2R) + 180.
            DD = MOD(DD,360.)
         END IF
      END IF
      FF = SQRT(U**2 + V**2)/FACTOR
      RETURN
      END
C-----------------------------------------------------------------------
C  MOIFUN IS A SET OF MOISTURE VARIABLE CONVERSION FUNCTIONS. THE
C  FUNCTION PACKAGE RELATES ATMOSPHERIC PRESSURE, TEMPERATURE, AND
C  WATER VAPOR PRESSURE, WITH THE MOISTURE VARIABLES OF RELATIVE
C  HUMIDITY, MIXING RATIO, SPECIFIC HUMIDITY, DEWPOINT, AND VIRTUAL
C  TEMPERATURE. VARIOUS COMPONENTS OF THE FUNCTIONS ARE GIVEN BELOW.
C
C
C  SYMBOLS (AND THEIR UNITS) USED IN THE FUNCTION ARGUMENTS ARE:
C
C     P  = ATMOSPHEREIC PRESSURE     (MB)
C     E  = WATER VAPOR PRESSURE      (MB)
C     T  = TEMPERATURE               (DEG C)
C     R  = RELATIVE HUMIDITY         (PERCENT)
C     W  = MIXING RATIO              (G/G)
C     Q  = SPECIFIC HUMIDITY         (G/G)
C     DP = TEMPERATURE               (DEG C)
C     ES = SATURATION VAPOR PRESSURE (MB)
C     WS = SATURATION MIXING RATION  (G/G)
C     TV = VIRTUAL TEMPERATURE       (DEG C)
C     TS = SENSIBLE TEMPERATURE      (DEG C)
C
C  PHYSICAL CONSTANTS USED ARE:
C
C     EZERO = WATER VAPOR ES @ 0 DEG C    (   6.11  MILIBARS        )
C     EVLAT = LATENT HEAT OF EVAPORATION  (  597.3  CAL/G           )
C     VMOLW = MOLECULAR WEIGHT OF WATER   ( 18.016  G/MOL           )
C     DMOLW = MOLECULAR WEIGHT OF DRY AIR ( 28.966  G/MOL           )
C     RSTAR = UNIVERSAL GAS CONSTANT      (   1.98  CAL/(MOL*DEG K) )
C     TZERO = 0 DEG C IN DEG K            ( 273.16  DEG K           )
C     EPSLN = VMOLW/DMOLW                 (   .622  NO UNITS        )
C
C  THE SATURATION WATER VAPOR PRESSURE(ES) IS COMPUTED AS A SOLUTION
C  OF THE CLAUSIUS-CLAPEYRON EQUATION (SEE HESS,"INTRODUCTION TO
C  THEORETICAL METEOROLOGY", PGS 48-49). THE SPECIFIC FORMULATION,
C
C  ES(T)=EZERO*EXP(17.269*T/(T+237.3)),
C
C  WHERE T IS DEGREES CELSIUS, IS A FORM OF TETENS' FORMULA FOR SATURATION
C  VAPOR PRESSURE. FOR MORE DETAILS SEE NMC OFFICE NOTE 36, OR
C  TETENS, O., 1930: OBER EINIGE METEOROLOGISCHE VEGRIFFE. Z. GEOPHYS.
C
C  THE DEFINITIONS OF MOISTURE VARIABLES ARE (FROM HESS,PGS 58-60):
C
C     RELATIVE HUMIDITY    R  = W/WS
C     MIXING RATIO         W  = EPSLN*E/(P-E)
C     SPECIFIC HUMIDITY    Q  = EPSLN*E/(P-E*(1-EPSLN))    (EXACT FORM)
C     DEW POINT            DP = TEMPERATURE WHERE WS(DP,P,E) = W(T,P,E)
C     VIRTUAL TEMPERATURE  VT = T*(1+W/EPSLN)/(1+W)
C     SENSIBLE TEMPERATURE TS = T*(1+W)/(1+W/EPSLN)
C
C  MOIFUN PROVIDES ENTRY POINTS WHICH COMPUTE MOISTURE VARIABLES FROM
C  THEIR COMPONENTS. ENTRIES ARE ALSO GIVEN WHICH CALCULATE E FROM
C  THE MOISTURE VARIABLES. THIS ENABLES CONVERSIONS BETWEEN MOISTURE
C  VARIABLES. THE FUNCTION ENTRY POINTS ARE:
C
C     1) ESVP(T     ) - SATURATION WATER VAPOR PRESSURE FROM T
C     2) ERLH(P,R,T ) - WATER VAPOR PRESSURE FROM P,R,T
C     3) EMIX(P,W   ) - WATER VAPOR PRESSURE FROM P,W
C     4) ESPH(P,Q   ) - WATER VAPOR PRESSURE FROM P,Q
C     5) EDEW(DP    ) - WATER VAPOR PRESSURE FROM DP
C     6) RELH(P,E,T ) - RELATIVE HUMIDITY FROM P,E,T
C     7) WMIX(P,E   ) - MIXING RATIO FROM P,E
C     8) QSPH(P,E   ) - SPECIFIC HUMIDITY FROM P,E
C     9) DPAL(E     ) - DEW POINT FROM E   (ALGEBRAIC)
C    10) VIRT(P,E,TS) - VIRTUAL  TEMPERATURE FROM P,E,TS
C    11) SENT(P,E,TV) - SENSIBLE TEMPERATURE FROM P,E,TV
C
C  THE MAIN ENTRY POINT (MOIFUN) IS A DUMMY FUNCTION.
C
C     CALLING MOIFUN RESULTS IN AN ABORT.
C
C-----------------------------------------------------------------------
      FUNCTION MOIFUN(DUMMY)
 
      PARAMETER (RSTAR = 1.98        )
      PARAMETER (TZERO = 273.16      )
      PARAMETER (EVLAT = 597.3       )
      PARAMETER (VMOLW = 18.016      )
      PARAMETER (DMOLW = 28.966      )
      PARAMETER (EZERO = 6.11        )
      PARAMETER (EPSLN = VMOLW/DMOLW )
 
C-----------------------------------------------------------------------
      ES(T) = EZERO*EXP(17.269*T/(T+237.3))
C-----------------------------------------------------------------------
 
C  MAIN ENTRY POINT DOES NOTHING
C  -----------------------------
 
      moifun=0; RETURN
 
C  SATURATION VAPOR PRESSURE FROM T
C  --------------------------------
 
      ENTRY ESVP(T)
      ESVP = ES(T)
      RETURN
 
C  VAPOR PRESSURE FROM P,R,T
C  -------------------------
 
      ENTRY ERLH(P,R,T)
      RH   = .01*R
      ERLH = P/(1.+P/(RH*ES(T))-1./RH)
      RETURN
 
C  VAPOR PRESSURE FROM P,W
C  -----------------------
 
      ENTRY EMIX(P,W)
      EMIX = W*P/(EPSLN+W)
      RETURN
 
C  VAPOR PRESSURE FROM P,Q
C  -----------------------
 
      ENTRY ESPH(P,Q)
C     ESPH = Q*P/(1.   +Q*(1.-EPSLN))
      ESPH = Q*P/(EPSLN+Q*(1.-EPSLN))
      RETURN
 
C  VAPOR PRESSURE FROM DP
C  ----------------------
 
      ENTRY EDEW(DP)
      EDEW = ES(DP)
      RETURN
 
C  RELATIVE HUMIDITY FROM P,E,T
C  ----------------------------
 
      ENTRY RELH(P,E,T)
      EST  = ES(T)
      RELH = (E/(P-E))/(EST/(P-EST))
      RETURN
 
C  MIXING RATIO FROM P,E
C  ---------------------
 
      ENTRY WMIX(P,E)
      WMIX = EPSLN*E/(P-E)
      RETURN
 
C  SPECIFIC HUMIDITY FROM P,E
C  --------------------------
 
      ENTRY QSPH(P,E)
      QSPH = EPSLN*E/(P-E*(1.-EPSLN))
      RETURN
 
C  DEW POINT FROM ALGEBRAIC MANIPULATION OF ES FUNCTION GIVEN E
C  ------------------------------------------------------------
 
      ENTRY DPAL(E)
      XLNE = LOG(E/EZERO)
      DPAL = 237.3*XLNE/(17.269-XLNE)
      RETURN
 
C  VIRTUAL TEMPERATURE FROM P,E,TS
C  -------------------------------
 
      ENTRY VIRT(P,E,TS)
      RMIX = EPSLN*E/(P-E)
      VIRT = (TS+TZERO)*(1.+RMIX/EPSLN)/(1.+RMIX) - TZERO
      RETURN
 
C  SENSIBLE TEMPERATURE FROM P,E,TV
C  --------------------------------
 
      ENTRY SENT(P,E,TV)
      RMIX = EPSLN*E/(P-E)
      SENT = (TV+TZERO)*(1.+RMIX)/(1.+RMIX/EPSLN) - TZERO
      RETURN
 
      END
