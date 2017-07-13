C Version 21-Diciembre-2001
C Programa para combinar imagenes infrarrojas
C
        PROGRAM IMCOMBINE
        IMPLICIT NONE
C
        INCLUDE 'dimensions.inc'
        INCLUDE 'imcombine_dir.inc'
        INCLUDE 'sexpath.inc'
C
        REAL PI
        PARAMETER (PI=3.141592654)
C
        INTEGER SYSTEMFUNCTION
        INTEGER READILIM
        INTEGER TRUEBEG,TRUELEN
        REAL READF
        REAL FMEAN0
        REAL FMEDIAN1
        REAL FMEDIAN1MAD
        REAL RANDOMNUMBER
        REAL LEECOLUMN
        DOUBLE PRECISION FFJ
        CHARACTER*255 READC
C
        INTEGER I,I1,I2
        INTEGER J
        INTEGER NL
        INTEGER K,K1,K2,K_
        INTEGER L,L1,L2,LL1,LL2,LLL1,LLL2,LLLL1,LLLL2
        INTEGER II,JJ,KK
        INTEGER NPIX
        INTEGER IDUM
        INTEGER NP
        INTEGER YYYY,MM,DD,SS !Para calculo de fecha juliana
        INTEGER HOUR,MINUTE !Para calculo de fecha juliana con EMIR
        INTEGER ID_CLOSEST(NMAXFRAMES,NMAXFRAMES),ID_FJ(NMAXFRAMES)
        INTEGER II_FRAME(NMAXFRAMES),JJ_FRAME(NMAXFRAMES)
        INTEGER I1F(NMAXFRAMES),I2F(NMAXFRAMES)
        INTEGER J1F(NMAXFRAMES),J2F(NMAXFRAMES)
        INTEGER NF,NF_LAST,NF_,NF1,NF2
        INTEGER NFRAMES
        INTEGER IREADWRITE,BLOCKSIZE,ISTATUS,BITPIX
        INTEGER IUT(NMAXFRAMES)
        INTEGER NAXISF(0:2,NMAXFRAMES),NAXISF1,NAXISF2,NFOUND
        INTEGER NAXIS(2)
        INTEGER FIRSTPIX
        INTEGER IXMIN,IXMAX,IYMIN,IYMAX
        INTEGER OFFX(NMAXFRAMES),OFFY(NMAXFRAMES)
        INTEGER NX1,NX2,NY1,NY2
        INTEGER NBUNDLE
        INTEGER NXX,NXX_EFF
        INTEGER IFRACTION_REMOVE
        INTEGER ISYSTEM
        INTEGER FRAME_IOK(NMAXFRAMES)
        INTEGER NPIXPERPIX(NMAXFRAMES)
        INTEGER ISUBMENU
        INTEGER FRAME_NPIXELS(NXMAXF,NYMAXF)
        INTEGER MIN_FRAME_NPIXELS,MAX_FRAME_NPIXELS
        INTEGER NSEED
        INTEGER NBOXFILTX,NBOXFILTY,NPIXMIN
        INTEGER IREG,NREG,NX1Q,NX2Q,NY1Q,NY2Q
        INTEGER N_X_IMAGE,N_Y_IMAGE,N_A_IMAGE,N_B_IMAGE,N_THETA_IMAGE
        REAL SECOND !para calculo fecha juliana con EMIR
        REAL AIRMASS(NMAXFRAMES)
        REAL K_EXTINCTION,FACTOR_EXTINCTION
        REAL PIXEL(NXMAXF*NYMAXF)
        REAL PIXEL_FMEAN,PIXEL_FSIGMA
        REAL FRAME_DARK(NXMAXF,NYMAXF)
        REAL FRAME_FLAT(NXMAXF,NYMAXF)
        REAL FMEDIAN_RAW(NMAXFRAMES)
        REAL FMEDIAN_RAW_REG(NMAXREG)
        REAL FMEDIAN_SKY(NMAXFRAMES)
        REAL FMEDIAN_SKY_REG(NMAXREG)
        REAL FRAME_SUMFLAT(NXMAXF,NYMAXF)
        REAL FRAME_DATA(NXMAXF,NYMAXF)
        REAL FROW(NXMAX)
        REAL IMAGEN_FINAL(NXMAX,NYMAX)
        REAL IMAGEN_FINAL_RMS(NXMAX,NYMAX)
        REAL IMAGEN_FINAL_NPIXELS(NXMAX,NYMAX)
        REAL STACK_DATA(NXMAX,NMAXFRAMES)
        REAL TR(6),BG,FG
        REAL XMIN,XMAX,YMIN,YMAX,DX,DY
        REAL FX1,FX2,FY1,FY2
        REAL XX(NMAXFRAMES),XX_EFF(NMAXFRAMES)
        REAL XX_MEAN,XX_MEDIAN,XX_SIGMA
        REAL FRACTION_REMOVE,TSIGMA_REMOVE
        REAL SONDEO4C_FMEAN,SONDEO4C_FSIGMA
        REAL XP(NMAXFRAMES),YP(NMAXFRAMES)
        REAL YP1(NMAXFRAMES),YP2(NMAXFRAMES),YP3(NMAXFRAMES)
        REAL MEANPERPIX(NMAXFRAMES),SIGMAPERPIX(NMAXFRAMES)
        REAL RMS_1FRAME,MEAN_ALLFRAME
        REAL R1,R2
        REAL TIMES1_BGFG,TIMES2_BGFG
        REAL DFJ(NMAXFRAMES)
        REAL FACTOR_DARK
        REAL FINCREASE
        REAL EXPTIME
        REAL EXPTIME_CHECK(NMAXFRAMES)
        REAL EXPTIME_DARK(NMAXFRAMES)
        REAL EXPTIME_DATA(NMAXFRAMES)
        REAL X_IMAGE,Y_IMAGE,A_IMAGE,B_IMAGE,THETA_IMAGE
        REAL THETA0,R0,X0,Y0,XELL(361),YELL(361)
        DOUBLE PRECISION FJ(NMAXFRAMES)
        DOUBLE PRECISION DSUM1,DSUM1_,DSUM2
        CHARACTER*1 CPFLAT,CMENU,CPAUSE,CSAVE,CREPEAT
        CHARACTER*1 CBGFG_MODE,CASK
        CHARACTER*1 CSKY,CFILTER,COPC,CMERGE
        CHARACTER*1 COBJECT_MASK,COMPARE_MASK
        CHARACTER*1 CRUN_THROUGH
        CHARACTER*1 CBORDER
        CHARACTER*2 CSUFFIX,CSUFFIX_OLD
        CHARACTER*50 CDATE(NMAXFRAMES),COMMENT,CDUMMY
        CHARACTER*50 TELESCOPE,INSTRUMENT
        CHARACTER*255 TTER
        CHARACTER*1000 CLINEA
        CHARACTER*255 FILE_LIST_FRAMES
        CHARACTER*255 FILE_DATA,FILE_RMS,FILE_NUMBER_PIXELS
        CHARACTER*255 FILE_DARK_FRAME(NMAXFRAMES)
        CHARACTER*255 FILE_FRAME(NMAXFRAMES)
        CHARACTER*255 FILE_SUPERFLAT
        CHARACTER*255 FILE_SUPERFLAT_NPIX
        CHARACTER*255 FILE_FRAME_NOR(NMAXFRAMES)
        CHARACTER*255 FILE_FRAME_SKYFIT(NMAXFRAMES)
        CHARACTER*255 FILE_FRAME_SKYFIT_OLD(NMAXFRAMES)
        CHARACTER*255 FILE_FRAME_SKYSUB(NMAXFRAMES)
        CHARACTER*255 FILE_FRAME_AFTER_FLAT(NMAXFRAMES)
        CHARACTER*255 FILE_MASK_FRAME
        CHARACTER*255 FILE_MASK_IMAGEN
        CHARACTER*255 FILE_MASK_IMAGEN_CAT
        CHARACTER*255 FILE_MEDIAN_SKY
        CHARACTER*255 FILE_RMS_NUMBER
        CHARACTER*255 FILE_IMAGE_FAKE_ERROR
        CHARACTER*255 FILELABEL
        CHARACTER*255 IMCOMBINE_DIR_,SEXPATH_
        LOGICAL LECHO
        LOGICAL SIMPLE,EXTEND
        LOGICAL MASK_FRAME(NXMAXF,NYMAXF)
        LOGICAL MASK_FRAME_EFF(NXMAXF,NYMAXF)
        LOGICAL MASK_IMAGEN_FINAL(NXMAX,NYMAX)
        LOGICAL LROW(NXMAX)
        LOGICAL STACK_IFPIXEL(NXMAX,NMAXFRAMES)
        LOGICAL IFROW(NYMAX)
        LOGICAL LOGFILE
        LOGICAL ANYNULL
        LOGICAL LOOP
        LOGICAL LANY,LEQUAL_NAXIS
        LOGICAL LFIRSTPLOT
        LOGICAL LOK
        LOGICAL LOVER_CDMASK,LOVER_OBMASK
C
        COMMON/BLKLECHO/LECHO !for iofunctions.f
        COMMON/BLK_MASK_FRAME/MASK_FRAME
C------------------------------------------------------------------------------
        FILE_LIST_FRAMES='@'
        FILE_DATA='@'
        FILE_RMS='@'
        FILE_NUMBER_PIXELS='@'
        FILE_RMS_NUMBER='@'
        FILE_MASK_IMAGEN='@'
        FILE_MASK_IMAGEN_CAT='@'
        FILE_IMAGE_FAKE_ERROR='@'
        COBJECT_MASK='n'
        CSAVE='n'
        CREPEAT='n'
        CPAUSE='y'
        COMPARE_MASK='n'
        CRUN_THROUGH='n'
        CASK='n'
        CSUFFIX='i1'
C
        BG=0.0
        FG=1.0
        TIMES1_BGFG=8.0
        TIMES2_BGFG=8.0
        TR(1)=0.
        TR(2)=1.
        TR(3)=0.
        TR(4)=0.
        TR(5)=0.
        TR(6)=1.
        LOVER_CDMASK=.FALSE.
        LOVER_OBMASK=.FALSE.
C
        FRACTION_REMOVE=0.1 !10%
        TSIGMA_REMOVE=3.0
        NBOXFILTX=11
        NBOXFILTY=11
        IMCOMBINE_DIR_=IMCOMBINE_DIR
        SEXPATH_=SEXPATH
C------------------------------------------------------------------------------
c abrimos la salida grafica
        TTER=READC('Graphics device/type (? to see list)','/XSERVE','@')
        IF(TTER(1:1).EQ.'@')THEN
          TTER=TTER(2:)
          LECHO=.TRUE.
          WRITE(*,101) TTER(1:TRUELEN(TTER))
        ELSE
          LECHO=.FALSE.
        END IF
        CALL PGBEGIN(0,TTER,1,1)
        CALL PGASK(.FALSE.)
        CALL PGSCF(2)
C------------------------------------------------------------------------------
C pedimos fichero con lista de frames, darks, masks y offsets
        LOOP=.TRUE.
        DO WHILE(LOOP)
          FILE_LIST_FRAMES=READC('File with list of frames',
     +     FILE_LIST_FRAMES,'@')
          INQUIRE(FILE=FILE_LIST_FRAMES,EXIST=LOGFILE)
          IF(LOGFILE)THEN
            LOOP=.FALSE.
          ELSE
            WRITE(*,101) 'ERROR: this file does not exist. Try again.'
            WRITE(*,100) 'Press <CR> to continue...'
            READ(*,*)
          END IF
        END DO
C leemos nombre de todas las imagenes, darks, masks y offsets
        OPEN(10,FILE=FILE_LIST_FRAMES,STATUS='OLD',FORM='FORMATTED')
        I=0
10      READ(10,101,END=11) CLINEA
        I=I+1
c frame_iok
        L1=TRUEBEG(CLINEA)
        L2=TRUELEN(CLINEA)
        L=(L1-1)+INDEX(CLINEA(L1:L2),' ')
        READ(CLINEA(L1:),*) FRAME_IOK(I)
        CLINEA=CLINEA(L+1:)
c frame
        L1=TRUEBEG(CLINEA)
        L2=TRUELEN(CLINEA)
        L=(L1-1)+INDEX(CLINEA(L1:L2),' ')
        FILE_FRAME(I)=CLINEA(L1:L-1)
        CLINEA=CLINEA(L+1:)
c dark
        L1=TRUEBEG(CLINEA)
        L2=TRUELEN(CLINEA)
        L=(L1-1)+INDEX(CLINEA(L1:L2),' ')
        FILE_DARK_FRAME(I)=CLINEA(L1:L-1)
        CLINEA=CLINEA(L+1:)
c offsets
        L1=TRUEBEG(CLINEA)
        READ(CLINEA(L1:),*) OFFX(I),OFFY(I)
!       OFFX(I)=-OFFX(I)
!       OFFY(I)=-OFFY(I)
        GOTO 10
11      CLOSE(10)
        NFRAMES=I
        WRITE(*,100) '>>> Total number of frames: '
        WRITE(*,*) NFRAMES
        IF(NFRAMES.LT.2)THEN
          WRITE(*,100) 'ERROR: number of frames = '
          WRITE(*,*) NFRAMES
          CALL PGEND
          STOP
        END IF
C------------------------------------------------------------------------------
C pedimos la mascara de defectos cosmeticos de los frames (usamos la misma
C mascara para todos los frames)
        LOOP=.TRUE.
        DO WHILE(LOOP)
          FILE_MASK_FRAME=READC('Cosmetic defect mask for frames '//
     +     '(none=no mask)','none','@')
          IF(FILE_MASK_FRAME.EQ.'none')THEN
            LOOP=.FALSE.
          ELSE
            INQUIRE(FILE=FILE_MASK_FRAME,EXIST=LOGFILE)
            IF(LOGFILE)THEN
              LOOP=.FALSE.
            ELSE
              WRITE(*,101) 'ERROR: this file does not exist. Try again.'
              WRITE(*,100) 'Press <CR> to continue...'
              READ(*,*)
              IF(LECHO) WRITE(*,*)
            END IF
          END IF
        END DO
        WRITE(*,100) 'Computing cosmetic defects mask...'
        CALL MASKFRAME(FILE_MASK_FRAME)
        WRITE(*,101) '...OK!'
C------------------------------------------------------------------------------
C chequeamos que los frames existen
        WRITE(*,100) 'Checking frames...'
        DO I=1,NFRAMES
          INQUIRE(FILE=FILE_FRAME(I),EXIST=LOGFILE)
          IF(.NOT.LOGFILE)THEN
            L=TRUELEN(FILE_FRAME(I))
            WRITE(*,100) 'FRAME#'
            WRITE(*,*) I
            WRITE(*,100) 'ERROR: The frame file '
            WRITE(*,100) FILE_FRAME(I)(1:L)
            WRITE(*,101) ' does not exist!'
            CALL PGEND
            STOP
          END IF
        END DO
        WRITE(*,101) '...OK!'
C------------------------------------------------------------------------------
C chequeamos que los darks existen
        DO I=1,NFRAMES
          IF(FILE_DARK_FRAME(I).NE.'none')THEN
            INQUIRE(FILE=FILE_DARK_FRAME(I),EXIST=LOGFILE)
            IF(.NOT.LOGFILE)THEN
              L=TRUELEN(FILE_DARK_FRAME(I))
              WRITE(*,100) 'FRAME#'
              WRITE(*,*) I
              WRITE(*,100) 'ERROR: The dark file '
              WRITE(*,100) FILE_DARK_FRAME(I)(1:L)
              WRITE(*,101) ' does not exist!'
              CALL PGEND
              STOP
            END IF
          END IF
        END DO
C------------------------------------------------------------------------------
C leemos las dimensiones, fecha, tiempo universal, masa de aire y tiempo de
C exposición de todos los frames
        WRITE(*,100) 'Reading frame dimensions, date,'//
     +   ' UT, airmass...'
        ISTATUS=0
        IREADWRITE=0
        DO NF=1,NFRAMES
          CALL FTOPEN(80,FILE_FRAME(NF),IREADWRITE,BLOCKSIZE,ISTATUS)
          CALL FTGKYJ(80,'NAXIS',NAXISF(0,NF),COMMENT,ISTATUS)
          IF(NAXISF(0,NF).NE.2)THEN
            CALL FTCLOS(80,ISTATUS)
            WRITE(*,100) 'ERROR: in image #'
            WRITE(*,*) NF
            WRITE(*,100) '=> NAXIS='
            WRITE(*,*) NAXISF(0,NF)
            CALL PGEND
            STOP
          END IF
          CALL FTGKNJ(80,'NAXIS',1,2,NAXISF(1,NF),NFOUND,ISTATUS)
          !leemos fecha y hora de la observacion
          CALL FTGKYS(80,'TELESCOP',TELESCOPE,COMMENT,ISTATUS)
          L1=TRUEBEG(TELESCOPE)
          L2=TRUELEN(TELESCOPE)
          TELESCOPE=TELESCOPE(L1:L2)
          IF(TELESCOPE(1:2).EQ.'WH') THEN
             CALL FTGKYD(80,'JD',FJ(NF),COMMENT,ISTATUS)
          ELSE IF(TELESCOPE(1:2).EQ.'CA') THEN
             CALL FTGKYS(80,'DATE',CDATE(NF),COMMENT,ISTATUS)
             CALL FTGKYJ(80,'UT',IUT(NF),COMMENT,ISTATUS)
             CALL FTGKYE(80,'EXPTIME',EXPTIME_CHECK(NF),COMMENT,ISTATUS)
             !calculamos la fecha juliana
             READ(CDATE(NF)(7:8),*) YYYY
             YYYY=YYYY+2000
             READ(CDATE(NF)(4:5),*) MM
             READ(CDATE(NF)(1:2),*) DD
             SS=IUT(NF)
             FJ(NF)=FFJ(YYYY,MM,DD,SS)
          ELSE IF(TELESCOPE(1:3).EQ.'GTC') THEN
             CALL FTGKYS(80,'INSTRUME',INSTRUMENT,COMMENT,ISTATUS)
             IF(INSTRUMENT(1:5).EQ.'CIRCE')THEN
               CALL FTGKYD(80,'JD',FJ(NF),COMMENT,ISTATUS)
               CALL FTGKYE(80,'EXPTIME',EXPTIME_CHECK(NF),COMMENT,
     +          ISTATUS)
             ELSE IF(INSTRUMENT(1:4).EQ.'EMIR')THEN
               CALL FTGKYS(80,'DATE-OBS',CDATE(NF),COMMENT,ISTATUS)
               CALL FTGKYE(80,'EXPTIME',EXPTIME_CHECK(NF),COMMENT,
     +          ISTATUS)
               !calculamos fecha juliana
               READ(CDATE(NF)(1:4),*) YYYY
               READ(CDATE(NF)(6:7),*) MM
               READ(CDATE(NF)(9:10),*) DD
               READ(CDATE(NF)(12:13),*) HOUR
               READ(CDATE(NF)(15:16),*) MINUTE
               READ(CDATE(NF)(18:22),*) SECOND
               SS=HOUR*3600+MINUTE*60+INT(SECOND+0.5)
               FJ(NF)=FFJ(YYYY,MM,DD,SS)
             ELSE
               WRITE(*,*)
               WRITE(*,*) INSTRUMENT
               WRITE(*,*) 'Name of instrument not recognized'
               STOP
             END IF
          ELSE
             WRITE(*,*) 'Name of telescope not recognized'
             STOP
          ENDIF
          !leemos masa de aire
          CALL FTGKYE(80,'AIRMASS',AIRMASS(NF),COMMENT,ISTATUS)
          CALL FTCLOS(80,ISTATUS)
          IF(ISTATUS.GT.0) CALL PRINTERROR(ISTATUS)
        END DO
        WRITE(*,101) 'OK!'
C------------------------------------------------------------------------------
C comprobamos que el tiempo de integración de todas las imágenes es el
C mismo
        DO I=2,NFRAMES
          IF(EXPTIME_CHECK(I).NE.EXPTIME_CHECK(1))THEN
            CALL PGEND
            WRITE(*,101) 'FATAL ERROR: different EXPTIME found'
            WRITE(*,100) 'EXPTIME (frame #001): '
            WRITE(*,*) EXPTIME_CHECK(1)
            WRITE(*,100) 'EXPTIME (frame #'
            WRITE(*,'(I3.3,$)') NF
            WRITE(*,100) '): '
            WRITE(*,*) EXPTIME_CHECK(NF)
            STOP
          END IF
        END DO
        WRITE(*,100) '>>> EXPTIME: '
        WRITE(*,*) EXPTIME_CHECK(1)
C------------------------------------------------------------------------------
C calculamos, para cada frame, una lista ordenada de indices que nos indica
C cuales son los frames que se encuentran mas cerca en el tiempo
        WRITE(*,100) 'Finding closests frames '//
     +   '(see file closests_frames.dat)...'
        OPEN(35,FILE='closests_frames.dat',STATUS='UNKNOWN',
     +   FORM='FORMATTED')
        DO NF=1,NFRAMES
          DO NF_=1,NFRAMES
            ID_FJ(NF_)=NF_
            DFJ(NF_)=REAL(DABS(FJ(NF)-FJ(NF_))*86400.0D0)
          END DO
          CALL ORDENA1F1I(NFRAMES,DFJ,ID_FJ)
          WRITE(35,'(A7,I3,A2,$)') 'Frame #',NF,': '
          DO NF_=1,NFRAMES
            ID_CLOSEST(NF_,NF)=ID_FJ(NF_)
            WRITE(CDUMMY,*) ID_FJ(NF_)
            CALL RMBLANK(CDUMMY,CDUMMY,L)
            IF(NF_.GT.1) WRITE(35,100) ' '
            WRITE(35,100) CDUMMY(1:L)
          END DO
          WRITE(35,*)
        END DO
        CLOSE(35)
        WRITE(*,101) '..OK!'
C------------------------------------------------------------------------------
C comprobamos si todos los frames tienen las mismas dimensiones
        NAXISF1=NAXISF(1,1)
        NAXISF2=NAXISF(2,1)
        LEQUAL_NAXIS=.TRUE.
        DO NF=2,NFRAMES
          IF(NAXISF1.NE.NAXISF(1,NF)) LEQUAL_NAXIS=.FALSE.
          IF(NAXISF2.NE.NAXISF(2,NF)) LEQUAL_NAXIS=.FALSE.
        END DO
        IF(LEQUAL_NAXIS)THEN
          WRITE(*,100) '>>> All the frames have the same dimensions: '
          WRITE(*,*) NAXISF1,NAXISF2
        ELSE
          WRITE(*,100) 'FATAL ERROR: All the frames do not '//
     +     'have the same dimensions.'
          CALL PGEND
          STOP
        END IF
C------------------------------------------------------------------------------
C determinamos dimension final de la imagen compuesta
        IXMIN=1+OFFX(1)
        IXMAX=NAXISF1+OFFX(1)
        IYMIN=1+OFFY(1)
        IYMAX=NAXISF2+OFFY(1)
        DO NF=2,NFRAMES
          IXMIN=MIN0(IXMIN,1+OFFX(NF))
          IXMAX=MAX0(IXMAX,NAXISF1+OFFX(NF))
          IYMIN=MIN0(IYMIN,1+OFFY(NF))
          IYMAX=MAX0(IYMAX,NAXISF2+OFFY(NF))
        END DO
        WRITE(*,100) 'Initial combined image dimensions in X: '
        WRITE(*,*) IXMIN,IXMAX
        WRITE(*,100) 'Initial combined image dimensions in Y: '
        WRITE(*,*) IYMIN,IYMAX
        NAXIS(1)=IXMAX-IXMIN+1
        NAXIS(2)=IYMAX-IYMIN+1
        WRITE(*,100) '>>> Maximum combined image size: '
        WRITE(*,*) NXMAX,NYMAX
        WRITE(*,100) '>>> Actual combined image NAXIS: '
        WRITE(*,*) NAXIS(1),NAXIS(2)
        IF((NAXIS(1).GT.NXMAX).OR.(NAXIS(2).GT.NYMAX))THEN
          WRITE(*,101) 'ERROR: redim NXMAX and/or NYMAX'
          CALL PGEND
          STOP
        END IF
C calculamos offsets respecto al origen para cada frame
        DO NF=1,NFRAMES
          JJ_FRAME(NF)=-IXMIN+1+OFFX(NF)
          II_FRAME(NF)=-IYMIN+1+OFFY(NF)
        END DO
C calculamos limites de cada frame en el sistema de referencia de la imagen
C final combinada
        DO NF=1,NFRAMES
          J1F(NF)=1+JJ_FRAME(NF)
          J2F(NF)=NAXISF1+JJ_FRAME(NF)
          I1F(NF)=1+II_FRAME(NF)
          I2F(NF)=NAXISF2+II_FRAME(NF)
        END DO
C------------------------------------------------------------------------------
C preguntamos si vamos a ejecutar todos los pasos del programa de forma
C consecutiva
        CRUN_THROUGH(1:1)=
     +   READC('Are you running the program linearly (y/n)',
     +   CRUN_THROUGH,'@')
C------------------------------------------------------------------------------
C inicializamos primera y ultima imagen disponibles
        NF1=1
        NF2=NFRAMES
C------------------------------------------------------------------------------
C******************************************************************************
C------------------------------------------------------------------------------
40      WRITE(*,*)
        WRITE(*,101) '***********************************************'//
     +               '********'
        WRITE(*,101) '(p) plot shifted individual frames'
        WRITE(*,101) '-----------------------------------------------'//
     +               '--------'
        WRITE(*,101) '(1) normalize individual (raw-dark) frames'
        WRITE(*,101) '(2) compute superflat'
        WRITE(*,101) '(3) compute individual (raw-dark)/superflat'
        WRITE(*,101) '(4) sky subtraction in (raw-dark)/superflat'
        WRITE(*,101) '(5) combine sky-subtracted (raw-dark)/superflat'//
     +               ' frames'
        WRITE(*,101) '(6) compute r.m.s. versus number of frames'
        WRITE(*,100) '(7) read r.m.s. vs number of frames file,'
        WRITE(*,101) ' create:'
        WRITE(*,101) '    - fake noise image'
        WRITE(*,101) '    - mask of objects'
        WRITE(*,101) '(0) QUIT'
        WRITE(*,101) '***********************************************'//
     +               '********'
        CMENU(1:1)=READC('Option (p/1...7/0)','0','pP01234567')
        IF(CMENU.EQ.'0')THEN
          CALL PGEND
          STOP
        END IF
C..............................................................................
C generamos el nombre de los ficheros con los flatfields y los frames tras el 
C flatfielding
        LOOP=.TRUE.
        DO WHILE(LOOP)
          CSUFFIX(1:2)=READC('Two-character suffix to identify '//
     +     'current iteration',CSUFFIX,'@')
          IF(TRUELEN(CSUFFIX).NE.2)THEN
            WRITE(*,101) 'ERROR: the suffix must have 2 non-blank'//
     +       ' characters. Try again.'
            WRITE(*,100) 'Press <CR> to continue...'
            READ(*,*)
          ELSE
            LOOP=.FALSE.
          END IF
        END DO
        FILE_SUPERFLAT='superflat_'//CSUFFIX//'.fits'
        FILE_SUPERFLAT_NPIX='superflat_npix_'//CSUFFIX//'.fits'
        DO NF=1,NFRAMES
          L2=TRUELEN(FILE_FRAME(NF))
          L1=L2+1
          LOOP=.TRUE.
          DO WHILE(LOOP)
            L1=L1-1
            IF(FILE_FRAME(NF)(L1:L1).EQ.'/')THEN
              L1=L1+1
              LOOP=.FALSE.
            END IF
            IF(LOOP)THEN
              IF(L1.EQ.1) LOOP=.FALSE.
            END IF
          END DO
          FILE_FRAME_NOR(NF)=FILE_FRAME(NF)(L1:L2-5)//
     +     '_nor_'//CSUFFIX//'.fits'
          FILE_FRAME_SKYFIT(NF)=FILE_FRAME(NF)(L1:L2-5)//
     +     '_skyfit_'//CSUFFIX//'.fits'
          FILE_FRAME_SKYSUB(NF)=FILE_FRAME(NF)(L1:L2-5)//
     +     '_skysub_'//CSUFFIX//'.fits'
          FILE_FRAME_AFTER_FLAT(NF)=FILE_FRAME(NF)(L1:L2-5)//
     +     '_aflat_'//CSUFFIX//'.fits'
        END DO
        FILE_MEDIAN_SKY='median_aflat_'//CSUFFIX//'.dat'
        FILE_RMS_NUMBER='skyrms_'//CSUFFIX//'.dat'
C
C..............................................................................
C leemos mascara de objetos o, si no existe, la inicializamos (notar que
C empleamos la variable IMAGEN_FINAL para almacenar la imagen FITS temporal
C que necesitamos para calcular la mascara)
        COBJECT_MASK(1:1)=READC('Are you using objects mask',
     +   COBJECT_MASK,'yn')
        IF(COBJECT_MASK.EQ.'y')THEN
          LOOP=.TRUE.
          DO WHILE(LOOP)
            FILE_MASK_IMAGEN=
     +       READC('Input file name for mask in combined image',
     +       FILE_MASK_IMAGEN,'@')
            INQUIRE(FILE=FILE_MASK_IMAGEN,EXIST=LOGFILE)
            IF(LOGFILE)THEN
              LOOP=.FALSE.
            ELSE
              WRITE(*,101) 'ERROR: this file does not exist. Try again.'
              WRITE(*,100) 'Press <CR> to continue...'
              READ(*,*)
            END IF
          END DO
          CALL READFITS(FILE_MASK_IMAGEN,NXMAX,NYMAX,NAXIS(1),NAXIS(2),
     +     IMAGEN_FINAL,.TRUE.,EXPTIME)
          DO I=1,NAXIS(2)
            DO J=1,NAXIS(1)
              MASK_IMAGEN_FINAL(J,I)=(IMAGEN_FINAL(J,I).NE.0.0)
            END DO
          END DO
        ELSE
          DO I=1,NAXIS(2)
            DO J=1,NAXIS(1)
              MASK_IMAGEN_FINAL(J,I)=.FALSE.
            END DO
          END DO
        END IF
C..............................................................................
        IF((CMENU.EQ.'p').OR.(CMENU.EQ.'P'))THEN
          GOTO 45
        ELSEIF(CMENU.EQ.'1')THEN
          GOTO 50
        ELSEIF(CMENU.EQ.'2')THEN
          GOTO 52
        ELSEIF(CMENU.EQ.'3')THEN
          GOTO 54
        ELSEIF(CMENU.EQ.'4')THEN
          GOTO 56
        ELSEIF(CMENU.EQ.'5')THEN
          GOTO 58
        ELSEIF(CMENU.EQ.'6')THEN
          GOTO 60
        ELSEIF(CMENU.EQ.'7')THEN
          GOTO 70
        END IF
C------------------------------------------------------------------------------
C******************************************************************************
C------------------------------------------------------------------------------
C si queremos, dibujamos imagenes desplazandolas por el correspondiente offset
45      CONTINUE
        WRITE(*,*)
        WRITE(*,101) 'Select type of frames to be plotted:'
        WRITE(*,101) '(1) Raw frames'
        WRITE(*,101) '(2) Normalized (raw-dark) frames'
        WRITE(*,101) '(3) Frames after flatfielding'
        CPFLAT(1:1)=READC('Option (1..3)','1','123')
        CBGFG_MODE(1:1)=READC('BG & FG mode: [a]uto or [f]ixed (a/f)',
     +   'a','af')
        CALL PGSUBP(1,1)
        CALL PGERAS
        NX1=1
        NX2=NAXIS(1)
        NY1=1
        NY2=NAXIS(2)
        LOOP=.TRUE.
        NF=1
        NF_LAST=0
        NF_=0
        DO WHILE(LOOP)
c..............................................................................
          IF(NF_.GT.0)THEN
            LOK=.FALSE.
            DO WHILE(.NOT.LOK)
              NF=NF+1
              IF(NF.GT.NFRAMES) NF=1
              LOK=(FRAME_IOK(NF).EQ.0)
            END DO
          END IF
          WRITE(CDUMMY,*) NF
          WRITE(*,101) ' 0: exit'
          WRITE(*,101) ' #: frame to plot'
          IF(NF_LAST.NE.0)THEN
            WRITE(*,100) '-1: BG & FG mode: '
            IF(CBGFG_MODE.EQ.'a')THEN
              WRITE(*,101) 'auto'
            ELSE
              WRITE(*,101) 'fixed'
            END IF
            WRITE(*,101) '-2: change plot limits'
            WRITE(*,100) '-3: overplot mask: '
            IF(LOVER_CDMASK)THEN
              WRITE(*,101) 'TRUE'
            ELSE
              WRITE(*,101) 'FALSE'
            END IF
            WRITE(*,100) '-4: overplot objects mask: '
            IF(LOVER_OBMASK)THEN
              WRITE(*,101) 'TRUE'
            ELSE
              WRITE(*,101) 'FALSE'
            END IF
            NF_=READILIM('Option',CDUMMY,-4,NFRAMES)
          ELSE
            NF_=READILIM('Option',CDUMMY,0,NFRAMES)
          END IF
c..............................................................................
          IF(NF_.EQ.-4)THEN
            IF(LOVER_OBMASK)THEN
              LOVER_OBMASK=.FALSE.
            ELSE
              LOVER_OBMASK=.TRUE.
            END IF
c..............................................................................
          ELSEIF(NF_.EQ.-3)THEN
            IF(LOVER_CDMASK)THEN
              LOVER_CDMASK=.FALSE.
            ELSE
              LOVER_CDMASK=.TRUE.
            END IF
c..............................................................................
          ELSEIF(NF_.EQ.-2)THEN
            WRITE(CDUMMY,*) NX1
            NX1=READILIM('New NX1',CDUMMY,1,NAXIS(1))
            WRITE(CDUMMY,*) NX2
            NX2=READILIM('New NX2',CDUMMY,NX1,NAXIS(1))
            WRITE(CDUMMY,*) NY1
            NY1=READILIM('New NY1',CDUMMY,1,NAXIS(2))
            WRITE(CDUMMY,*) NY2
            NY2=READILIM('New NY2',CDUMMY,NY1,NAXIS(2))
c..............................................................................
          ELSEIF(NF_.EQ.-1)THEN
            CBGFG_MODE(1:1)=
     +       READC('BG & FG mode: [a]uto or [f]ixed (a/f)',
     +       CBGFG_MODE,'af')
            IF(CBGFG_MODE.EQ.'f')THEN
              WRITE(CDUMMY,*) BG
              BG=READF('Background',CDUMMY)
              WRITE(CDUMMY,*) FG
              FG=READF('Foreground',CDUMMY)
            ELSE
              WRITE(CDUMMY,*) TIMES1_BGFG
              TIMES1_BGFG=READF('-Times sigma for BG?',CDUMMY)
              WRITE(CDUMMY,*) TIMES2_BGFG
              TIMES2_BGFG=READF('+Times sigma for FG?',CDUMMY)
              CALL SONDEA4C(FRAME_DATA,NXMAXF,NYMAXF,NAXISF1,NAXISF2,
     +         SONDEO4C_FMEAN,SONDEO4C_FSIGMA)
              BG=SONDEO4C_FMEAN-TIMES1_BGFG*SONDEO4C_FSIGMA
              FG=SONDEO4C_FMEAN+TIMES2_BGFG*SONDEO4C_FSIGMA
            END IF
c..............................................................................
          ELSEIF(NF_.EQ.0)THEN
            LOOP=.FALSE.
c..............................................................................
          ELSE
            IF(FRAME_IOK(NF_).EQ.0)THEN
              NF=NF_
            ELSE
              WRITE(*,101) 'WARNING: this frame does not exist.'
              NF=NF_LAST
            END IF
            ISTATUS=0
            IREADWRITE=0
            ANYNULL=.FALSE.
            IF(CPFLAT.EQ.'1')THEN
              FILELABEL=FILE_FRAME(NF)
            ELSEIF(CPFLAT.EQ.'2')THEN
              FILELABEL=FILE_FRAME_NOR(NF)
            ELSEIF(CPFLAT.EQ.'3')THEN
              FILELABEL=FILE_FRAME_AFTER_FLAT(NF)
            END IF
            CALL READFITS(FILELABEL,NXMAXF,NYMAXF,
     +       NAXISF1,NAXISF2,FRAME_DATA,.TRUE.,EXPTIME)
            IF((NF_LAST.EQ.0).OR.(CBGFG_MODE.EQ.'a'))THEN
              CALL SONDEA4C(FRAME_DATA,NXMAXF,NYMAXF,NAXISF1,NAXISF2,
     +         SONDEO4C_FMEAN,SONDEO4C_FSIGMA)
              BG=SONDEO4C_FMEAN-TIMES1_BGFG*SONDEO4C_FSIGMA
              FG=SONDEO4C_FMEAN+TIMES2_BGFG*SONDEO4C_FSIGMA
              WRITE(*,100) '> New BG & FG values: '
              WRITE(*,*) BG,FG
            END IF
            NF_LAST=NF
            DO I=1,NAXIS(2)
              DO J=1,NAXIS(1)
                IMAGEN_FINAL(J,I)=0.0
              END DO
            END DO
            DO I=1,NAXISF2
              DO J=1,NAXISF1
                IMAGEN_FINAL(J+JJ_FRAME(NF),I+II_FRAME(NF))=
     +           FRAME_DATA(J,I)
              END DO
            END DO
            FX1=REAL(J1F(NF))-0.5
            FX2=REAL(J2F(NF))+0.5
            FY1=REAL(I1F(NF))-0.5
            FY2=REAL(I2F(NF))+0.5
          END IF
c..............................................................................
          IF(LOOP)THEN
            IF((NF_.NE.-3).AND.(NF_.NE.-4))THEN
              CALL DIBUFRAME(IMAGEN_FINAL,NXMAX,NYMAX,NAXIS(1),NAXIS(2),
     +         NX1,NX2,NY1,NY2,FILELABEL,.FALSE.,BG,FG)
              CALL PGSCI(3)
              CALL PGMOVE(FX1,FY1)
              CALL PGDRAW(FX2,FY1)
              CALL PGDRAW(FX2,FY2)
              CALL PGDRAW(FX1,FY2)
              CALL PGDRAW(FX1,FY1)
              CALL PGSCI(1)
              WRITE(CDUMMY,'(I10,A1,I10)') NF,'/',NFRAMES
              CALL RMBLANK(CDUMMY,CDUMMY,L)
              WRITE(*,100) '> Displayed image is '//CDUMMY(1:L)//': '
              WRITE(*,101) FILE_FRAME(NF)(1:TRUELEN(FILE_FRAME(NF)))
              WRITE(*,*)
            END IF
            IF(LOVER_CDMASK)THEN
              CALL PGBBUF
              CALL PGSCI(2)
              DO I=1,NAXISF2
                DO J=1,NAXISF1
                  IF(MASK_FRAME(J,I))THEN
                    CALL PGRECT(REAL(J+JJ_FRAME(NF_LAST))-0.5,
     +                          REAL(J+JJ_FRAME(NF_LAST))+0.5,
     +                          REAL(I+II_FRAME(NF_LAST))-0.5,
     +                          REAL(I+II_FRAME(NF_LAST))+0.5)
                  END IF
                END DO
              END DO
              CALL PGSCI(1)
              CALL PGEBUF
            END IF
            IF(LOVER_OBMASK)THEN
              CALL PGBBUF
              CALL PGSCI(7)
              DO I=1,NAXIS(2)
                DO J=1,NAXIS(1)
                  IF(MASK_IMAGEN_FINAL(J,I))THEN
                    CALL PGRECT(REAL(J)-0.5,
     +                          REAL(J)+0.5,
     +                          REAL(I)-0.5,
     +                          REAL(I)+0.5)
                  END IF
                END DO
              END DO
              CALL PGSCI(1)
              CALL PGEBUF
            END IF
          END IF
c..............................................................................
        END DO
        GOTO 40
C------------------------------------------------------------------------------
C******************************************************************************
C------------------------------------------------------------------------------
C (1) normalize individual (raw-dark) frames
50      CONTINUE
        WRITE(*,*)
        WRITE(*,100) '>>> STEP '
        WRITE(*,101) '(1) normalize individual (raw-dark) frames'
        WRITE(*,*)
        WRITE(CDUMMY,*) NF1
        NF1=READILIM('First frame to normalize (raw-dark)',CDUMMY,
     +   1,NFRAMES)
        WRITE(CDUMMY,*) NF2
        NF2=READILIM('Last frame to normalize (raw-dark)',CDUMMY,
     +   NF1,NFRAMES)
        WRITE(*,*)
        WRITE(*,101) '* Normalization mode:'
        WRITE(*,101) '(0) divide by median of full frame '//
     +   '(excl. C.D. & Obj.)'
        WRITE(*,101) '(1) divide by median of each quadrant '//
     +   '(excl. C.D. & Obj.)'
        WRITE(*,101) '(2) divide by smooth 2D sky fit'
        CSKY(1:1)=READC('Option (0/1/2)','1','012')
        IF(CSKY.EQ.'2')THEN
          CSUFFIX_OLD=' '
          DO WHILE(TRUELEN(CSUFFIX_OLD).NE.2)
            CSUFFIX_OLD(1:2)=READC('Two-character suffix to identify '//
     +       'previous iteration (e.g. i1)','@','@')
            IF(TRUELEN(CSUFFIX_OLD).NE.2)THEN
              WRITE(*,101) 'ERROR: the suffix must have 2 non-blank'//
     +         ' characters. Try again.'
              WRITE(*,100) 'Press <CR> to continue...'
              READ(*,*)
            END IF
          END DO
        END IF
        WRITE(*,101) 'Working'
        DO NF=1,NFRAMES
          FMEDIAN_RAW(NF)=0.0
          IF((NF.GE.NF1).AND.(NF.LE.NF2))THEN
            IF(FRAME_IOK(NF).EQ.0)THEN
              !leemos frame
              CALL READFITS(FILE_FRAME(NF),NXMAXF,NYMAXF,
     +         NAXISF1,NAXISF2,FRAME_DATA,.TRUE.,EXPTIME_DATA(NF))
              !leemos dark
              CALL READFITS(FILE_DARK_FRAME(NF),NXMAXF,NYMAXF,
     +         NAXISF1,NAXISF2,FRAME_DARK,.TRUE.,EXPTIME_DARK(NF))
              IF(EXPTIME_DARK(NF).EQ.0.0)THEN
                WRITE(*,100) 'Frame number:'
                WRITE(*,*) NF
                WRITE(*,101) 'EXPTIME=0 in DARK image'
                STOP 'FATAL ERROR'
              END IF
              !calculamos (frame-dark)
              FACTOR_DARK=EXPTIME_DATA(NF)/EXPTIME_DARK(NF)
              DO I=1,NAXISF2
                DO J=1,NAXISF1
                  FRAME_DATA(J,I)=
     +             FRAME_DATA(J,I)-FACTOR_DARK*FRAME_DARK(J,I)
                END DO
              END DO
              !----------------------------------------------------------------
              IF(CSKY.EQ.'0')THEN
                !calculamos la mediana en (frame-dark), eliminando en el
                !calculo defectos cosmeticos y objetos
                NREG=1  !numero total de regiones
                FMEDIAN_RAW(NF)=0.0
                DO IREG=1,NREG
                  NX1Q=1
                  NX2Q=NAXISF1
                  NY1Q=1
                  NY2Q=NAXISF2
                  K=0
                  DO I=NY1Q,NY2Q
                    II=I+II_FRAME(NF)
                    DO J=NX1Q,NX2Q
                      JJ=J+JJ_FRAME(NF)
                      IF(.NOT.MASK_IMAGEN_FINAL(JJ,II))THEN
                        IF(.NOT.MASK_FRAME(J,I))THEN
                          K=K+1
                          PIXEL(K)=FRAME_DATA(J,I)
                        END IF
                      END IF
                    END DO
                  END DO
                  IF(K.GT.0)THEN
                    FMEDIAN_RAW_REG(IREG)=FMEDIAN1(K,PIXEL)
                  ELSE
                    FMEDIAN_RAW_REG(IREG)=1.0
                  END IF
                  IF(NF.EQ.NF1)THEN
                    WRITE(*,100) 'ireg, k, fmedian_raw_ireg: '
                    WRITE(*,*) IREG, K, FMEDIAN_RAW_REG(IREG)
                  END IF
                  !normalizamos
                  DO I=NY1Q,NY2Q
                    DO J=NX1Q,NX2Q
                      FRAME_DATA(J,I)=FRAME_DATA(J,I)/
     +                 FMEDIAN_RAW_REG(IREG)
                    END DO
                  END DO
                  FMEDIAN_RAW(NF)=FMEDIAN_RAW(NF)+
     +             FMEDIAN_RAW_REG(IREG)
                END DO
                FMEDIAN_RAW(NF)=FMEDIAN_RAW_REG(IREG)/REAL(NREG)
              !----------------------------------------------------------------
              ELSE IF(CSKY.EQ.'1')THEN
                !calculamos la mediana en (frame-dark), eliminando en el
                !calculo defectos cosmeticos y objetos
                NREG = 4 !numero total de regiones 
                FMEDIAN_RAW(NF)=0.0
                DO IREG=1,NREG
                  IF(IREG.EQ.1)THEN
                    NX1Q=1
                    NX2Q=(NAXISF1)/2
                    NY1Q=1
                    NY2Q=(NAXISF2)/2
                  ELSEIF(IREG.EQ.2)THEN
                    NX1Q=(NAXISF1)/2+1
                    NX2Q=NAXISF1
                    NY1Q=1
                    NY2Q=(NAXISF2)/2
                  ELSEIF(IREG.EQ.3)THEN
                    NX1Q=1
                    NX2Q=(NAXISF1)/2
                    NY1Q=(NAXISF2)/2+1
                    NY2Q=NAXISF2
                  ELSE
                    NX1Q=(NAXISF1)/2+1
                    NX2Q=NAXISF1
                    NY1Q=(NAXISF2)/2+1
                    NY2Q=NAXISF2
                  END IF
                  K=0
                  DO I=NY1Q,NY2Q
                    II=I+II_FRAME(NF)
                    DO J=NX1Q,NX2Q
                      JJ=J+JJ_FRAME(NF)
                      IF(.NOT.MASK_IMAGEN_FINAL(JJ,II))THEN
                        IF(.NOT.MASK_FRAME(J,I))THEN
                          K=K+1
                          PIXEL(K)=FRAME_DATA(J,I)
                        END IF
                      END IF
                    END DO
                  END DO
                  IF(K.GT.0)THEN
                    FMEDIAN_RAW_REG(IREG)=FMEDIAN1(K,PIXEL)
                  ELSE
                    FMEDIAN_RAW_REG(IREG)=1.0
                  END IF
                  IF(NF.EQ.NF1)THEN
                    WRITE(*,100) 'ireg, k, fmedian_raw_ireg: '
                    WRITE(*,*) IREG, K, FMEDIAN_RAW_REG(IREG)
                  END IF
                  !normalizamos
                  DO I=NY1Q,NY2Q
                    DO J=NX1Q,NX2Q
                      FRAME_DATA(J,I)=FRAME_DATA(J,I)/
     +                 FMEDIAN_RAW_REG(IREG)
                    END DO
                  END DO
                  FMEDIAN_RAW(NF)=FMEDIAN_RAW(NF)+
     +             FMEDIAN_RAW_REG(IREG)
                END DO
                FMEDIAN_RAW(NF)=FMEDIAN_RAW(NF)/REAL(NREG)
              !----------------------------------------------------------------
              ELSE
                L2=TRUELEN(FILE_FRAME(NF))
                L1=L2+1
                LOOP=.TRUE.
                DO WHILE(LOOP)
                  L1=L1-1
                  IF(FILE_FRAME(NF)(L1:L1).EQ.'/')THEN
                    L1=L1+1
                    LOOP=.FALSE.
                  END IF
                  IF(LOOP)THEN
                    IF(L1.EQ.1) LOOP=.FALSE.
                  END IF
                END DO
                FILE_FRAME_SKYFIT_OLD(NF)=FILE_FRAME(NF)(L1:L2-5)//
     +           '_skyfit_'//CSUFFIX_OLD//'.fits'
                !leemos FILE_FRAME_SKYFIT_OLD(NF), usando como array auxiliar
                !la variable FRAME_DARK
                CALL READFITS(FILE_FRAME_SKYFIT_OLD(NF),NXMAXF,NYMAXF,
     +           NAXISF1,NAXISF2,FRAME_DARK,.TRUE.,EXPTIME)
                !normalizamos
                DO I=1,NAXISF2
                  DO J=1,NAXISF1
                    IF(ABS(FRAME_DARK(J,I)).LT.1.E-30)THEN
                      WRITE(*,100) 'ERROR: Division by zero at J,I='
                      WRITE(*,*) J,I
                    ELSE
                      FRAME_DATA(J,I)=FRAME_DATA(J,I)/FRAME_DARK(J,I)
                    END IF
                  END DO
                END DO
              END IF
              !----------------------------------------------------------------
              !salvamos frames normalizados
              SIMPLE=.TRUE.
              BITPIX=-32
              EXTEND=.FALSE.
              BLOCKSIZE=1
              ISTATUS=0
              L2=TRUELEN(FILE_FRAME_NOR(NF))
              INQUIRE(FILE=FILE_FRAME_NOR(NF),EXIST=LOGFILE)
              IF(LOGFILE)THEN
                ISYSTEM=SYSTEMFUNCTION('rm -f '//
     +           FILE_FRAME_NOR(NF)(1:L2))
              END IF
              CALL FTINIT(80,FILE_FRAME_NOR(NF),
     +         BLOCKSIZE,ISTATUS)
              CALL FTPHPR(80,SIMPLE,BITPIX,2,NAXISF(1,1),0,1,
     +         EXTEND,ISTATUS)
              CALL FTPKYF(80,'EXPTIME',EXPTIME_DATA(NF),2,
     +         'Total Exposure Time [secs]',ISTATUS)
              CALL FTP2DE(80,1,NXMAXF,NAXISF1,NAXISF2,FRAME_DATA,
     +         ISTATUS)
              CALL FTCLOS(80,ISTATUS)
              IF(ISTATUS.GT.0) CALL PRINTERROR(ISTATUS)
              WRITE(*,100) '#' !imagen calculada
            ELSE
              WRITE(*,100) 'X' !imagen no valida en lista
            END IF
          ELSE
            WRITE(*,100) 'i' !imagen ignorada
          END IF
          IF(MOD(NF,10).EQ.0) WRITE(*,*) NF
        END DO
        WRITE(*,*)
        WRITE(*,100) '>>> END OF STEP '
        WRITE(*,101) '(1) normalize individual (raw-dark) frames'
c
        IF(CRUN_THROUGH.EQ.'n') GOTO 40
C------------------------------------------------------------------------------
C******************************************************************************
C------------------------------------------------------------------------------
C (2) compute superflat
52      CONTINUE
        WRITE(*,*)
        WRITE(*,100) '>>> STEP '
        WRITE(*,101) '(2) compute superflat'
        WRITE(*,*)
        WRITE(CDUMMY,*) NF1
        NF1=READILIM('First frame to compute superflat',CDUMMY,
     +   1,NFRAMES)
        WRITE(CDUMMY,*) NF2
        NF2=READILIM('Last frame to compute superflat',CDUMMY,
     +   NF1,NFRAMES)
c inicializamos imagen suma
        DO I=1,NAXISF2
          DO J=1,NAXISF1
            FRAME_SUMFLAT(J,I)=0.0
            FRAME_NPIXELS(J,I)=0
          END DO
        END DO
c calculamos imagen suma, excluyendo solamente los objetos
        DO NF=1,NFRAMES
          IF((NF.GE.NF1).AND.(NF.LE.NF2))THEN
            IF(FRAME_IOK(NF).EQ.0)THEN
              !leemos (raw-dark) normalizado
              CALL READFITS(FILE_FRAME_NOR(NF),NXMAXF,NYMAXF,
     +         NAXISF1,NAXISF2,FRAME_DATA,.TRUE.,EXPTIME_DATA(NF))
              DO I=1,NAXISF2
                II=I+II_FRAME(NF)
                DO J=1,NAXISF1
                  JJ=J+JJ_FRAME(NF)
                  IF(.NOT.MASK_IMAGEN_FINAL(JJ,II))THEN
                    FRAME_SUMFLAT(J,I)=FRAME_SUMFLAT(J,I)+
     +               FRAME_DATA(J,I)
                    FRAME_NPIXELS(J,I)=FRAME_NPIXELS(J,I)+1
                  END IF
                END DO
              END DO
              WRITE(*,100) '#' !imagen calculada
            ELSE
              WRITE(*,100) 'X' !imagen no valida en lista
            END IF
          ELSE
            WRITE(*,100) 'i' !imagen ignorada
          END IF
          IF(MOD(NF,10).EQ.0) WRITE(*,*) NF
        END DO
        WRITE(*,*)
c calculamos histograma de numero de frames/pixel para el flat
        MIN_FRAME_NPIXELS=NFRAMES
        MAX_FRAME_NPIXELS=0
        DO K=1,NFRAMES
          NPIX=0
          DO I=1,NAXISF2
            DO J=1,NAXISF1
              IF(FRAME_NPIXELS(J,I).EQ.K) NPIX=NPIX+1
            END DO
          END DO
          IF(NPIX.GT.0)THEN
            WRITE(*,100) 'Nframes/pixel, Npixels: '
            WRITE(*,*) K,NPIX
            MIN_FRAME_NPIXELS=MIN0(MIN_FRAME_NPIXELS,K)
            MAX_FRAME_NPIXELS=MAX0(MAX_FRAME_NPIXELS,K)
          END IF
        END DO
        WRITE(*,100) '> min_frame_npixels, max_frame_npixels: '
        WRITE(*,*) MIN_FRAME_NPIXELS,MAX_FRAME_NPIXELS
c normalizamos el flatfield
        DO I=1,NAXISF2
          DO J=1,NAXISF1
            FRAME_FLAT(J,I)=FRAME_SUMFLAT(J,I)/REAL(FRAME_NPIXELS(J,I))
          END DO
        END DO
        FILE_SUPERFLAT_NPIX=READC('File name for superflat_npix.fits',
     +   FILE_SUPERFLAT_NPIX,'@')
c dibujamos superflat_npix.fits
        NX1=1
        NX2=NAXISF1
        NY1=1
        NY2=NAXISF2
        BG=REAL(MIN_FRAME_NPIXELS)-0.5
        FG=REAL(MAX_FRAME_NPIXELS)+0.5
        !OJO: reutilizamos la matriz FRAME_DATA para ahorrar memoria
        DO I=1,NAXISF2
          DO J=1,NAXISF1
            FRAME_DATA(J,I)=REAL(FRAME_NPIXELS(J,I))
          END DO
        END DO
        CALL DIBUFRAME(FRAME_DATA,NXMAXF,NYMAXF,NAXISF1,NAXISF2,
     +   NX1,NX2,NY1,NY2,FILE_SUPERFLAT_NPIX,.FALSE.,BG,FG)
c salvamos imagen de npixels para superflat
        INQUIRE(FILE=FILE_SUPERFLAT_NPIX,EXIST=LOGFILE)
        IF(LOGFILE)THEN
          L2=TRUELEN(FILE_SUPERFLAT_NPIX)
          ISYSTEM=SYSTEMFUNCTION('rm -f '//
     +     FILE_SUPERFLAT_NPIX(1:L2))
        END IF
        SIMPLE=.TRUE.
        BITPIX=-32
        EXTEND=.FALSE.
        BLOCKSIZE=1
        CALL FTINIT(80,FILE_SUPERFLAT_NPIX,BLOCKSIZE,ISTATUS)
        CALL FTPHPR(80,SIMPLE,BITPIX,2,NAXISF(1,1),0,1,
     +   EXTEND,ISTATUS)
        CALL FTP2DE(80,1,NXMAXF,NAXISF1,NAXISF2,FRAME_DATA,ISTATUS)
        CALL FTCLOS(80,ISTATUS)
        IF(ISTATUS.GT.0) CALL PRINTERROR(ISTATUS)
c pausa
        FILE_SUPERFLAT=READC('File name for superflat.fits',
     +   FILE_SUPERFLAT,'@')
c dibujamos superflat.fits
        NX1=1
        NX2=NAXISF1
        NY1=1
        NY2=NAXISF2
        CALL DIBUFRAME(FRAME_FLAT,NXMAXF,NYMAXF,NAXISF1,NAXISF2,
     +   NX1,NX2,NY1,NY2,FILE_SUPERFLAT,.TRUE.,BG,FG)
c salvamos imagen de superflat
        INQUIRE(FILE=FILE_SUPERFLAT,EXIST=LOGFILE)
        IF(LOGFILE)THEN
          L2=TRUELEN(FILE_SUPERFLAT)
          ISYSTEM=SYSTEMFUNCTION('rm -f '//
     +     FILE_SUPERFLAT(1:L2))
        END IF
        SIMPLE=.TRUE.
        BITPIX=-32
        EXTEND=.FALSE.
        BLOCKSIZE=1
        CALL FTINIT(80,FILE_SUPERFLAT,BLOCKSIZE,ISTATUS)
        CALL FTPHPR(80,SIMPLE,BITPIX,2,NAXISF(1,1),0,1,
     +   EXTEND,ISTATUS)
        CALL FTP2DE(80,1,NXMAXF,NAXISF1,NAXISF2,FRAME_FLAT,ISTATUS)
        CALL FTCLOS(80,ISTATUS)
        IF(ISTATUS.GT.0) CALL PRINTERROR(ISTATUS)
        WRITE(*,*)
        WRITE(*,100) '>>> END OF STEP '
        WRITE(*,101) '(2) compute superflat'
c
        IF(CRUN_THROUGH.EQ.'n') GOTO 40
C------------------------------------------------------------------------------
C******************************************************************************
C------------------------------------------------------------------------------
C (3) compute individual (raw-dark)/superflat
54      CONTINUE
        WRITE(*,*)
        WRITE(*,100) '>>> STEP '
        WRITE(*,101) '(3) compute individual (raw-dark)/superflat'
        WRITE(*,*)
c leemos superflat
        FILE_SUPERFLAT=READC('File name of superflat.fits',
     +   FILE_SUPERFLAT,'@')
        CALL READFITS(FILE_SUPERFLAT,NXMAXF,NYMAXF,
     +   NAXISF1,NAXISF2,FRAME_FLAT,.TRUE.,EXPTIME)
        WRITE(CDUMMY,*) NF1
        NF1=READILIM('First frame to compute (raw-dark)/superflat',
     +   CDUMMY,1,NFRAMES)
        WRITE(CDUMMY,*) NF2
        NF2=READILIM('Last frame to compute (raw-dark)/superflat',
     +   CDUMMY,NF1,NFRAMES)
        CASK(1:1)=READC('Pause between plots (y/n)',CASK,'yn')
c leemos imagenes (raw-dark) y las dividimos por el superflat
        DO NF=1,NFRAMES
          WRITE(*,100) 'Working with frame #'
          WRITE(CDUMMY,'(I10,A1,I10)') NF,'/',NFRAMES
          CALL RMBLANK(CDUMMY,CDUMMY,L)
          WRITE(*,100) CDUMMY(1:L)
          IF((NF.GE.NF1).AND.(NF.LE.NF2))THEN
ccc         IF(FRAME_IOK(NF).EQ.0)THEN
              !leemos frame
              CALL READFITS(FILE_FRAME(NF),NXMAXF,NYMAXF,
     +         NAXISF1,NAXISF2,FRAME_DATA,.TRUE.,EXPTIME_DATA(NF))
              !leemos dark
              CALL READFITS(FILE_DARK_FRAME(NF),NXMAXF,NYMAXF,
     +         NAXISF1,NAXISF2,FRAME_DARK,.TRUE.,EXPTIME_DARK(NF))
              !calculamos (frame-dark)
              FACTOR_DARK=EXPTIME_DATA(NF)/EXPTIME_DARK(NF)
              DO I=1,NAXISF2
                DO J=1,NAXISF1
                  FRAME_DATA(J,I)=
     +             FRAME_DATA(J,I)-FACTOR_DARK*FRAME_DARK(J,I)
                END DO
              END DO
              !dividimos por superflat, teniendo cuidado de division por cero
              DO I=1,NAXISF2
                DO J=1,NAXISF1
                  IF(ABS(FRAME_FLAT(J,I)).GT.1E-30)THEN !flat mayor que cero
                    FRAME_DATA(J,I)=FRAME_DATA(J,I)/FRAME_FLAT(J,I)
                  ELSE
                    FRAME_DATA(J,I)=0.0
                  END IF
                END DO
              END DO
              WRITE(*,101) '...OK!' !imagen calculada
              !dibujamos FILE_FRAME_AFTER_FLAT(NF)
              NX1=1
              NX2=NAXISF1
              NY1=1
              NY2=NAXISF2
              CALL DIBUFRAME(FRAME_DATA,NXMAXF,NYMAXF,NAXISF1,NAXISF2,
     +         NX1,NX2,NY1,NY2,FILE_FRAME_AFTER_FLAT(NF),.TRUE.,BG,FG)
              IF(CASK.EQ.'y')THEN
                WRITE(*,100) 'Press <CR> to continue...'
                READ(*,*)
                IF(LECHO) WRITE(*,*)
              END IF
              !calculamos mediana ignorando defectos cosmeticos y objetos
              K=0
              CALL PGBBUF
              DO I=1,NAXISF2
                II=I+II_FRAME(NF)
                DO J=1,NAXISF1
                  JJ=J+JJ_FRAME(NF)
                  IF(.NOT.MASK_IMAGEN_FINAL(JJ,II))THEN
                    IF(.NOT.MASK_FRAME(J,I))THEN
                      K=K+1
                      PIXEL(K)=FRAME_DATA(J,I)
                    ELSE
                      CALL PGSCI(2)
                      CALL PGRECT(REAL(J)-0.5,REAL(J)+0.5,
     +                            REAL(I)-0.5,REAL(I)+0.5)
                    END IF
                  ELSE
                    CALL PGSCI(7)
                    CALL PGRECT(REAL(J)-0.5,REAL(J)+0.5,
     +                          REAL(I)-0.5,REAL(I)+0.5)
                  END IF
                END DO
              END DO
              CALL PGSCI(1)
              CALL PGEBUF
              FMEDIAN_SKY(NF)=FMEDIAN1(K,PIXEL)
              IF(CASK.EQ.'y')THEN
                WRITE(*,100) 'Press <CR> to continue...'
                READ(*,*)
                IF(LECHO) WRITE(*,*)
              END IF
              !salvamos imagen (raw-dark)/superflat
              INQUIRE(FILE=FILE_FRAME_AFTER_FLAT(NF),EXIST=LOGFILE)
              IF(LOGFILE)THEN
                L2=TRUELEN(FILE_FRAME_AFTER_FLAT(NF))
                ISYSTEM=SYSTEMFUNCTION('rm -f '//
     +           FILE_FRAME_AFTER_FLAT(NF)(1:L2))
              END IF
              SIMPLE=.TRUE.
              BITPIX=-32
              EXTEND=.FALSE.
              BLOCKSIZE=1
              CALL FTINIT(80,FILE_FRAME_AFTER_FLAT(NF),BLOCKSIZE,
     +         ISTATUS)
              CALL FTPHPR(80,SIMPLE,BITPIX,2,NAXISF(1,1),0,1,
     +         EXTEND,ISTATUS)
              CALL FTPKYF(80,'EXPTIME',EXPTIME_DATA(NF),2,
     +         'Total Exposure Time [secs]',ISTATUS)
              CALL FTP2DE(80,1,NXMAXF,NAXISF1,NAXISF2,FRAME_DATA,
     +         ISTATUS)
              CALL FTCLOS(80,ISTATUS)
              IF(ISTATUS.GT.0) CALL PRINTERROR(ISTATUS)
ccc         ELSE
ccc           WRITE(*,101) ' (skipped)' !imagen no valida en lista
ccc         END IF
          ELSE
            WRITE(*,101) ' (ignored)' !imagen ignorada
          END IF
        END DO
c dibujamos los resultados
        CALL PGSUBP(1,1)
        CALL PGERAS
        DO NF=1,NFRAMES
          XP(NF)=REAL(NF)
          YP(NF)=FMEDIAN_SKY(NF)
        END DO
        NP=NFRAMES
        CALL FINDMML(NP,1,NP,XP,XMIN,XMAX)
        CALL FINDMML(NP,1,NP,YP,YMIN,YMAX)
        DX=XMAX-XMIN
        XMIN=XMIN-DX/20.
        XMAX=XMAX+DX/20.
        DY=YMAX-YMIN
        YMIN=YMIN-DY/20.
        YMAX=YMAX+DY/20.
        CALL PGENV(XMIN,XMAX,YMIN,YMAX,0,0)
        CALL PGLABEL('number of frame','median',' ')
        CALL PGPOINT(NP,XP,YP,17)
        CALL PGSCI(2)
        DO NF=1,NFRAMES
          IF(FRAME_IOK(NF).NE.0)THEN
            CALL PGPOINT(1,XP(NF),YP(NF),5)
          END IF
        END DO
        CALL PGSCI(1)
C salvamos los resultados a un fichero
        FILE_MEDIAN_SKY=READC('Output ASCII file for median values',
     +   FILE_MEDIAN_SKY,'@')
        INQUIRE(FILE=FILE_MEDIAN_SKY,EXIST=LOGFILE)
        IF(LOGFILE)THEN
          L2=TRUELEN(FILE_MEDIAN_SKY)
          ISYSTEM=SYSTEMFUNCTION('rm -f '//FILE_MEDIAN_SKY(1:L2))
        END IF
        OPEN(10,FILE=FILE_MEDIAN_SKY,STATUS='NEW',FORM='FORMATTED')
        DO NF=1,NFRAMES
          WRITE(10,*) NF,FMEDIAN_SKY(NF)
        END DO
        CLOSE(10)
        WRITE(*,*)
        WRITE(*,100) '>>> END OF STEP '
        WRITE(*,101) '(3) compute individual (raw-dark)/superflat'
C
        IF(CRUN_THROUGH.EQ.'n') GOTO 40
C------------------------------------------------------------------------------
C******************************************************************************
C------------------------------------------------------------------------------
C (4) sky subraction in (frame-dark)/superflat
C
C Corregimos la falta de planitud en (frame-dark)/superflat debido al hecho
C de que el cielo puede cambiar, espacialmente, de unos frames a otros. Para
C ello vamos a calcular una imagen con variaciones de baja frecuencia ajustada
C a (frame-dark)/superflat, enmascarando los pixels afectados por defectos
C cosmeticos y objetos.
56      CONTINUE
        WRITE(*,*)
        WRITE(*,100) '>>> STEP '
        WRITE(*,101) '(4) sky subraction in (frame-dark)/superflat'
        WRITE(*,*)
        WRITE(CDUMMY,*) NF1
        NF1=READILIM('First frame to compute smooth 2D surface',CDUMMY,
     +   1,NFRAMES)
        WRITE(CDUMMY,*) NF2
        NF2=READILIM('Last frame to compute smooth 2D surface',CDUMMY,
     +   NF1,NFRAMES)
        WRITE(*,101) '(1) subtract median of each quadrant'
        WRITE(*,101) '(2) subtract median of each 64 CIRCE regions'
        WRITE(*,101) '(3) subtract smooth 2D fit'
        COPC(1:1)=READC('Option (1/2/3)','1','123')
        IF(COPC.EQ.'3')THEN
          WRITE(CDUMMY,*) NBOXFILTX
          NBOXFILTX=READILIM('Filter box size in X-direction (odd)',
     +     CDUMMY,1,NAXISF1)
          IF(MOD(NBOXFILTX,2).EQ.0) NBOXFILTX=NBOXFILTX+1
          WRITE(CDUMMY,*) NBOXFILTY
          NBOXFILTY=READILIM('Filter box size in Y-direction (odd)',
     +     CDUMMY,1,NAXISF2)
          IF(MOD(NBOXFILTY,2).EQ.0) NBOXFILTY=NBOXFILTY+1
          WRITE(CDUMMY,*) NBOXFILTX*NBOXFILTY
          NPIXMIN=READILIM('Minimun number of pixels in filter box',
     +     CDUMMY,1,NBOXFILTX*NBOXFILTY)
          WRITE(*,101) '(1) mean'
          WRITE(*,101) '(2) median'
          CFILTER(1:1)=READC('Option (1/2)','2','12')
        END IF
        CASK(1:1)=READC('Pause between plots (y/n)',CASK,'yn')
C
        DO NF=1,NFRAMES
          WRITE(*,100) 'Working with frame #'
          WRITE(CDUMMY,'(I10,A1,I10)') NF,'/',NFRAMES
          CALL RMBLANK(CDUMMY,CDUMMY,L)
          WRITE(*,100) CDUMMY(1:L)
          IF((NF.GE.NF1).AND.(NF.LE.NF2))THEN
            IF(FRAME_IOK(NF).EQ.0)THEN
              !leemos imagen (frame-dark)/superflat
              CALL READFITS(FILE_FRAME_AFTER_FLAT(NF),NXMAXF,NYMAXF,
     +         NAXISF1,NAXISF2,FRAME_DATA,.TRUE.,EXPTIME_DATA(NF))
              !dibujamos FILE_FRAME_AFTER_FLAT(NF)
              NX1=1
              NX2=NAXISF1
              NY1=1
              NY2=NAXISF2
              CALL DIBUFRAME(FRAME_DATA,NXMAXF,NYMAXF,NAXISF1,NAXISF2,
     +         NX1,NX2,NY1,NY2,FILE_FRAME_AFTER_FLAT(NF),.TRUE.,BG,FG)
              !calculamos mascara efectiva para eliminar pixel en el ajuste
              DO I=1,NAXISF2
                II=I+II_FRAME(NF)
                DO J=1,NAXISF1
                  JJ=J+JJ_FRAME(NF)
                  MASK_FRAME_EFF(J,I)=.TRUE.
                  IF(.NOT.MASK_IMAGEN_FINAL(JJ,II))THEN
                    IF(.NOT.MASK_FRAME(J,I))THEN
                      MASK_FRAME_EFF(J,I)=.FALSE.
                    END IF
                  END IF
                END DO
              END DO
              !pausa
              IF(CASK.EQ.'y')THEN
                WRITE(*,*)
                WRITE(*,100) 'Press <CR> to overplot effective mask...'
                READ(*,*)
                IF(LECHO) WRITE(*,*)
              END IF
              !dibujamos mascara efectiva
              CALL PGBBUF
              CALL PGSCI(4)
              DO I=1,NAXISF2
                DO J=1,NAXISF1
                  IF(MASK_FRAME_EFF(J,I))THEN
                    CALL PGRECT(REAL(J)-0.5,REAL(J)+0.5,
     +                          REAL(I)-0.5,REAL(I)+0.5)
                  END IF
                END DO
              END DO
              CALL PGSCI(1)
              CALL PGEBUF
              !segun la opcion elegida, realizamos la sustraccion de la
              !mediana o la sustraccion del ajuste a superficie suave
              !----------------------------------------------------------------
              !restamos la mediana de cada cuadrante
              IF(COPC.EQ.'1')THEN
                FMEDIAN_SKY(NF)=0.0
                NREG=4  !numero total de regiones
                DO IREG=1,NREG
                  IF(IREG.EQ.1)THEN
                    NX1Q=1
                    NX2Q=(NAXISF1)/2
                    NY1Q=1
                    NY2Q=(NAXISF2)/2
                  ELSEIF(IREG.EQ.2)THEN
                    NX1Q=(NAXISF1)/2+1
                    NX2Q=NAXISF1
                    NY1Q=1
                    NY2Q=(NAXISF2)/2
                  ELSEIF(IREG.EQ.3)THEN
                    NX1Q=1
                    NX2Q=(NAXISF1)/2
                    NY1Q=(NAXISF2)/2+1
                    NY2Q=NAXISF2
                  ELSE
                    NX1Q=(NAXISF1)/2+1
                    NX2Q=NAXISF1
                    NY1Q=(NAXISF2)/2+1
                    NY2Q=NAXISF2
                  END IF
                  K=0
                  DO I=NY1Q,NY2Q
                    DO J=NX1Q,NX2Q
                      IF(.NOT.MASK_FRAME_EFF(J,I))THEN
                        K=K+1
                        PIXEL(K)=FRAME_DATA(J,I)
                      END IF
                    END DO
                  END DO
                  FMEDIAN_SKY_REG(IREG)=FMEDIAN1(K,PIXEL)
                  DO I=NY1Q,NY2Q
                    DO J=NX1Q,NX2Q
                      FRAME_DATA(J,I)=FRAME_DATA(J,I)-
     +                 FMEDIAN_SKY_REG(IREG)
                    END DO
                  END DO
                  FMEDIAN_SKY(NF)=FMEDIAN_SKY(NF)+
     +             FMEDIAN_SKY_REG(IREG)
                END DO
                FMEDIAN_SKY(NF)=FMEDIAN_SKY(NF)/REAL(NREG)
                WRITE(*,*)
              !----------------------------------------------------------------
              !restamos la mediana de cada region de CIRCE
              ELSE IF(COPC.EQ.'2')THEN
                IF((NAXISF1.NE.2048).OR.(NAXISF2.NE.2048))THEN
                  WRITE(*,100)'NAXIS1, NAXIS2: '
                  WRITE(*,*) NAXISF1, NAXISF2
                  STOP('Unexpected CIRCE image dimension')
                END IF
                FMEDIAN_SKY(NF)=0.0
                NREG=64  !numero total de regiones
                DO IREG=1,NREG
                  IF(IREG.GT.32)THEN
                    NX1Q=1+(IREG-33)*64
                    NX2Q=NX1Q+63
                    NY1Q=1025
                    NY2Q=2048
                  ELSE
                    NX1Q=1+(IREG-1)*64
                    NX2Q=NX1Q+63
                    NY1Q=1
                    NY2Q=1024
                  END IF
                  K=0
                  DO I=NY1Q,NY2Q
                    DO J=NX1Q,NX2Q
                      IF(.NOT.MASK_FRAME_EFF(J,I))THEN
                        K=K+1
                        PIXEL(K)=FRAME_DATA(J,I)
                      END IF
                    END DO
                  END DO
                  IF(K.GT.0)THEN
                    FMEDIAN_SKY_REG(IREG)=FMEDIAN1(K,PIXEL)
                  ELSE
                    FMEDIAN_SKY_REG(IREG)=0.0
                  END IF
                  DO I=NY1Q,NY2Q
                    DO J=NX1Q,NX2Q
                      FRAME_DATA(J,I)=FRAME_DATA(J,I)-
     +                 FMEDIAN_SKY_REG(IREG)
                    END DO
                  END DO
                  FMEDIAN_SKY(NF)=FMEDIAN_SKY(NF)+
     +             FMEDIAN_SKY_REG(IREG)
                END DO
                FMEDIAN_SKY(NF)=FMEDIAN_SKY(NF)/REAL(NREG)
                WRITE(*,*)
              !----------------------------------------------------------------
              !restamos una imagen suave
              ELSE
                !filtramos la imagen usando la mascara efectiva; empleamos
                !la variable FRAME_DARK como matriz auxiliar para almacenar
                !la imagen filtrada
                IF(CASK.EQ.'n') WRITE(*,*)
                CALL FILTRASKY(CFILTER,
     +           FRAME_DATA,FRAME_DARK,PIXEL,MASK_FRAME_EFF,
     +           NXMAXF,NYMAXF,NAXISF1,NAXISF2,
     +           NBOXFILTX,NBOXFILTY,NPIXMIN)
                !pausa
                IF(CASK.EQ.'y')THEN
                  WRITE(*,*)
                  WRITE(*,100) 'Press <CR> to plot filtered image...'
                  READ(*,*)
                  IF(LECHO) WRITE(*,*)
                END IF
                !dibujamos imagen filtrada (almacenada en FRAME_DARK)
                NX1=1
                NX2=NAXISF1
                NY1=1
                NY2=NAXISF2
                CALL DIBUFRAME(FRAME_DARK,NXMAXF,NYMAXF,NAXISF1,NAXISF2,
     +           NX1,NX2,NY1,NY2,FILE_FRAME_AFTER_FLAT(NF),.TRUE.,BG,FG)
                !pausa
                IF(CASK.EQ.'y')THEN
                  WRITE(*,100) 'Press <CR> to continue...'
                  READ(*,*)
                  IF(LECHO) WRITE(*,*)
                END IF
                !salvamos imagen suavizada
                INQUIRE(FILE=FILE_FRAME_SKYFIT(NF),EXIST=LOGFILE)
                IF(LOGFILE)THEN
                  L2=TRUELEN(FILE_FRAME_SKYFIT(NF))
                  ISYSTEM=SYSTEMFUNCTION('rm -f '//
     +             FILE_FRAME_SKYFIT(NF)(1:L2))
                END IF
                SIMPLE=.TRUE.
                BITPIX=-32
                EXTEND=.FALSE.
                BLOCKSIZE=1
                CALL FTINIT(80,FILE_FRAME_SKYFIT(NF),BLOCKSIZE,
     +           ISTATUS)
                CALL FTPHPR(80,SIMPLE,BITPIX,2,NAXISF(1,1),0,1,
     +           EXTEND,ISTATUS)
                CALL FTP2DE(80,1,NXMAXF,NAXISF1,NAXISF2,FRAME_DARK,
     +           ISTATUS)
                CALL FTCLOS(80,ISTATUS)
                IF(ISTATUS.GT.0) CALL PRINTERROR(ISTATUS)
                !corregimos (frame-dark)/superflat de cielo residual
                DO I=1,NAXISF2
                  DO J=1,NAXISF1
                    FRAME_DATA(J,I)=FRAME_DATA(J,I)-FRAME_DARK(J,I)
                  END DO
                END DO
              END IF
              !----------------------------------------------------------------
              !dibujamos el resultado
              NX1=1
              NX2=NAXISF1
              NY1=1
              NY2=NAXISF2
              CALL DIBUFRAME(FRAME_DATA,NXMAXF,NYMAXF,NAXISF1,NAXISF2,
     +         NX1,NX2,NY1,NY2,FILE_FRAME_AFTER_FLAT(NF),.TRUE.,BG,FG)
              !salvamos ultima imagen
              INQUIRE(FILE=FILE_FRAME_SKYSUB(NF),EXIST=LOGFILE)
              IF(LOGFILE)THEN
                L2=TRUELEN(FILE_FRAME_SKYSUB(NF))
                ISYSTEM=SYSTEMFUNCTION('rm -f '//
     +           FILE_FRAME_SKYSUB(NF)(1:L2))
              END IF
              SIMPLE=.TRUE.
              BITPIX=-32
              EXTEND=.FALSE.
              BLOCKSIZE=1
              CALL FTINIT(80,FILE_FRAME_SKYSUB(NF),BLOCKSIZE,
     +         ISTATUS)
              CALL FTPHPR(80,SIMPLE,BITPIX,2,NAXISF(1,1),0,1,
     +         EXTEND,ISTATUS)
              CALL FTPKYF(80,'EXPTIME',EXPTIME_DATA(NF),2,
     +         'Total Exposure Time [secs]',ISTATUS)
              CALL FTP2DE(80,1,NXMAXF,NAXISF1,NAXISF2,FRAME_DATA,
     +         ISTATUS)
              CALL FTCLOS(80,ISTATUS)
              IF(ISTATUS.GT.0) CALL PRINTERROR(ISTATUS)
            ELSE
              WRITE(*,101) ' (skipped)' !imagen no valida en lista
            END IF
          ELSE
            WRITE(*,101) ' (ignored)' !imagen ignorada
          END IF
        END DO
        WRITE(*,*)
        WRITE(*,100) '>>> END OF STEP '
        WRITE(*,101) '(4) sky subraction in (frame-dark)/superflat'
C
        IF(CRUN_THROUGH.EQ.'n') GOTO 40
C------------------------------------------------------------------------------
C******************************************************************************
C------------------------------------------------------------------------------
C (5) combine sky-subtracted (raw-dark)/superflat frames
58      CONTINUE
        WRITE(*,*)
        WRITE(*,100) '>>> STEP '
        WRITE(*,101) '(5) combine sky-subtracted (raw-dark)/superflat'//
     +   ' frames'
        WRITE(*,*)
        K_EXTINCTION=READF('Atmospheric extinction coefficient '//
     +   '(mag. per airmass unit, e.g. 0.09)','@')
C------------------------------------------------------------------------------
        NBUNDLE=NMAXFRAMES/NFRAMES
        WRITE(*,100) '>>> NBUNDLE: '
        WRITE(*,*) NBUNDLE
        WRITE(*,*)
        WRITE(*,101) '(1) Median'
        WRITE(*,101) '(2) Histogram clipping'
        CMERGE(1:1)=READC('Option (1/2)','1','12')
        IF(CMERGE.EQ.'2')THEN
          WRITE(*,101)'************************************************'
          WRITE(*,101)'* STEPS to combine frames:'
          WRITE(*,101)'1) measure MEAN and SIGMA in histogram after'
          WRITE(*,101)'   removing a given fraction of pixels (faintest'
          WRITE(*,101)'   and brightest) of the histogram'
          WRITE(*,101)'2) obtain MEAN after removing +/- TSIGMA times'
          WRITE(*,101)'   SIGMA in the original histogram'
          WRITE(*,101)'************************************************'
          WRITE(*,*)
          WRITE(CDUMMY,*) FRACTION_REMOVE
          FRACTION_REMOVE=
     +     READF('Fraction of pixels to be removed in histograms',
     +     CDUMMY)
          WRITE(CDUMMY,*) TSIGMA_REMOVE
          TSIGMA_REMOVE=READF('Times sigma to remove pixels',CDUMMY)
        END IF
C------------------------------------------------------------------------------
        CALL PGERAS
        WRITE(CDUMMY,*) BG
        BG=READF('Background',CDUMMY)
        WRITE(CDUMMY,*) FG
        FG=READF('Foreground',CDUMMY)
C inicializamos imagenes finales
        DO I=1,NAXIS(2)
          DO J=1,NAXIS(1)
            IMAGEN_FINAL(J,I)=0.0
            IMAGEN_FINAL_RMS(J,I)=0.0
            IMAGEN_FINAL_NPIXELS(J,I)=0.0
          END DO
        END DO
C recorremos la imagen final a intervalos entre I1 y I2; para cada uno de
C estos intervalos, generamos un stack que contiene NBUNDLE filas de cada 
C frame NF
        I1=1
        LOOP=.TRUE.
        DO WHILE(LOOP)
          I2=I1+NBUNDLE-1
          IF(I2.GT.NAXIS(2)) I2=NAXIS(2)
          WRITE(*,100) 'Working with rows: '
          WRITE(*,*) I1,I2
          !recorremos todos los frames y buscamos cuales tenemos que leer
          DO NF=1,NFRAMES
            !inicialiamos el bundle del frame NF en el stack
            K1=(NF-1)*NBUNDLE+1
            K2=K1+NBUNDLE-1
            DO K=K1,K2
              DO J=1,NAXIS(1)
                STACK_DATA(J,K)=0.0
                STACK_IFPIXEL(J,K)=.FALSE.
              END DO
            END DO
            !si el frame se utiliza, trabajamos con el
            IF(FRAME_IOK(NF).EQ.0)THEN
              !coeficiente de correccion debido a extinction atmosferica
              FACTOR_EXTINCTION=10.**(0.4*AIRMASS(NF)*K_EXTINCTION)
              !vemos si hay alguna fila util en el frame NF
              LANY=.FALSE.
              DO I=1,NAXIS(2)
                IFROW(I)=.FALSE.
              END DO
              DO I=I1,I2
                IFROW(I)=.TRUE.
              END DO
              DO I=I1F(NF),I2F(NF)
                IF(IFROW(I)) LANY=.TRUE.
              END DO
              !si hay alguna fila util en el frame NF, leemos el bundle
              IF(LANY)THEN
                ISTATUS=0
                IREADWRITE=0
                CALL FTOPEN(80,FILE_FRAME_SKYSUB(NF),
     +           IREADWRITE,BLOCKSIZE,ISTATUS)
                DO K=K1,K2
                  I=I1-II_FRAME(NF)+(K-K1)
                  IF((I.GE.1).AND.(I.LE.NAXISF2))THEN
                    FIRSTPIX=(I-1)*NAXISF1+1
                    CALL FTGPFE(80,1,FIRSTPIX,NAXISF(1,NF),FROW,
     +               LROW,ANYNULL,ISTATUS)
                    DO J=1,NAXISF1
                      !si el pixel no esta mascarado en el propio frame
                      IF(.NOT.MASK_FRAME(J,I))THEN
                        STACK_DATA(J+JJ_FRAME(NF),K)=
     +                   FROW(J)*FACTOR_EXTINCTION
                        STACK_IFPIXEL(J+JJ_FRAME(NF),K)=.TRUE.
                      END IF
                    END DO
                  END IF
                END DO
                CALL FTCLOS(80,ISTATUS)
                IF(ISTATUS.GT.0) CALL PRINTERROR(ISTATUS)
              END IF
            END IF
          END DO
          !rellenamos el stack no utilizado con ceros
          IF(NBUNDLE*NFRAMES.NE.NMAXFRAMES)THEN
            DO K=NBUNDLE*NFRAMES+1,NMAXFRAMES
              DO J=1,NAXIS(1)
                STACK_DATA(J,K)=0.0
                STACK_IFPIXEL(J,K)=.FALSE.
              END DO
            END DO
          END IF
          !ya hemos generado el stack; ahora lo dibujamos
          NX1=1
          NX2=NAXIS(1)
          NY1=1
          NY2=NMAXFRAMES
          XMIN=REAL(NX1)-0.6
          XMAX=REAL(NX2)+0.6
          YMIN=REAL(NY1)-0.6
          YMAX=REAL(NY2)+0.6
          CALL PGSUBP(1,1)
          CALL PGSVP(0.07,0.95,0.57,0.95)
          CALL PGSWIN(XMIN,XMAX,YMIN,YMAX)
          CALL PGSCI(5)
          CALL PGBOX('BCTSNI',0.0,0,'BCTSNI',0.0,0)
          CALL PGSCI(7)
          CALL PGLABEL('X axis','Y axis',' ')
          CALL PGSCI(1)
          CALL PGGRAY(STACK_DATA,NXMAX,NMAXFRAMES,
     +     NX1,NX2,NY1,NY2,FG,BG,TR)
          CALL PGSUBP(2,2)
          !hacemos estadistica para cada pixel en el stack
          DO I=I1,I2
            DO J=1,NAXIS(1)
              NXX=0
              DO NF=1,NFRAMES
                K=(NF-1)*NBUNDLE+1+(I-I1)
                IF(STACK_IFPIXEL(J,K))THEN
                  NXX=NXX+1
                  XX(NXX)=STACK_DATA(J,K)
                END IF
              END DO
              IF(NXX.EQ.0)THEN
                IMAGEN_FINAL(J,I)=0.0
                IMAGEN_FINAL_RMS(J,I)=0.0
                IMAGEN_FINAL_NPIXELS(J,I)=0.0
              ELSEIF(NXX.EQ.1)THEN
                IMAGEN_FINAL(J,I)=XX(1)
                IMAGEN_FINAL_RMS(J,I)=0.0
                IMAGEN_FINAL_NPIXELS(J,I)=1.0
              ELSEIF(NXX.EQ.2)THEN
                IMAGEN_FINAL(J,I)=(XX(1)+XX(2))/2.0
                IMAGEN_FINAL_RMS(J,I)=ABS(XX(1)-XX(2))/1.41421356
                IMAGEN_FINAL_NPIXELS(J,I)=2.0
!             ELSEIF(NXX.EQ.3)THEN !eliminamos el mas brillante
!               CALL ORDENA1F(NXX,XX)
!               IMAGEN_FINAL(J,I)=(XX(1)+XX(2))/2.0
!               IMAGEN_FINAL_RMS(J,I)=ABS(XX(1)-XX(2))/1.41421356
!               IMAGEN_FINAL_NPIXELS(J,I)=2.0
              ELSEIF(NXX.EQ.3)THEN !nos quedamos con el central (mediana)
                CALL ORDENA1F(NXX,XX)
                IMAGEN_FINAL(J,I)=XX(2)
                IMAGEN_FINAL_RMS(J,I)=ABS(XX(1)-XX(3))/1.41421356
                IMAGEN_FINAL_NPIXELS(J,I)=3.0
              ELSE
                IF(CMERGE.EQ.'1')THEN
                  XX_MEDIAN=FMEDIAN1MAD(NXX,XX,XX_SIGMA)
                  IMAGEN_FINAL(J,I)=XX_MEDIAN
                  IMAGEN_FINAL_RMS(J,I)=XX_SIGMA
                  IMAGEN_FINAL_NPIXELS(J,I)=REAL(NXX)
                ELSE
                  ! PASOS A SEGUIR
                  ! 1) calculamos el histograma eliminado los IFRACTION_REMOVE 
                  !    pixels con menos y con mas sen~al
                  ! 2) determinamos la media y la sigma del resto de pixels
                  ! 3) repetimos la estadistica eliminando puntos a +/- TSIGMA
                  !    sigma; este ultimo valor de MEAN calculado sera el
                  !    asignado a la imagen combinada
                  IFRACTION_REMOVE=NINT(FRACTION_REMOVE*REAL(NXX))
                  IF(IFRACTION_REMOVE.LT.1) IFRACTION_REMOVE=1
                  IF(NXX-2*IFRACTION_REMOVE.LT.3)THEN
                    IFRACTION_REMOVE=(NXX-3)/2
                  END IF
                  CALL ORDENA1F(NXX,XX)
                  NXX_EFF=NXX-2*IFRACTION_REMOVE
                  DO K=1,NXX_EFF
                    XX_EFF(K)=XX(K+IFRACTION_REMOVE)
                  END DO
                  IF(NXX_EFF.GT.0)THEN
                    XX_MEAN=FMEAN0(NXX_EFF,XX_EFF,XX_SIGMA)
                  ELSE
                    XX_MEAN=0.0
                  END IF
                  NXX_EFF=0
                  DO K=1,NXX
                    IF(ABS(XX(K)-XX_MEAN).LT.TSIGMA_REMOVE*XX_SIGMA)THEN
                      NXX_EFF=NXX_EFF+1
                      XX_EFF(NXX_EFF)=XX(K)
                    END IF
                  END DO
                  IF(NXX_EFF.GT.0)THEN
                    XX_MEAN=FMEAN0(NXX_EFF,XX_EFF,XX_SIGMA)
                  ELSE
                    XX_MEAN=0.0
                  END IF
                  IMAGEN_FINAL(J,I)=XX_MEAN
                  IMAGEN_FINAL_RMS(J,I)=XX_SIGMA
                  IMAGEN_FINAL_NPIXELS(J,I)=REAL(NXX_EFF)
                END IF
              END IF
            END DO
          END DO
          !dibujamos imagen final hasta el momento
          NX1=1
          NX2=NAXIS(1)
          NY1=1
          NY2=NAXIS(2)
          XMIN=REAL(NX1)-0.6
          XMAX=REAL(NX2)+0.6
          YMIN=REAL(NY1)-0.6
          YMAX=REAL(NY2)+0.6
          CALL PGPANL(1,2)
          CALL PGENV(XMIN,XMAX,YMIN,YMAX,0,-2)
          CALL PGSCI(5)
          CALL PGBOX('BCTSNI',0.0,0,'BCTSNI',0.0,0)
          CALL PGSCI(7)
          CALL PGLABEL('X axis','Y axis',' ')
          CALL PGSCI(1)
          CALL PGGRAY(IMAGEN_FINAL_NPIXELS,
     +     NXMAX,NYMAX,NX1,NX2,NY1,NY2,REAL(NFRAMES),0.0,TR)
          CALL PGPANL(2,1)
          CALL PGENV(XMIN,XMAX,YMIN,YMAX,0,-2)
          CALL PGSCI(5)
          CALL PGBOX('BCTSNI',0.0,0,'BCTSNI',0.0,0)
          CALL PGSCI(7)
          CALL PGLABEL('X axis','Y axis',' ')
          CALL PGSCI(1)
          CALL PGGRAY(IMAGEN_FINAL,
     +     NXMAX,NYMAX,NX1,NX2,NY1,NY2,FG,BG,TR)
          !incrementamos I1 e I2
          IF(I2.EQ.NAXIS(2)) LOOP=.FALSE.
          IF(LOOP)THEN
            I1=I2+1
          END IF
        END DO
C------------------------------------------------------------------------------
        SIMPLE=.TRUE.
        BITPIX=-32
        EXTEND=.FALSE.
        BLOCKSIZE=1
C salvamos imagen de datos
        FILE_DATA=READC('Output file name for combined data image...',
     +   FILE_DATA,'@')
        INQUIRE(FILE=FILE_DATA,EXIST=LOGFILE)
        IF(LOGFILE)THEN
          L2=TRUELEN(FILE_DATA)
          ISYSTEM=SYSTEMFUNCTION('rm -f '//FILE_DATA(1:L2))
        END IF
        CALL FTINIT(80,FILE_DATA,BLOCKSIZE,ISTATUS)
        CALL FTPHPR(80,SIMPLE,BITPIX,2,NAXIS,0,1,EXTEND,ISTATUS)
        CALL FTPKYF(80,'EXPTIME',EXPTIME_CHECK(1),2,
     +   'Total Exposure Time [secs]',ISTATUS)
        CALL FTP2DE(80,1,NXMAX,NAXIS(1),NAXIS(2),IMAGEN_FINAL,ISTATUS)
        CALL FTCLOS(80,ISTATUS)
        IF(ISTATUS.GT.0) CALL PRINTERROR(ISTATUS)
C salvamos imagen de errores
        FILE_RMS=READC('Output file name for r.m.s. image..........',
     +   FILE_RMS,'@')
        INQUIRE(FILE=FILE_RMS,EXIST=LOGFILE)
        IF(LOGFILE)THEN
          L2=TRUELEN(FILE_RMS)
          ISYSTEM=SYSTEMFUNCTION('rm -f '//FILE_RMS(1:L2))
        END IF
        CALL FTINIT(80,FILE_RMS,BLOCKSIZE,ISTATUS)
        CALL FTPHPR(80,SIMPLE,BITPIX,2,NAXIS,0,1,EXTEND,ISTATUS)
        CALL FTP2DE(80,1,NXMAX,NAXIS(1),NAXIS(2),IMAGEN_FINAL_RMS,
     +   ISTATUS)
        CALL FTCLOS(80,ISTATUS)
        IF(ISTATUS.GT.0) CALL PRINTERROR(ISTATUS)
C salvamos imagen con numero de pixels utilizados
        FILE_NUMBER_PIXELS=
     +   READC('Output file name for number of pixels image',
     +   FILE_NUMBER_PIXELS,'@')
        INQUIRE(FILE=FILE_NUMBER_PIXELS,EXIST=LOGFILE)
        IF(LOGFILE)THEN
          L2=TRUELEN(FILE_NUMBER_PIXELS)
          ISYSTEM=SYSTEMFUNCTION('rm -f '//FILE_NUMBER_PIXELS(1:L2))
        END IF
        CALL FTINIT(80,FILE_NUMBER_PIXELS,BLOCKSIZE,ISTATUS)
        CALL FTPHPR(80,SIMPLE,BITPIX,2,NAXIS,0,1,EXTEND,ISTATUS)
        CALL FTP2DE(80,1,NXMAX,NAXIS(1),NAXIS(2),IMAGEN_FINAL_NPIXELS,
     +   ISTATUS)
        CALL FTCLOS(80,ISTATUS)
        IF(ISTATUS.GT.0) CALL PRINTERROR(ISTATUS)
        WRITE(*,*)
        WRITE(*,100) '>>> END OF STEP '
        WRITE(*,101) '(5) combine sky-subtracted (raw-dark)/superflat'//
     +   ' frames'
C
        IF(CRUN_THROUGH.EQ.'n') GOTO 40
C------------------------------------------------------------------------------
C******************************************************************************
C------------------------------------------------------------------------------
C (6) compute r.m.s. versus number of frames
60      CONTINUE
        WRITE(*,*)
        WRITE(*,100) '>>> STEP '
        WRITE(*,101) '(6) compute r.m.s. versus number of frames'
        WRITE(*,*)
        ISTATUS=0
        IREADWRITE=0
        ANYNULL=.FALSE.
c leemos imagen de datos
        FILE_DATA=READC('Input file name for combined data image...',
     +   FILE_DATA,'@')
        CALL READFITS(FILE_DATA,NXMAX,NYMAX,NAXIS(1),NAXIS(2),
     +   IMAGEN_FINAL,.TRUE.,EXPTIME)
c leemos imagen con numero de pixels utilizados
        FILE_NUMBER_PIXELS=
     +   READC('Input file name for number of pixels image',
     +   FILE_NUMBER_PIXELS,'@')
        CALL READFITS(FILE_NUMBER_PIXELS,NXMAX,NYMAX,NAXIS(1),NAXIS(2),
     +   IMAGEN_FINAL_NPIXELS,.TRUE.,EXPTIME)
c calculamos r.m.s. en el cielo en funcion del numero de frames utilizado
        WRITE(CDUMMY,*) FRACTION_REMOVE
        FRACTION_REMOVE=
     +   READF('Fraction of pixels to be removed in histograms',CDUMMY)
        WRITE(CDUMMY,*) TSIGMA_REMOVE
        TSIGMA_REMOVE=READF('Times sigma to remove pixels',CDUMMY)
        DO NF=1,NFRAMES
          K=0
          DO I=1,NAXIS(2)
            DO J=1,NAXIS(1)
              IF(.NOT.MASK_IMAGEN_FINAL(J,I))THEN !eliminamos objetos
                IF(NINT(IMAGEN_FINAL_NPIXELS(J,I)).EQ.NF)THEN
                  K=K+1
                  PIXEL(K)=IMAGEN_FINAL(J,I)
                END IF
              END IF
            END DO
          END DO
          IF(K.GT.3)THEN
            IFRACTION_REMOVE=NINT(FRACTION_REMOVE*REAL(K))
            CALL ORDENA1F(K,PIXEL)
            PIXEL_FMEAN=
     +       FMEAN0(K-2*IFRACTION_REMOVE,PIXEL(IFRACTION_REMOVE+1),
     +       PIXEL_FSIGMA)
            K_=0
            DO KK=1,K
              IF(ABS(PIXEL(KK)-PIXEL_FMEAN).LT.
     +         TSIGMA_REMOVE*PIXEL_FSIGMA)THEN
                K_=K_+1
                PIXEL(K_)=PIXEL(KK) !esta asignacion no destruye la informacion
                                    !en PIXEL porque la variable KK avanza mas
                                    !rapidamente que la variable K_
              END IF
            END DO
            IF(K_.GT.3)THEN
              PIXEL_FMEAN=FMEAN0(K_,PIXEL,PIXEL_FSIGMA)
              NPIXPERPIX(NF)=K_
              MEANPERPIX(NF)=PIXEL_FMEAN
              SIGMAPERPIX(NF)=PIXEL_FSIGMA
            ELSE
              NPIXPERPIX(NF)=0
              MEANPERPIX(NF)=0.0
              SIGMAPERPIX(NF)=0.0
            END IF
          ELSE
            NPIXPERPIX(NF)=0
            MEANPERPIX(NF)=0.0
            SIGMAPERPIX(NF)=0.0
          END IF
          WRITE(*,100) '#'
          IF(MOD(NF,10).EQ.0) WRITE(*,*) NF
        END DO
        WRITE(*,*)
c salvamos fichero
        FILE_RMS_NUMBER=READC('Output file for sky r.m.s. versus '//
     +    'number of frames',FILE_RMS_NUMBER,'@')
        INQUIRE(FILE=FILE_RMS_NUMBER,EXIST=LOGFILE)
        IF(LOGFILE)THEN
          L2=TRUELEN(FILE_RMS_NUMBER)
          ISYSTEM=SYSTEMFUNCTION('rm -f '//FILE_RMS_NUMBER(1:L2))
        END IF
        OPEN(10,FILE=FILE_RMS_NUMBER,STATUS='NEW',FORM='FORMATTED')
        DO NF=1,NFRAMES
          WRITE(10,*) NF,NPIXPERPIX(NF),MEANPERPIX(NF),SIGMAPERPIX(NF)
        END DO
        CLOSE(10)
        WRITE(*,*)
        WRITE(*,100) '>>> END OF STEP '
        WRITE(*,101) '(6) compute r.m.s. versus number of frames'
C
        IF(CRUN_THROUGH.EQ.'n') GOTO 40
C------------------------------------------------------------------------------
C******************************************************************************
C------------------------------------------------------------------------------
C (7) read r.m.s. vs number of frames file, create:
C     - fake noise image
C     - mask of objects
70      CONTINUE
        WRITE(*,*)
        WRITE(*,100) '>>> STEP '
        WRITE(*,101) '(7) read r.m.s. vs number of frames file, create:'
        WRITE(*,101) '     - fake noise image'
        WRITE(*,101) '     - mask of objects'
        WRITE(*,*)
c leemos fichero con r.m.s. frente a numero de frames
        LOOP=.TRUE.
        DO WHILE(LOOP)
          FILE_RMS_NUMBER=READC('Input file for sky r.m.s. versus '//
     +     'number of frames',FILE_RMS_NUMBER,'@')
          INQUIRE(FILE=FILE_RMS_NUMBER,EXIST=LOGFILE)
          IF(.NOT.LOGFILE)THEN
            WRITE(*,101) 'ERROR: this file does not exist. Try again.'
            WRITE(*,100) 'Press <CR> to continue...'
            READ(*,*)
          ELSE
            LOOP=.FALSE.
          END IF
        END DO
        OPEN(10,FILE=FILE_RMS_NUMBER,STATUS='OLD',FORM='FORMATTED')
        DO NF=1,NFRAMES
          READ(10,*) IDUM,NPIXPERPIX(NF),MEANPERPIX(NF),SIGMAPERPIX(NF)
        END DO
        CLOSE(10)
c dibujamos resultados
        CALL PGSUBP(1,1)
        CALL PGERAS
        K=0
        DSUM1=0.D0
        DSUM1_=0.D0
        DSUM2=0.D0
        DO NF=1,NFRAMES
          IF(NPIXPERPIX(NF).GT.3)THEN
            K=K+1
            XP(K)=REAL(NF)
            YP(K)=SIGMAPERPIX(NF)
            YP2(K)=YP(K)*SQRT(XP(K))
            YP3(K)=MEANPERPIX(NF)
            DSUM1=DSUM1+DBLE(YP2(K))*DBLE(NPIXPERPIX(NF))
            DSUM1_=DSUM1_+DBLE(YP3(K))*DBLE(NPIXPERPIX(NF))
            DSUM2=DSUM2+DBLE(NPIXPERPIX(NF))
          END IF
        END DO
        IF(K.EQ.0)THEN
          WRITE(*,101) 'ERROR: number of regions with more than'
          WRITE(*,101) '3 pixels/frame = 0!'
          WRITE(*,100) 'Press <CR> to continue...'
          READ(*,*)
          GOTO 40
        END IF
        RMS_1FRAME=REAL(DSUM1/DSUM2)
        MEAN_ALLFRAME=REAL(DSUM1_/DSUM2)
        DO K_=1,K
          YP1(K_)=RMS_1FRAME/SQRT(XP(K_))
        END DO
        XMIN=1.0
        XMAX=REAL(NFRAMES)
        DX=XMAX-XMIN
        XMIN=XMIN-DX/20.
        XMAX=XMAX+DX/20.
        CALL FINDMML(K,1,K,YP,YMIN,YMAX)
        DY=YMAX-YMIN
        YMIN=YMIN-DY/20.
        YMAX=YMAX+DY/20.
        CALL PGSVP(0.10,0.95,0.65,0.90)
        CALL PGSWIN(XMIN,XMAX,YMIN,YMAX)
        CALL PGBOX('BCTS',0.0,0,'BCTSN',0.0,0)
        CALL PGLABEL(' ',' measured sky r.m.s.',' ')
        CALL PGSCI(3)
        CALL PGPOINT(K,XP,YP,17)
        CALL PGSCI(7)
        CALL PGSLS(2)
        CALL PGLINE(K,XP,YP1)
        CALL PGSLS(1)
        CALL PGSCI(1)
        CALL FINDMML(K,1,K,YP2,YMIN,YMAX)
        DY=YMAX-YMIN
        YMIN=YMIN-DY/20.
        YMAX=YMAX+DY/20.
        CALL PGSVP(0.10,0.95,0.40,0.65)
        CALL PGSWIN(XMIN,XMAX,YMIN,YMAX)
        CALL PGBOX('BCTS',0.0,0,'BCTSN',0.0,0)
        CALL PGLABEL(' ',' scaled sky r.m.s.',' ')
        CALL PGSCI(2)
        CALL PGPOINT(K,XP,YP2,17)
        CALL PGSCI(7)
        CALL PGSLS(2)
        CALL PGMOVE(XMIN,RMS_1FRAME)
        CALL PGDRAW(XMAX,RMS_1FRAME)
        CALL PGSLS(1)
        CALL PGSCI(1)
        CALL FINDMML(K,1,K,YP3,YMIN,YMAX)
        DY=YMAX-YMIN
        YMIN=YMIN-DY/20.
        YMAX=YMAX+DY/20.
        CALL PGSVP(0.10,0.95,0.15,0.40)
        CALL PGSWIN(XMIN,XMAX,YMIN,YMAX)
        CALL PGBOX('BCTSN',0.0,0,'BCTSN',0.0,0)
        CALL PGLABEL('number of frames/pixel','mean sky signal',' ')
        CALL PGSCI(5)
        CALL PGPOINT(K,XP,YP3,17)
        CALL PGSCI(1)
c leemos imagen con numero de pixels utilizados
        FILE_NUMBER_PIXELS=
     +   READC('Input file name for number of pixels image',
     +   FILE_NUMBER_PIXELS,'@')
        CALL READFITS(FILE_NUMBER_PIXELS,NXMAX,NYMAX,NAXIS(1),NAXIS(2),
     +   IMAGEN_FINAL_NPIXELS,.TRUE.,EXPTIME)
c dibujamos imagen con numero de pixels
        NX1=1
        NX2=NAXIS(1)
        NY1=1
        NY2=NAXIS(2)
        XMIN=REAL(NX1)-0.6
        XMAX=REAL(NX2)+0.6
        YMIN=REAL(NY1)-0.6
        YMAX=REAL(NY2)+0.6
        BG=0.0
        FG=REAL(NFRAMES)
        CALL PGENV(XMIN,XMAX,YMIN,YMAX,1,-2)
        CALL PGSCI(5)
        CALL PGBOX('BCTSNI',0.0,0,'BCTSNI',0.0,0)
        CALL PGSCI(7)
        CALL PGLABEL('X axis','Y axis',' ')
        CALL PGSCI(1)
        CALL PGGRAY(IMAGEN_FINAL_NPIXELS,
     +   NXMAX,NYMAX,NX1,NX2,NY1,NY2,FG,BG,TR)
c creamos fake error image
        WRITE(*,100) 'Creating fake sky error image...'
        NSEED=-1
        DO I=1,NAXIS(2)
          DO J=1,NAXIS(1)
            IF(IMAGEN_FINAL_NPIXELS(J,I).GT.0.0)THEN
              R1=RANDOMNUMBER(NSEED)
              R2=RANDOMNUMBER(NSEED)
              IMAGEN_FINAL(J,I)=
     +         1.41421356*RMS_1FRAME/SQRT(IMAGEN_FINAL_NPIXELS(J,I))*
     +         SQRT(-1.*LOG(1.-R1))*COS(2.*PI*R2)
            ELSE
              IMAGEN_FINAL(J,I)=0.0
            END IF
          END DO
        END DO
        WRITE(*,101) 'OK!'
        WRITE(*,100) 'Press <CR> to display fake sky error image...'
        READ(*,*)
        IF(LECHO) WRITE(*,*)
c dibujamos fake error image
        BG=MEAN_ALLFRAME-3.*RMS_1FRAME
        FG=MEAN_ALLFRAME+3.*RMS_1FRAME
        CALL PGENV(XMIN,XMAX,YMIN,YMAX,1,-2)
        CALL PGSCI(5)
        CALL PGBOX('BCTSNI',0.0,0,'BCTSNI',0.0,0)
        CALL PGSCI(7)
        CALL PGLABEL('X axis','Y axis',' ')
        CALL PGSCI(1)
        CALL PGGRAY(IMAGEN_FINAL,
     +   NXMAX,NYMAX,NX1,NX2,NY1,NY2,FG,BG,TR)
c salvamos fake error image
        CSAVE(1:1)=
     +   READC('Are you saving the fake sky error image (y/n)',
     +   CSAVE,'yn')
        IF(CSAVE.EQ.'y')THEN
          SIMPLE=.TRUE.
          BITPIX=-32
          EXTEND=.FALSE.
          BLOCKSIZE=1
          FILE_IMAGE_FAKE_ERROR=
     +     READC('Output file name for fake sky error image',
     +     FILE_IMAGE_FAKE_ERROR,'@')
          L2=TRUELEN(FILE_IMAGE_FAKE_ERROR)
          INQUIRE(FILE=FILE_IMAGE_FAKE_ERROR,EXIST=LOGFILE)
          IF(LOGFILE)THEN
            ISYSTEM=SYSTEMFUNCTION('rm -f '//
     +       FILE_IMAGE_FAKE_ERROR(1:L2))
          END IF
          CALL FTINIT(80,FILE_IMAGE_FAKE_ERROR,BLOCKSIZE,ISTATUS)
          CALL FTPHPR(80,SIMPLE,BITPIX,2,NAXIS,0,1,EXTEND,ISTATUS)
          CALL FTP2DE(80,1,NXMAX,NAXIS(1),NAXIS(2),IMAGEN_FINAL,
     +     ISTATUS)
          CALL FTCLOS(80,ISTATUS)
          IF(ISTATUS.GT.0) CALL PRINTERROR(ISTATUS)
        END IF
c leemos imagen de datos
        FILE_DATA=READC('Input file name for combined data image',
     +   FILE_DATA,'@')
        CALL READFITS(FILE_DATA,NXMAX,NYMAX,NAXIS(1),NAXIS(2),
     +   IMAGEN_FINAL,.TRUE.,EXPTIME)
c dibujamos imagen de datos con los mismos cortes que la fake error image
        LOOP=.TRUE.
        LFIRSTPLOT=.TRUE.
        DO WHILE(LOOP)
          IF(LFIRSTPLOT)THEN
            LFIRSTPLOT=.FALSE.
          ELSE
            CALL PGENV(XMIN,XMAX,YMIN,YMAX,1,-2)
            CALL PGSCI(5)
            CALL PGBOX('BCTSNI',0.0,0,'BCTSNI',0.0,0)
            CALL PGSCI(7)
            CALL PGLABEL('X axis','Y axis',' ')
            CALL PGSCI(1)
          END IF
          CALL PGGRAY(IMAGEN_FINAL,
     +     NXMAX,NYMAX,NX1,NX2,NY1,NY2,FG,BG,TR)
          WRITE(*,*)
          WRITE(*,101) '0: exit'
          WRITE(*,101) '1: change BG/FG'
          WRITE(*,101) '2: change plot limits'
          ISUBMENU=READILIM('Option','0',0,2)
          IF(ISUBMENU.EQ.0)THEN
            LOOP=.FALSE.
          ELSEIF(ISUBMENU.EQ.1)THEN
            WRITE(CDUMMY,*) BG
            BG=READF('Background',CDUMMY)
            WRITE(CDUMMY,*) FG
            FG=READF('Foreground',CDUMMY)
          ELSEIF(ISUBMENU.EQ.2)THEN
            WRITE(CDUMMY,*) NX1
            NX1=READILIM('New NX1',CDUMMY,1,NAXIS(1))
            WRITE(CDUMMY,*) NX2
            NX2=READILIM('New NX2',CDUMMY,NX1,NAXIS(1))
            WRITE(CDUMMY,*) NY1
            NY1=READILIM('New NY1',CDUMMY,1,NAXIS(2))
            WRITE(CDUMMY,*) NY2
            NY2=READILIM('New NY2',CDUMMY,NY1,NAXIS(2))
            XMIN=REAL(NX1)-0.6
            XMAX=REAL(NX2)+0.6
            YMIN=REAL(NY1)-0.6
            YMAX=REAL(NY2)+0.6
          END IF
        END DO
c..............................................................................
c generamos máscara de objetos usando SExtractor
        FILE_MASK_IMAGEN=
     +   READC('Output file name for mask in combined image',
     +   FILE_MASK_IMAGEN,'@')
        INQUIRE(FILE=FILE_MASK_IMAGEN,EXIST=LOGFILE)
        IF(LOGFILE)THEN
          L2=TRUELEN(FILE_MASK_IMAGEN)
          ISYSTEM=SYSTEMFUNCTION('rm -f '//FILE_MASK_IMAGEN(1:L2))
        END IF
        FILE_MASK_IMAGEN_CAT=
     +   READC('Output file name for catalogue with objects',
     +   FILE_MASK_IMAGEN_CAT,'@')
        INQUIRE(FILE=FILE_MASK_IMAGEN_CAT,EXIST=LOGFILE)
        IF(LOGFILE)THEN
          L2=TRUELEN(FILE_MASK_IMAGEN_CAT)
          ISYSTEM=SYSTEMFUNCTION('rm -f '//FILE_MASK_IMAGEN_CAT(1:L2))
        END IF
c definimos un aumento artificial del r.m.s. en los bordes para evitar
c detecciones espureas (nota: usamos el array IMAGEN_FINAL_NPIXELS para
c definir el aumento del r.m.s. pixel a pixel, y evitar consumo excesivo de 
c memoria definiendo un nuevo array)
        WRITE(*,*)
        WRITE(*,101) 'Note: you can avoid spurious detections by '//
     +   'increasing the local rms.'
        CBORDER(1:1)=
     +   READC('Are you increasing the rms in image border (y/n)'
     +   ,'n','yn')
        IF(CBORDER.EQ.'y')THEN
          NX1=1
          NX2=NAXIS(1)
          NY1=1
          NY2=NAXIS(2)
          FINCREASE=READF('Factor to be used in the border (factor>1)',
     +     '5.0')
          WRITE(*,101) 'Define the useful region rectangle:'
          WRITE(CDUMMY,*) NX1
          NX1=READILIM('New NX1',CDUMMY,1,NAXIS(1))
          WRITE(CDUMMY,*) NX2
          NX2=READILIM('New NX2',CDUMMY,NX1,NAXIS(1))
          WRITE(CDUMMY,*) NY1
          NY1=READILIM('New NY1',CDUMMY,1,NAXIS(2))
          WRITE(CDUMMY,*) NY2
          NY2=READILIM('New NY2',CDUMMY,NY1,NAXIS(2))
          !inicializamos a uno
          DO I=1,NAXIS(2)
            DO J=1,NAXIS(1)
              IMAGEN_FINAL_NPIXELS(J,I)=1.0
            END DO
          END DO
          !generamos campana de coseno
          IF(NX1.GT.1)THEN
            NL=NX1
            DO I=1,NAXIS(2)
              DO J=1,NX1
                IMAGEN_FINAL_NPIXELS(J,I)=IMAGEN_FINAL_NPIXELS(J,I)*
     +           0.5*(1.-COS(PI*REAL(J)/REAL(NL)))
              END DO
            END DO
          END IF
          IF(NX2.LT.NAXIS(1))THEN
            NL=NAXIS(1)-NX2
            DO I=1,NAXIS(2)
              DO J=NX2,NAXIS(1)
                IMAGEN_FINAL_NPIXELS(J,I)=IMAGEN_FINAL_NPIXELS(J,I)*
     +           0.5*(1.-COS(PI*REAL(NAXIS(1)-J)/REAL(NL)))
              END DO
            END DO
          END IF
          IF(NY1.GT.1)THEN
            NL=NY1
            DO I=1,NY1
              DO J=1,NAXIS(1)
                IMAGEN_FINAL_NPIXELS(J,I)=IMAGEN_FINAL_NPIXELS(J,I)*
     +           0.5*(1.-COS(PI*REAL(I)/REAL(NL)))
              END DO
            END DO
          END IF
          IF(NY2.LT.NAXIS(2))THEN
            NL=NAXIS(2)-NY2
            DO I=NY2,NAXIS(2)
              DO J=1,NAXIS(1)
                IMAGEN_FINAL_NPIXELS(J,I)=IMAGEN_FINAL_NPIXELS(J,I)*
     +           0.5*(1.-COS(PI*REAL(NAXIS(2)-I)/REAL(NL)))
              END DO
            END DO
          END IF
          !invertimos la campana de coseno y re-escalamos
          DO I=1,NAXIS(2)
            DO J=1,NAXIS(1)
              IF((J.LE.NX1).OR.(J.GE.NX2).OR.(I.LE.NY1).OR.
     +         (I.GE.NY2))THEN
                IMAGEN_FINAL_NPIXELS(J,I)=1.0+
     +           (FINCREASE-1.0)*(1.0-IMAGEN_FINAL_NPIXELS(J,I))
              END IF
            END DO
          END DO
          !salvamos imagen temporal (weigths4rms.fits)
          INQUIRE(FILE='weights4rms.fits',EXIST=LOGFILE)
          IF(LOGFILE)THEN
            ISYSTEM=SYSTEMFUNCTION('rm -f weights4rms.fits')
          END IF
          SIMPLE=.TRUE.
          BITPIX=-32
          EXTEND=.FALSE.
          BLOCKSIZE=1
          CALL FTINIT(80,'weights4rms.fits',BLOCKSIZE,ISTATUS)
          CALL FTPHPR(80,SIMPLE,BITPIX,2,NAXIS,0,1,EXTEND,ISTATUS)
          CALL FTP2DE(80,1,NXMAX,NAXIS(1),NAXIS(2),IMAGEN_FINAL_NPIXELS,
     +     ISTATUS)
          CALL FTCLOS(80,ISTATUS)
          IF(ISTATUS.GT.0) CALL PRINTERROR(ISTATUS)
        END IF
c copiamos los ficheros necesarios para ejecutar SExtractor si éstos no existen
        L1=TRUEBEG(IMCOMBINE_DIR_)
        L2=TRUELEN(IMCOMBINE_DIR_)
        INQUIRE(FILE='default.conv',EXIST=LOGFILE)
        IF(.NOT.LOGFILE)THEN
          ISYSTEM=SYSTEMFUNCTION('cp -f '//IMCOMBINE_DIR_(L1:L2)//
     +     'SExtractor/default.conv .')
        END IF
        INQUIRE(FILE='default.nnw',EXIST=LOGFILE)
        IF(.NOT.LOGFILE)THEN
          ISYSTEM=SYSTEMFUNCTION('cp -f '//IMCOMBINE_DIR_(L1:L2)//
     +     'SExtractor/default.nnw .')
        END IF
        INQUIRE(FILE='default.param',EXIST=LOGFILE)
        IF(.NOT.LOGFILE)THEN
          ISYSTEM=SYSTEMFUNCTION('cp -f '//IMCOMBINE_DIR_(L1:L2)//
     +     'SExtractor/default.param .')
        END IF
        INQUIRE(FILE='default.sex',EXIST=LOGFILE)
        IF(.NOT.LOGFILE)THEN
          ISYSTEM=SYSTEMFUNCTION('cp -f '//IMCOMBINE_DIR_(L1:L2)//
     +     'SExtractor/default.sex .')
        END IF
c ejecutamos SExtractor
        L1=TRUEBEG(SEXPATH_)
        L2=TRUELEN(SEXPATH_)
        LL1=TRUEBEG(FILE_DATA)
        LL2=TRUELEN(FILE_DATA)
        LLL1=TRUEBEG(FILE_MASK_IMAGEN_CAT)
        LLL2=TRUELEN(FILE_MASK_IMAGEN_CAT)
        LLLL1=TRUEBEG(FILE_MASK_IMAGEN)
        LLLL2=TRUELEN(FILE_MASK_IMAGEN)
        IF(CBORDER.EQ.'n')THEN !sólo ejecutamos una vez
          WRITE(*,*)
          WRITE(*,101) '>>> Executing:'
          WRITE(*,101) SEXPATH_(L1:L2)//'sex '//
     +     FILE_DATA(LL1:LL2)//
     +     ' -CATALOG_NAME '//FILE_MASK_IMAGEN_CAT(LLL1:LLL2)//
     +     ' -CHECKIMAGE_NAME '//FILE_MASK_IMAGEN(LLLL1:LLLL2)//
     +     ' -CHECKIMAGE_TYPE SEGMENTATION'
          ISYSTEM=SYSTEMFUNCTION(SEXPATH_(L1:L2)//'sex '//
     +     FILE_DATA(LL1:LL2)//
     +     ' -CATALOG_NAME '//FILE_MASK_IMAGEN_CAT(LLL1:LLL2)//
     +     ' -CHECKIMAGE_NAME '//FILE_MASK_IMAGEN(LLLL1:LLLL2)//
     +     ' -CHECKIMAGE_TYPE SEGMENTATION')
        ELSE !ejecutamos dos veces
          !1) calculamos imagen de r.m.s. (generamos fichero map_rms.fits)
          WRITE(*,*)
          WRITE(*,101) '>>> Executing:'
          WRITE(*,101) SEXPATH_(L1:L2)//'sex '//
     +     FILE_DATA(LL1:LL2)//
     +     ' -CATALOG_NAME '//FILE_MASK_IMAGEN_CAT(LLL1:LLL2)//
     +     ' -CHECKIMAGE_NAME map_rms.fits'//
     +     ' -CHECKIMAGE_TYPE BACKGROUND_RMS'
          ISYSTEM=SYSTEMFUNCTION(SEXPATH_(L1:L2)//'sex '//
     +     FILE_DATA(LL1:LL2)//
     +     ' -CATALOG_NAME '//FILE_MASK_IMAGEN_CAT(LLL1:LLL2)//
     +     ' -CHECKIMAGE_NAME map_rms.fits'//
     +     ' -CHECKIMAGE_TYPE BACKGROUND_RMS')
          !2) aumentamos los bordes de la imagen map_rms.fits (nota: usamos
          !temporalmente los arrays IMAGEN_FINAL e IMAGEN_FINAL_NPIXELS para 
          !ahorrar !memoria)
          CALL MULTIPLICA('map_rms.fits','weights4rms.fits',
     +     'map_rms_weighted.fits',IMAGEN_FINAL,IMAGEN_FINAL_NPIXELS)
          !3) generamos máscara de objetos pesando con map_rms.fits
          WRITE(*,*)
          WRITE(*,101) '>>> Executing:'
          WRITE(*,101) SEXPATH_(L1:L2)//'sex '//
     +     FILE_DATA(LL1:LL2)//
     +     ' -CATALOG_NAME '//FILE_MASK_IMAGEN_CAT(LLL1:LLL2)//
     +     ' -CHECKIMAGE_NAME '//FILE_MASK_IMAGEN(LLLL1:LLLL2)//
     +     ' -CHECKIMAGE_TYPE SEGMENTATION'//
     +     ' -WEIGHT_TYPE MAP_RMS -WEIGHT_IMAGE map_rms_weighted.fits'
          ISYSTEM=SYSTEMFUNCTION(SEXPATH_(L1:L2)//'sex '//
     +     FILE_DATA(LL1:LL2)//
     +     ' -CATALOG_NAME '//FILE_MASK_IMAGEN_CAT(LLL1:LLL2)//
     +     ' -CHECKIMAGE_NAME '//FILE_MASK_IMAGEN(LLLL1:LLLL2)//
     +     ' -CHECKIMAGE_TYPE SEGMENTATION'//
     +     ' -WEIGHT_TYPE MAP_RMS -WEIGHT_IMAGE map_rms_weighted.fits')
        END IF
c dibujamos objectos encontrados por SExtractor
        N_X_IMAGE=0
        N_Y_IMAGE=0
        N_A_IMAGE=0
        N_B_IMAGE=0
        N_THETA_IMAGE=0
        OPEN(44,FILE=FILE_MASK_IMAGEN_CAT,STATUS='OLD',FORM='FORMATTED')
71      READ(44,101,END=72) CLINEA
        IF(CLINEA(1:1).EQ.'#')THEN
          IF(CLINEA(7:22).EQ.'X_IMAGE') 
     +     READ(CLINEA(2:5),*) N_X_IMAGE
          IF(CLINEA(7:22).EQ.'Y_IMAGE')
     +     READ(CLINEA(2:5),*) N_Y_IMAGE
          IF(CLINEA(7:22).EQ.'A_IMAGE') 
     +     READ(CLINEA(2:5),*) N_A_IMAGE
          IF(CLINEA(7:22).EQ.'B_IMAGE') 
     +     READ(CLINEA(2:5),*) N_B_IMAGE
          IF(CLINEA(7:22).EQ.'THETA_IMAGE') 
     +     READ(CLINEA(2:5),*) N_THETA_IMAGE
          GOTO 71
        ELSE
          GOTO 72
        END IF
72      CLOSE(44)
        WRITE(*,100) 'N_X_IMAGE: '
        WRITE(*,*) N_X_IMAGE
        WRITE(*,100) 'N_Y_IMAGE: '
        WRITE(*,*) N_Y_IMAGE
        WRITE(*,100) 'N_A_IMAGE: '
        WRITE(*,*) N_A_IMAGE
        WRITE(*,100) 'N_B_IMAGE: '
        WRITE(*,*) N_B_IMAGE
        WRITE(*,100) 'N_THETA_IMAGE: '
        WRITE(*,*) N_THETA_IMAGE
        IF(N_X_IMAGE*N_Y_IMAGE*N_A_IMAGE*N_B_IMAGE*N_THETA_IMAGE.EQ.0)
     +   THEN
          STOP 'ERROR: invalid SExtractor catalogue'
        END IF
        CALL PGSCI(5)
        OPEN(44,FILE=FILE_MASK_IMAGEN_CAT,STATUS='OLD',FORM='FORMATTED')
73      READ(44,101,END=74) CLINEA
        IF(CLINEA(1:1).EQ.'#')THEN
        ELSE
          X_IMAGE=LEECOLUMN(CLINEA,N_X_IMAGE)
          Y_IMAGE=LEECOLUMN(CLINEA,N_Y_IMAGE)
          A_IMAGE=3.0*LEECOLUMN(CLINEA,N_A_IMAGE)
          B_IMAGE=3.0*LEECOLUMN(CLINEA,N_B_IMAGE)
          THETA_IMAGE=-LEECOLUMN(CLINEA,N_THETA_IMAGE)*3.141593/180.
          DO I=0,360
            THETA0=REAL(I)*3.141593/180.0
            R0=A_IMAGE*B_IMAGE/SQRT(A_IMAGE*A_IMAGE-
     +       (A_IMAGE*A_IMAGE-B_IMAGE*B_IMAGE)*
     +       COS(THETA0)*COS(THETA0))
            X0=R0*COS(THETA0)
            Y0=R0*SIN(THETA0)
            XELL(I+1)=X_IMAGE
     +       +X0*COS(THETA_IMAGE)+Y0*SIN(THETA_IMAGE)
            YELL(I+1)=Y_IMAGE
     +       -X0*SIN(THETA_IMAGE)+Y0*COS(THETA_IMAGE)
          END DO
          CALL PGLINE(361,XELL,YELL)
        END IF
        GOTO 73
74      CLOSE(44)
        CALL PGSCI(1)
C
        WRITE(*,*)
        WRITE(*,100) '>>> END OF STEP '
        WRITE(*,101) '(7) read r.m.s. vs number of frames file, create:'
        WRITE(*,101) '     - fake noise image'
        WRITE(*,101) '     - mask of objects'
c..............................................................................
c regresamos al menu principal
        IF(CRUN_THROUGH.EQ.'n') GOTO 40
C------------------------------------------------------------------------------
        GOTO 40
100     FORMAT(A,$)
101     FORMAT(A)
        END
