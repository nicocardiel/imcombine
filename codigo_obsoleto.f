C------------------------------------------------------------------------------
C******************************************************************************
C------------------------------------------------------------------------------
C Este codigo es obsoleto
53	CONTINUE
	NF1=READILIM('First frame','1',1,NFRAMES)
	WRITE(CDUMMY,*) NFRAMES
	NF2=READILIM('Last frame',CDUMMY,NF1,NFRAMES)
	CPAUSE=READC('Pause between frames (y/n)',CPAUSE,'yn')
	WRITE(*,*)
	!para cada imagen, calculamos el flatfield sumando los
	!NSUBFRAMES_FLAT frames mas proximos en el tiempo
	DO NF=NF1,NF2
c
	  WRITE(*,100) 'Working with frame #'
	  WRITE(*,*) NF
	  !si es el primer frame a realizar, habra que calcular el sumatorio
          !de todos los frames ineludiblemente
	  IF(NF.EQ.NF1)THEN
	    LRESTART=.TRUE.
	    K2=2
	    DO K=1,NSUBFRAMES_FLAT
	      DO WHILE(FRAME_IOK(ID_CLOSEST(K2,NF)).NE.0)
	        K2=K2+1
	        IF(K2.GT.NFRAMES)THEN
	          WRITE(*,100) 'FATAL ERROR: K, K2='
	          WRITE(*,*) K,K2
	          STOP
	        END IF
	      END DO
	      ID2(K)=ID_CLOSEST(K2,NF)
	      K2=K2+1
	    END DO
	    DO K=1,NSUBFRAMES_FLAT
	      WRITE(*,100) '> k, id2(k): '
	      WRITE(*,*) K,ID2(K)
	    END DO
	  !si no es el primer frame, ya existe un sumatorio de imagenes; en
	  !ese caso hay que estudiar si merece la pena volver a calcular
	  !el sumatorio completo o, si por el contrario, podemos reutilizar
	  !parte del sumatorio
	  ELSE
	    K1=2
	    K2=2
	    DO K=1,NSUBFRAMES_FLAT
	      DO WHILE(FRAME_IOK(ID_CLOSEST(K1,NF-1)).NE.0)
	        K1=K1+1
	        IF(K1.GT.NFRAMES)THEN
	          WRITE(*,100) 'FATAL ERROR: K, K1='
	          WRITE(*,*) K,K1
	          STOP
	        END IF
	      END DO
	      ID1(K)=ID_CLOSEST(K1,NF-1)
	      K1=K1+1
	      DO WHILE(FRAME_IOK(ID_CLOSEST(K2,NF)).NE.0)
	        K2=K2+1
	        IF(K2.GT.NFRAMES)THEN
	          WRITE(*,100) 'FATAL ERROR: K, K2='
	          WRITE(*,*) K,K2
	          STOP
	        END IF
	      END DO
	      ID2(K)=ID_CLOSEST(K2,NF)
	      K2=K2+1
	    END DO
	    DO K=1,NSUBFRAMES_FLAT
	      WRITE(*,100) '> k, id1(k), id2(k): '
	      WRITE(*,*) K,ID1(K),ID2(K)
	    END DO
	    NSAME=0
	    DO K1=1,NSUBFRAMES_FLAT
	      DO K2=1,NSUBFRAMES_FLAT
	        IF(ID1(K1).EQ.ID2(K2)) NSAME=NSAME+1
	      END DO
	    END DO
	    WRITE(*,100) '> number of repeated frames for flat: '
	    WRITE(*,*) NSAME
	    LRESTART=(2*(NSUBFRAMES_FLAT-NSAME).GE.NSUBFRAMES_FLAT)
	  END IF
c
	  IF(LRESTART)THEN
	    DO I=1,NAXISF2
	      DO J=1,NAXISF1
	        FRAME_SUMFLAT(J,I)=0.0
	        FRAME_NPIXELS(J,I)=0
	      END DO
	    END DO
	    !incluimos todos los frames
	    DO K2=1,NSUBFRAMES_FLAT
	      NF_EFF=ID2(K2)
	      !leemos frame
	      CALL READFITS(FILE_FRAME(NF_EFF),NXMAXF,NYMAXF,
     +         NAXISF1,NAXISF2,FRAME_DATA,.TRUE.)
	      !leemos dark
	      CALL READFITS(FILE_DARK_FRAME(NF_EFF),NXMAXF,NYMAXF,
     +         NAXISF1,NAXISF2,FRAME_DARK,.TRUE.)
	      !calculamos la mediana en (frame-dark), eliminando en el
	      !calculo defectos cosmeticos y objetos
	      K=0
	      DO I=1,NAXISF2
	        II=I+II_FRAME(NF_EFF)
	        DO J=1,NAXISF1
	          JJ=J+JJ_FRAME(NF_EFF)
	          IF(.NOT.MASK_IMAGEN_FINAL(JJ,II))THEN
	            IF(.NOT.MASK_FRAME(J,I))THEN
	              K=K+1
	              PIXEL(K)=
     +                 FRAME_DATA(J,I)-FACTOR_DARK*FRAME_DARK(J,I)
	            END IF
	          END IF
	        END DO
	      END DO
	      FMEDIAN_FLAT(NF_EFF)=FMEDIAN1(K,PIXEL)
              !acumulamos frame-dark (este ultimo escalado al mismo tiempo 
              !de exposicion) normalizando al valor de la mediana calculado
	      !anteriormente
	      DO I=1,NAXISF2
	        II=I+II_FRAME(NF_EFF)
	        DO J=1,NAXISF1
	          JJ=J+JJ_FRAME(NF_EFF)
	          IF(.NOT.MASK_IMAGEN_FINAL(JJ,II))THEN
	            FRAME_SUMFLAT(J,I)=FRAME_SUMFLAT(J,I)+
     +               (FRAME_DATA(J,I)-FACTOR_DARK*FRAME_DARK(J,I))/
     +               FMEDIAN_FLAT(NF_EFF)
	            FRAME_NPIXELS(J,I)=FRAME_NPIXELS(J,I)+1
	          END IF
	        END DO
	      END DO
	      WRITE(CDUMMY,*) NF_EFF
	      CALL RMBLANK(CDUMMY,CDUMMY,L)
	      WRITE(*,100) ' +'//CDUMMY(1:L)
	    END DO
	    WRITE(*,101) ' step 1'
	  ELSE
	    DO K1=1,NSUBFRAMES_FLAT
	      LOK=.FALSE.
	      DO K2=1,NSUBFRAMES_FLAT
	        IF(ID1(K1).EQ.ID2(K2)) LOK=.TRUE.
	      END DO
	      !borramos ID1
	      IF(.NOT.LOK)THEN
	        NF_EFF=ID1(K1)
	        !leemos frame
	        CALL READFITS(FILE_FRAME(NF_EFF),NXMAXF,NYMAXF,
     +           NAXISF1,NAXISF2,FRAME_DATA,.TRUE.)
	        !leemos dark
	        CALL READFITS(FILE_DARK_FRAME(NF_EFF),NXMAXF,NYMAXF,
     +           NAXISF1,NAXISF2,FRAME_DARK,.TRUE.)
                !restamos frame-dark (este ultimo escalado al mismo tiempo 
                !de exposicion) normalizado (no necesitamos volver a
	        !calcular FMEDIAN_FLAT porque lo hemos almacenado en un
	        !vector
	        DO I=1,NAXISF2
	          II=I+II_FRAME(NF_EFF)
	          DO J=1,NAXISF1
	            JJ=J+JJ_FRAME(NF_EFF)
	            IF(.NOT.MASK_IMAGEN_FINAL(JJ,II))THEN
	              FRAME_SUMFLAT(J,I)=FRAME_SUMFLAT(J,I)-
     +                 (FRAME_DATA(J,I)-FACTOR_DARK*FRAME_DARK(J,I))/
     +                 FMEDIAN_FLAT(NF_EFF)
	              FRAME_NPIXELS(J,I)=FRAME_NPIXELS(J,I)-1
	            END IF
	          END DO
	        END DO
	        WRITE(CDUMMY,*) NF_EFF
	        CALL RMBLANK(CDUMMY,CDUMMY,L)
	        WRITE(*,100) ' -'//CDUMMY(1:L)
	      END IF
	    END DO
	    DO K2=1,NSUBFRAMES_FLAT
	      LOK=.FALSE.
	      DO K1=1,NSUBFRAMES_FLAT
	        IF(ID1(K1).EQ.ID2(K2)) LOK=.TRUE.
	      END DO
	      !incluimos ID2
	      IF(.NOT.LOK)THEN
	        NF_EFF=ID2(K2)
	        !leemos frame
	        CALL READFITS(FILE_FRAME(NF_EFF),NXMAXF,NYMAXF,
     +           NAXISF1,NAXISF2,FRAME_DATA,.TRUE.)
	        !leemos dark
	        CALL READFITS(FILE_DARK_FRAME(NF_EFF),NXMAXF,NYMAXF,
     +           NAXISF1,NAXISF2,FRAME_DARK,.TRUE.)
	        !calculamos la mediana en (frame-dark), eliminando en el
	        !calculo defectos cosmeticos y objetos
	        K=0
	        DO I=1,NAXISF2
	          II=I+II_FRAME(NF_EFF)
	          DO J=1,NAXISF1
	            JJ=J+JJ_FRAME(NF_EFF)
	            IF(.NOT.MASK_IMAGEN_FINAL(JJ,II))THEN
	              IF(.NOT.MASK_FRAME(J,I))THEN
	                K=K+1
	                PIXEL(K)=
     +                   FRAME_DATA(J,I)-FACTOR_DARK*FRAME_DARK(J,I)
	              END IF
	            END IF
	          END DO
	        END DO
	        FMEDIAN_FLAT(NF_EFF)=FMEDIAN1(K,PIXEL)
                !acumulamos frame-dark (este ultimo escalado al mismo tiempo 
                !de exposicion) normalizando al valor de la mediana calculado
	        !anteriormente
	        DO I=1,NAXISF2
	          II=I+II_FRAME(NF_EFF)
	          DO J=1,NAXISF1
	            JJ=J+JJ_FRAME(NF_EFF)
	            IF(.NOT.MASK_IMAGEN_FINAL(JJ,II))THEN
	              FRAME_SUMFLAT(J,I)=FRAME_SUMFLAT(J,I)+
     +                 (FRAME_DATA(J,I)-FACTOR_DARK*FRAME_DARK(J,I))/
     +                 FMEDIAN_FLAT(NF_EFF)
	              FRAME_NPIXELS(J,I)=FRAME_NPIXELS(J,I)+1
	            END IF
	          END DO
	        END DO
	        WRITE(CDUMMY,*) NF_EFF
	        CALL RMBLANK(CDUMMY,CDUMMY,L)
	        WRITE(*,100) ' +'//CDUMMY(1:L)
	      ELSE
	        WRITE(CDUMMY,*) ID2(K2)
	        CALL RMBLANK(CDUMMY,CDUMMY,L)
	        WRITE(*,100) ' ='//CDUMMY(1:L)
	      END IF
	    END DO
	    WRITE(*,101) ' step 1'
	  END IF
c
          !calculamos histograma de numero de frames/pixel para el flat
	  MIN_FRAME_NPIXELS=NSUBFRAMES_FLAT
	  MAX_FRAME_NPIXELS=0
	  DO K=1,NSUBFRAMES_FLAT
	    NPIX=0
	    DO I=1,NAXISF2
	      DO J=1,NAXISF1
	        IF(FRAME_NPIXELS(J,I).EQ.K) NPIX=NPIX+1
	      END DO
	    END DO
	    WRITE(*,100) 'Nframes/pixel, Npixels: '
	    WRITE(*,*) K,NPIX
	    IF(NPIX.GT.0)THEN
	      MIN_FRAME_NPIXELS=MIN0(MIN_FRAME_NPIXELS,K)
	      MAX_FRAME_NPIXELS=MAX0(MAX_FRAME_NPIXELS,K)
	    END IF
	  END DO
	  WRITE(*,100) '> min_frame_npixels, max_frame_npixels: '
	  WRITE(*,*) MIN_FRAME_NPIXELS,MAX_FRAME_NPIXELS
c
	  !calculamos finalmente el flatfield
	  DO I=1,NAXISF2
	    DO J=1,NAXISF1
	      FRAME_FLAT(J,I)=FRAME_SUMFLAT(J,I)/
     +         REAL(FRAME_NPIXELS(J,I))
	    END DO
	  END DO
c dibujamos un poquito
	  NX1=1
	  NX2=NAXISF1
	  NY1=1
	  NY2=NAXISF2
	  XMIN=REAL(NX1)-0.6
	  XMAX=REAL(NX2)+0.6
	  YMIN=REAL(NY1)-0.6
	  YMAX=REAL(NY2)+0.6
	  CALL PGERAS
	  CALL PGSUBP(1,1)
	  CALL PGERAS
	  CALL PGENV(XMIN,XMAX,YMIN,YMAX,1,-2)
	  CALL PGSCI(5)
	  CALL PGBOX('BCTSNI',0.0,0,'BCTSNI',0.0,0)
	  CALL PGSCI(7)
	  CALL PGLABEL('X axis','Y axis',FILE_FRAME_NPIXELS(NF))
	  CALL PGSCI(1)
	  BG=REAL(MIN_FRAME_NPIXELS)-0.5
	  FG=REAL(MAX_FRAME_NPIXELS)+0.5
	  !OJO: reutilizamos la matriz FRAME_DATA para ahorrar memoria
	  DO I=1,NAXISF2
	    DO J=1,NAXISF1
	      FRAME_DATA(J,I)=REAL(FRAME_NPIXELS(J,I))
	    END DO
	  END DO
	  CALL PGGRAY(FRAME_DATA,NXMAXF,NYMAXF,
     +     NX1,NX2,NY1,NY2,FG,BG,TR)
	  INQUIRE(FILE=FILE_FRAME_NPIXELS(NF),EXIST=LOGFILE)
	  IF(LOGFILE)THEN
	    L2=TRUELEN(FILE_FRAME_NPIXELS(NF))
	    ISYSTEM=SYSTEMFUNCTION('rm -f '//
     +       FILE_FRAME_NPIXELS(NF)(1:L2))
	  END IF
	  SIMPLE=.TRUE.
	  BITPIX=-32
	  EXTEND=.FALSE.
	  BLOCKSIZE=1
	  CALL FTINIT(80,FILE_FRAME_NPIXELS(NF),BLOCKSIZE,ISTATUS)
	  CALL FTPHPR(80,SIMPLE,BITPIX,2,NAXISF(1,1),0,1,
     +     EXTEND,ISTATUS)
	  CALL FTP2DE(80,1,NXMAXF,NAXISF1,NAXISF2,FRAME_DATA,
     +     ISTATUS)
	  CALL FTCLOS(80,ISTATUS)
	  IF(ISTATUS.GT.0) CALL PRINTERROR(ISTATUS)
c leemos el frame al que queremos aplicar el flatfield (le sustraemos tambien
c el dark)
	  WRITE(*,100) '.'
	  !leemos frame
	  CALL READFITS(FILE_FRAME(NF),NXMAXF,NYMAXF,
     +     NAXISF1,NAXISF2,FRAME_DATA,.TRUE.)
	  WRITE(*,100) '.'
	  !leemos dark
	  CALL READFITS(FILE_DARK_FRAME(NF),NXMAXF,NYMAXF,
     +     NAXISF1,NAXISF2,FRAME_DARK,.TRUE.)
	  WRITE(*,100) '.'
          !calculamos frame-dark (este ultimo escalado al mismo tiempo 
          !de exposicion)
	  DO I=1,NAXISF2
	    II=I+II_FRAME(NF_EFF)
	    DO J=1,NAXISF1
	      JJ=J+JJ_FRAME(NF_EFF)
	      IF(.NOT.MASK_IMAGEN_FINAL(JJ,II))THEN
	        FRAME_DATA(J,I)=
     +           FRAME_DATA(J,I)-FACTOR_DARK*FRAME_DARK(J,I)
	      END IF
	    END DO
	  END DO
	  WRITE(*,100) '.'
c dividimos el frame por el flatfield
	  DO I=1,NAXISF2
	    DO J=1,NAXISF1
	      IF(ABS(FRAME_FLAT(J,I)).GT.1E-30)THEN !flat mayor que cero
	        FRAME_DATA(J,I)=FRAME_DATA(J,I)/FRAME_FLAT(J,I)
	      ELSE
	        FRAME_DATA(J,I)=0.0
	      END IF
	    END DO
	  END DO
	  WRITE(*,100) '.'
C salvamos flatfield
	  SIMPLE=.TRUE.
	  BITPIX=-32
	  EXTEND=.FALSE.
	  BLOCKSIZE=1
	  ISTATUS=0
	  L2=TRUELEN(FILE_FRAME_FLAT(NF))
	  INQUIRE(FILE=FILE_FRAME_FLAT(NF),EXIST=LOGFILE)
	  IF(LOGFILE)THEN
	    ISYSTEM=SYSTEMFUNCTION('rm -f '//
     +       FILE_FRAME_FLAT(NF)(1:L2))
	  END IF
	  CALL FTINIT(80,FILE_FRAME_FLAT(NF),
     +     BLOCKSIZE,ISTATUS)
	  CALL FTPHPR(80,SIMPLE,BITPIX,2,NAXISF(1,1),0,1,
     +     EXTEND,ISTATUS)
	  CALL FTP2DE(80,1,NXMAXF,NAXISF1,NAXISF2,FRAME_FLAT,ISTATUS)
	  CALL FTCLOS(80,ISTATUS)
	  IF(ISTATUS.GT.0) CALL PRINTERROR(ISTATUS)
	  WRITE(*,100) '.'
C salvamos frame tras dividir por el flatfield
	  L2=TRUELEN(FILE_FRAME_AFTER_FLAT(NF)) !OJO: aqui es NF, no NF_EFF
	  INQUIRE(FILE=FILE_FRAME_AFTER_FLAT(NF),EXIST=LOGFILE)
	  IF(LOGFILE)THEN
	    ISYSTEM=SYSTEMFUNCTION('rm -f '//
     +       FILE_FRAME_AFTER_FLAT(NF)(1:L2))
	  END IF
	  CALL FTINIT(80,FILE_FRAME_AFTER_FLAT(NF),
     +     BLOCKSIZE,ISTATUS)
	  CALL FTPHPR(80,SIMPLE,BITPIX,2,NAXISF(1,1),0,1,
     +     EXTEND,ISTATUS)
	  CALL FTP2DE(80,1,NXMAXF,NAXISF1,NAXISF2,FRAME_DATA,ISTATUS)
	  CALL FTCLOS(80,ISTATUS)
	  IF(ISTATUS.GT.0) CALL PRINTERROR(ISTATUS)
	  WRITE(*,100) '.'
c dibujamos un poquito
	  NX1=1
	  NX2=NAXISF1
	  NY1=1
	  NY2=NAXISF2
	  XMIN=REAL(NX1)-0.6
	  XMAX=REAL(NX2)+0.6
	  YMIN=REAL(NY1)-0.6
	  YMAX=REAL(NY2)+0.6
	  CALL PGERAS
	  CALL PGENV(XMIN,XMAX,YMIN,YMAX,1,-2)
	  CALL PGSCI(5)
	  CALL PGBOX('BCTSNI',0.0,0,'BCTSNI',0.0,0)
	  CALL PGSCI(7)
	  CALL PGLABEL('X axis','Y axis',FILE_FRAME_FLAT(NF))
	  IF(FRAME_IOK(NF).NE.0)THEN
	    CALL PGSCI(2)
	    CALL PGMTXT('T',0.5,0.5,0.5,'WARNING: not used in flat')
	  END IF
	  CALL PGSCI(1)
	  CALL SONDEA4C(FRAME_DATA,NXMAXF,NYMAXF,NAXISF1,NAXISF2,
     +     SONDEO4C_FMEAN,SONDEO4C_FSIGMA)
	  BG=SONDEO4C_FMEAN-8.0*SONDEO4C_FSIGMA
	  FG=SONDEO4C_FMEAN+8.0*SONDEO4C_FSIGMA
	  CALL PGGRAY(FRAME_DATA,NXMAXF,NYMAXF,
     +     NX1,NX2,NY1,NY2,FG,BG,TR)
	  WRITE(*,*)
	  IF(CPAUSE.EQ.'y')THEN
	    WRITE(*,100) 'Press <CR> to continue...'
	    READ(*,*)
	    IF(LECHO) WRITE(*,*)
	  END IF
	END DO
	WRITE(*,*)	
	IF(CRUN_THROUGH.EQ.'n') GOTO 40
