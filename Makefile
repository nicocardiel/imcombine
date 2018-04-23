#!/bin/tcsh
#------------------------------------------------------------------------------
# Version 12-Feb-2003
# Version 22-Apr-2016
#------------------------------------------------------------------------------
# To install imcombine properly, you must follow these steps:
# make include
# make imcombine
# make clean
#------------------------------------------------------------------------------
# define here maximum image dimensions
# NXMAXF must be the image size of each individual frame
# NYMAXF must be the image size of each individual frame
# NXMAX must be the image size of the final combined image
# NYMAX must be the image size of the final combined image
# NMAXFRAMES es el numero maximo de frames que pueden sumarse; notar que 
#            cuanto mayor sea este numero, mas filas de cada frame pueden
#            leerse de una sola vez (cuando el numero real de frames sea
#            menor que NMAXFRAMES)
#
# NOTA: se ha de verificar que NXMAX > NXMAXF y que NYMAX > NYMAXF
NXMAXF  = 2048
NYMAXF  = 2048
NXMAX   = 3000
NYMAX   = 3000
NMAXFRAMES = 1000
NMAXREG = 64
#------------------------------------------------------------------------------
# PGPLOT library directory
#PGPDIR  = /usr/local/pgplot
PGPDIR  = /opt/local/lib
#------------------------------------------------------------------------------
# X11 library directory
#X11DIR  = /usr/X11R6/lib
X11DIR  = /opt/local/lib
#------------------------------------------------------------------------------
# CFITSIO library directory
#FIODIR  = /usr/local/cfitsio
FIODIR  = /opt/local/lib
#------------------------------------------------------------------------------
# sextractor path
SEXPATH = /Users/cardiel/s/sextractor/sextractor-2.5.0/src
#------------------------------------------------------------------------------
# Set the appropiate FORTRAN compiler
#FCOMPIL = g77 -O3 -g -Wall
FCOMPIL = gfortran-mp-7 -O2 -g -Wall
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
# Nothing SHOULD be modified below this comment line
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
# macro definitions
FSOURCE = dibuframe.f \
          imcombine.f \
          iofunctions.f \
          ffj.f \
          filtrasky.f \
          findmml.f \
          fmean0.f \
          fmean0e.f \
          fmean0w.f \
          fmean1.f \
          fmean2.f \
          fmean2e.f \
          fmedian1.f \
          fmedian1mad.f \
          leecolumn.f \
          maskframe.f \
          multiplica.f \
          ordena1f.f \
          ordena1f1i.f \
          printanynull.f \
          printerror.f \
          randomnumber.f \
          readfits.f \
          sondea4c.f \
          systemfunction.f 
FOBJECT = $(FSOURCE:.f=.o)
# Default rule to create program
imcombine:  $(FOBJECT)
	$(FCOMPIL) -o $@ $(FOBJECT) -L$(PGPDIR) -L$(FIODIR) -L$(X11DIR) -lpgplot -lcfitsio -lX11
# Target to clean object modules
clean:    $(FOBJECT)
	rm -f $(FOBJECT)
	rm -f dimensions.inc
	rm -f imcombine_dir.inc
	rm -f sexpath.inc
# Target to touch source modules
touch:
	touch $(FSOURCE)
# Target to create the file dimensions.inc
include:
	echo "        INTEGER NXMAXF" > dimensions.inc
	echo "        PARAMETER (NXMAXF=$(NXMAXF))" >> dimensions.inc
	echo "        INTEGER NYMAXF" >> dimensions.inc
	echo "        PARAMETER (NYMAXF=$(NYMAXF))" >> dimensions.inc
	echo "        INTEGER NXMAX" >> dimensions.inc
	echo "        PARAMETER (NXMAX=$(NXMAX))" >> dimensions.inc
	echo "        INTEGER NYMAX" >> dimensions.inc
	echo "        PARAMETER (NYMAX=$(NYMAX))" >> dimensions.inc
	echo "        INTEGER NMAXFRAMES" >> dimensions.inc
	echo "        PARAMETER (NMAXFRAMES=$(NMAXFRAMES))" >> dimensions.inc
	echo "        CHARACTER*255 IMCOMBINE_DIR" > imcombine_dir.inc
	echo "        PARAMETER(IMCOMBINE_DIR=" >> imcombine_dir.inc
	echo "     +   '`pwd`/')" >> imcombine_dir.inc
	echo "        CHARACTER*255 SEXPATH" > sexpath.inc
	echo "        PARAMETER(SEXPATH=" >> sexpath.inc
	echo "     +   '$(SEXPATH)/')" >> sexpath.inc
	echo "        INTEGER NMAXREG" >> dimensions.inc
	echo "        PARAMETER (NMAXREG=$(NMAXREG))" >> dimensions.inc
	touch $(FSOURCE) $(CSOURCE)
# second level dependencies
.f.o: $(FSOURCE)
	$(FCOMPIL) -c $?
# definitions
.PRECIOUS: imcombine
