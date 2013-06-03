## Usage integrate_GLPK.sh [GLPK.tar.gz]
## Integrates the latest GLPK package
## theussl, 2008-07-08

## latest glpk tarball (Austrian mirror out of date)
#URL="http://gd.tuwien.ac.at/gnu/gnusrc/glpk/"
URL="ftp://ftp.gnu.org/gnu/glpk/"
latest="glpk-4.50.tar.gz"
## where to put source files and headers
DESTINATION=../src/GLPK

## --------------------------------------------
## Usage

usage() {
cat << "USAGE_END"
Usage: integrate_GLPK.sh [-g ]
       integrate_GLPK.sh [-i GLPK_source.tar.gz]
       integrate_GLPK.sh [-c]
Get, integrate or clean GLPK sources

Options:
  -g, --get           get latest Version of GLPK
  -i, --integrate     integrate given GLPK sources
  -c, --clean         clean the R package's src directory

USAGE_END
        exit $1
}

## --------------------------------------------
## Read command line arguments

for x in "$@" ; do
    case "${x}" in
        -i|--integrate)        # integrate sources
             integrate=true
             ;;
        -c|--clean)            # clean sources
             clean=true
             ;;
        -g|--get)            # clean sources
             get=true
	     sources=$latest # sources gets latest
             ;;
        -*)  # invalid option
             echo "$0: Invalid switch \"${x}\"!  Run $0 without parameters for help."
             exit 1
             ;;
        *)   # this should be the tarball of glpk sources
             if [[ ! -z "${sources}" ]] ; then
                 echo "$0: Only one source file allowed: \"${sources}\"!  Run $0 without parameters for help."
                 exit 1
             fi
             sources="${x}"
             ;;
    esac
done

## --------------------------------------------
## input validation

if [[ ! ( ${integrate} || ${clean} || ${get}) ]] ; then
    echo "$0: No option given; nothing to do!"
    usage 1
    exit 1
fi

if [[ ( ${integrate} && ${clean} ) || ( ${get} && ${clean} )]] ; then
    echo "$0: --clean can only be used alone!  Run $0 without parameters for help."
    exit 1
fi

if [[ -z "${sources}" && $integrate ]] ; then
    echo "$0: No source file to integrate given!"
    usage 1
    exit 1
fi


## --------------------------------------------
## integrate GLPK sources to package

if [[ $get ]] ; then
    if [[ ! -s "${sources}" ]] ; then
	wget $URL/$sources
    else
	echo "$sources already available."
    fi
fi

if [[ $integrate ]] ; then
    
    if [[ ! -s "${sources}" ]] ; then
	echo "$0: Selected source file \"$sources\" is not available or zero!"
	usage 1
	exit 1
    fi
    GLPK=`basename $sources .tar.gz`
    SOURCEDIR=${GLPK}

    tar xzf $sources

    if [[ ! -d ${DESTINATION} ]] ; then
	mkdir -p $DESTINATION
    fi

     cp -a $SOURCEDIR/src/* $DESTINATION
     
     cp -a $SOURCEDIR/examples/plan.* ../inst/examples/
     cp -a $SOURCEDIR/examples/assign.mod ../inst/examples/
     
    
    if [[ -d $SOURCEDIR ]] ; then
	rm -rf $SOURCEDIR
    fi
    echo "Patching upstream code: abort() statements replaced by xerror()!"
    cat ./glpenv01.patch | patch -p0 $DESTINATION/glpenv01.c
    cat ./glpenv04.patch | patch -p0 $DESTINATION/glpenv04.c
    cat ./glpmat.patch | patch -p0 $DESTINATION/glpmat.c
    cat ./glpbfd.patch | patch -p0 $DESTINATION/glpbfd.c
    cat ./glpenv07.patch | patch -p0 $DESTINATION/glpenv07.c
    cat ./glpapi19.patch | patch -p0 $DESTINATION/glpapi19.c

    ## create Makefile which is called via package's
    ## Makevars

    echo '#-*- Makefile -*-
#

include $(R_HOME)/etc$(R_ARCH)/Makeconf

SOURCES= \' > $DESTINATION/Makefile
(cd $DESTINATION ; find . | grep ".c$" | xargs printf "%s \\\\\n" >> Makefile)

echo '
OBJS = $(SOURCES:.c=.o)

PKG_CPPFLAGS = -I.

all: libglpk.a

libglpk.a: $(OBJS)
	$(AR) crs $@ $(OBJS)
' >> $DESTINATION/Makefile

    ## create Makefile.win which is called via package's
    ## Makevars.win
    echo '#-*- Makefile -*-
#
include $(R_HOME)/etc$(R_ARCH)/Makeconf

SOURCES= \' > $DESTINATION/Makefile.win
(cd $DESTINATION ; find . | grep ".c$" | xargs printf "%s \\\\\n" >> Makefile.win)

echo '
OBJS = $(SOURCES:.c=.o)

PKG_CPPFLAGS = -D__WOE__ -D__MINGW32__ -I.

all: libglpk.a

libglpk.a: $(OBJS)
	$(AR) crs $@ $(OBJS)
' >> $DESTINATION/Makefile.win
    #cp 05_Makefile.win $DESTINATION/src/Makefile.win
fi


if [[ $clean ]] ; then
    if [[ -d ${DESTINATION} ]] ; then
	rm -rf $DESTINATION/*
    fi
    rm -rf ../inst/mps
    rm -rf ../inst/mod

fi

echo "done."

exit 0
