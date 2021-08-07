#!/bin/bash

# A script to build tomboytools and make deb packages and windows iss files
# see https://www.debian.org/doc/manuals/debian-faq/ch-pkg_basics
# we can also add preinst, postinst, prerm, and postrm scripts if required
# David Bannon, August 2021
# Assumes a working FPC/Lazarus install with cross compile tools as described in
# http://wiki.lazarus.freepascal.org/Cross_compiling_for_Win32_under_Linux and
# http://wiki.lazarus.freepascal.org/Cross_compiling
# and that a 'Release' mode exists for all targets.

# ----------------------------------------------------------------------------
# Typical usage -
#          ./package_debian.sh $HOME"/lazarus/laz-200 <LeakCheck>

# Note we assume laz config has same name as Laz directory, ie .laz-200
# ----------------------------------------------------------------------------

PRODUCT="tomboytools"
VERSION=`cat version`

SOURCE_DIR="../source"
ICON_DIR="../Gallery"

WHOAMI="David Bannon <tomboy-ng@bannons.id.au>"
DOC_DIR="BUILD/usr/share/doc/$PRODUCT/"
# MANUALS=`cat note-files`

LPI="TomboyTools.lpi"
BUILDOPTS=" -B --quiet --quiet"
# BUILDOPTS=" -B "
BUILDDATE=`date -R`
LAZ_FULL_DIR="$1"
LAZ_DIR=`basename "$LAZ_FULL_DIR"`
WIN_DIR=WinPre_"$VERSION"
LEAKCHECK="NO"

if [ -z "$LAZ_DIR" ]; then
	echo "Usage : $0 /Full/Path/Lazarus/dir"
	echo "eg    : $0 \$HOME/bin/Lazarus/trunk"
	echo "or"
	echo "      : $0 clean"
	exit
fi

if [ "$2" == "LeakCheck" ]; then
	LEAKCHECK="YES"
fi


if [ $1 == "clean" ]; then
	rm  -f *.deb
	rm  -f *.tgz
	rm  -f *.rpm
	rm -Rf BUILD
	rm -Rf WinPre*
	exit
fi

# ----------------------


function LookForBinary () {
	cd "$SOURCE_DIR"
	if [ -a "$1" ]; then
		echo "Binary $1 was made"
	else	
		echo "---------- ERROR $1 was not made"
	fi
	cd "../package"
}

# Build binaries. Note that build-mode must be one already defined
# typically in the IDE.
# Lazbuild expects cpu=[x86_64, i386], we may build the arm one here. 
# For now, I build the arm binary on site.
# Build Modes required are -
# ReleaseLin64, ReleaseLin32, ReleaseWin64, ReleaseWin32, ReleaseRasPi 

function BuildAMode () {
    echo "------------- Building Mode $1 --------"
    case $1 in
        ReleaseLin64)
            CPU="x86_64"
            OS="linux"
            BIN="$PRODUCT"-64
        ;;
        ReleaseLin32)
            CPU="i386"
            OS="linux"
            BIN="$PRODUCT"-32
        ;;
        ReleaseWin32)
            CPU="i386"
            OS="win32"
            BIN="$PRODUCT"-32.exe
        ;;
        ReleaseWin64)
            CPU="x86_64"
	    OS="win64"
            BIN="$PRODUCT"-64.exe
        ;;
        ReleaseRasPi)
            CPU="arm"
            OS="linux"
            BIN="$PRODUCT"-armhf
        ;;
    esac
    cd ../source
    rm "$BIN"
    # TTools_VER="$VERSION" $LAZ_FULL_DIR/lazbuild $BUILDOPTS --pcp="$LAZ_CONFIG" --cpu="$CPU" --build-mode="$1" --os="$OS" "$LPI"
    TTools_VER="$VERSION" $LAZ_FULL_DIR/lazbuild $BUILDOPTS $LAZ_CONFIG --cpu="$CPU" --build-mode="$1" --os="$OS" "$LPI"
    # TTools_VER="$VERSION" $LAZ_FULL_DIR/lazbuild $BUILDOPTS --cpu="$CPU" --build-mode="$1" --os="$OS" "$LPI"

    if [ ! -f "$BIN" ]; then
	    echo "----- ERROR failed to build $BIN ---------"
	   exit
    fi	 
    cd ../package

#/home/dbannon/bin/Lazarus/trunk/lazbuild -B --cpu=x86_64 --build-mode=ReleaseLin64 --os=linux TomboyTools.lpi
}




function DebianPackage () {
	# We build a debian tree in BUILD and call dpkg-deb -b 
	#  BUILD/DEBIAN control,debian-binary and any scripts
	rm -rf BUILD
	mkdir -p BUILD/DEBIAN
	mkdir -p BUILD/usr/bin
	mkdir -p BUILD/usr/share/"$PRODUCT"
	for i in 16x16 22x22 24x24 32x32 48x48 256x256; do
		mkdir -p "BUILD/usr/share/icons/hicolor/$i/apps";
		cp "$ICON_DIR/$i.png" "BUILD/usr/share/icons/hicolor/$i/apps/$PRODUCT.png";
	done;
	#echo "------------- Done icons"
	#mkdir -p BUILD/usr/share/doc/$PRODUCT
	#echo "------------- Done 1"
	#cp ../doc/authors BUILD/usr/share/doc/$PRODUCT/.
	#echo "------------- Done 2"
	#cp -R ../doc/HELP BUILD/usr/share/"$PRODUCT"/.
	#echo "------------- Done 3"
	# -------------- Translation Files
	# we end up with, eg, /usr/share/locale/es/LC_MESSAGES/tomboy-ng.mo
	# and /usr/share/locale/es/LC_MESSAGES/lclstrconsts.mo for Linux 
	#mkdir -p BUILD/usr/share/locale
	#for i in `ls -b ../po/*.??.po`; do		# Deal with each country code in turn
	#        # echo "Name is $i"
	#        BASENAME=`basename -s.po "$i"`
	#        # echo "BASENAME is $BASENAME"
	#        CCODE=`echo "$BASENAME" | cut -d '.' -f2`
	#        # echo "CCode is $CCODE"
	#        mkdir -p BUILD/usr/share/locale/"$CCODE"/LC_MESSAGES
	#        BASENAME=`basename -s."$CCODE" "$BASENAME"`
	#	msgfmt -o BUILD/usr/share/locale/"$CCODE"/LC_MESSAGES/"$BASENAME".mo "$i"
	#	msgfmt -o BUILD/usr/share/locale/"$CCODE"/LC_MESSAGES/lclstrconsts.mo "$LAZ_FULL_DIR"/lcl/languages/lclstrconsts."$CCODE".po
	#done
	mkdir BUILD/usr/share/applications
	cp deb_files/"$PRODUCT.desktop" BUILD/usr/share/applications/.
	mkdir -p BUILD/usr/share/man/man1
	gzip -9kn deb_files/$PRODUCT.1
	mv deb_files/$PRODUCT.1.gz BUILD/usr/share/man/man1/.
	CTRL_ARCH="$1"
	CTRL_DEPENDS="libgtk2.0-0 (>= 2.6), libc6 (>= 2.14), libcanberra-gtk-module, wmctrl"
	CTRL_RELEASE="GTK2 release."
	case "$1" in
	"amd64")
		cp $SOURCE_DIR/tomboytools-64 BUILD/usr/bin/tomboytools	
		;;
	"i386")
		cp $SOURCE_DIR/tomboytools-32 BUILD/usr/bin/tomboytools
		;;
	"amd64Qt")
		cp $SOURCE_DIR/tomboytools-qt BUILD/usr/bin/tomboytools
		CTRL_ARCH="amd64"
		CTRL_DEPENDS="libqt5pas1, libc6 (>= 2.14), wmctrl"
		CTRL_RELEASE="Qt5 release."
		;;
	"armhf")
		cp $SOURCE_DIR/tomboytools-armhf BUILD/usr/bin/tomboytools
		# CTRL_DEPENDS=""   # just watch I dont need spec :armhf for each dep here
		CTRL_RELEASE="Raspberry Pi release."
		;;
	esac
	chmod 755 BUILD/usr/bin/tomboytools
	# -------------------- Documents -----------------
    mkdir -p "$DOC_DIR"
	echo "$PRODUCT ($VERSION)  unstable;  urgency=medium" >> "$DOC_DIR"changelog
	echo "  * Initial release" >> "$DOC_DIR"changelog
	echo "-- $WHOAMI  $BUILDDATE" >> "$DOC_DIR"changelog
	gzip -9n "$DOC_DIR"changelog
        # See https://www.debian.org/doc/packaging-manuals/copyright-format/1.0/#file-syntax
    # mkdir BUILD/usr/share/doc/$PRODUCT
    cp deb_files/copyright BUILD/usr/share/doc/$PRODUCT/copyright
	chmod -R g-w BUILD
    	# -------------------------------- Make control file -------------------------
	echo "Package: $PRODUCT" > BUILD/DEBIAN/control
	echo "Version: $VERSION" >> BUILD/DEBIAN/control
	echo "Architecture: $CTRL_ARCH" >> BUILD/DEBIAN/control
	echo "Maintainer: $WHOAMI" >> BUILD/DEBIAN/control
	# -------------------------------- Calculate size, thanks circular@LazForum
	SIZE_IN_KB="$(du -s BUILD | awk '{print $1;}')"
	echo "Installed-Size: ${SIZE_IN_KB}" >> "BUILD/DEBIAN/control"
	echo "Depends: $CTRL_DEPENDS" >> BUILD/DEBIAN/control
	echo "Priority: optional" >> BUILD/DEBIAN/control
	echo "Homepage: https://github.com/davidbannon/TomboyTools" >> BUILD/DEBIAN/control
	echo "Section: x11" >> BUILD/DEBIAN/control
	echo "Description: A tool to import and export Tomboy standard notes in a range of formats. $CTRL_RELEASE" >> BUILD/DEBIAN/control
	echo " Please report your experiences." >> BUILD/DEBIAN/control
  	fakeroot dpkg-deb -b BUILD/. "$PRODUCT""_$VERSION-0_$1.deb"
	# --------------------------------- Clean up -----------
	# rm -Rf BUILD
}


function MkWinPreInstaller() {
	# Make a dir containing everything we need to make a 32/64bit Inno Setup installer for Windows
	rm -Rf "$WIN_DIR"
	mkdir "$WIN_DIR"
	cp "$SOURCE_DIR"/tomboytools-64.exe "$WIN_DIR/."
	cp "$SOURCE_DIR"/tomboytools-32.exe "$WIN_DIR"/.
	cp deb_files/copyright "$WIN_DIR/copying"
	cp AfterInstall.txt "$WIN_DIR/."
	sed "s/MyAppVersion \"REPLACEME\"/MyAppVersion \"$VERSION\"/" tomboytools.iss > "$WIN_DIR/tomboytools.iss"
	# mkdir "$WIN_DIR/HELP_DIR"
	MANWIDTH=70 man -l deb_files/tomboytools.1 > "$WIN_DIR/readme.txt"
	unix2dos -q "$WIN_DIR/readme.txt"
	echo "----------- Windows installer dir created -----------"
}

# --------------------------------------

# It all starts here

if [ -f "$LAZ_FULL_DIR"/lazarus.cfg ]; then
	LAZ_CONFIG=""				# Assume if we have a cfg, it specifies pcp ??
else

	if [ -d "$HOME/.Laz_$LAZ_DIR" ]; then     # try my way of naming config first
		LAZ_CONFIG=" --pcp=""$HOME/.Laz_$LAZ_DIR";
	else
		echo "------ Testing for the .Laz config $HOME------"
		if [ -d "$HOME/.$LAZ_DIR" ]; then
			LAZ_CONFIG=" --pcp=""$HOME/.$LAZ_DIR";
		else 
			echo "**** CANNOT FIND Laz Config, exiting ****";
			exit;
		fi
	fi
fi

echo "-----  LAZ_CONFIG is $LAZ_CONFIG ------"


TESTMODE='yxes'

if [ "$TESTMODE" == "yes" ]; then
    BuildAMode ReleaseRasPi
    LookForBinary tomboytools-armhf
    #DebianPackage amd64
    exit
fi


# Build the Binaries
for REL in ReleaseLin64 ReleaseWin64 ReleaseLin32 ReleaseWin32 ReleaseRasPi ; do BuildAMode "$REL" ; done

# Check that they are all there.
for BIN in "$PRODUCT"-64 "$PRODUCT"-32 "$PRODUCT"-32.exe "$PRODUCT"-64.exe ; do LookForBinary "$BIN"; done

# Make the debian packages.
for BIN in amd64 i386 armhf ; do DebianPackage "$BIN"; done

echo "----------------- FINISHED DEBs ver $VERSION ------------"
# ls -l *.deb
MkWinPreInstaller
# ls -ltr
fakeroot bash ./mk_rpm.sh
# echo "OK, if that looks OK, run   fakeroot bash ./mk_rpm.sh"
# Dont sign under fakeroot, its messy
echo "OK, we will now sign the RPMs - david, use the longer passphrase !"
for i in `ls -b *.rpm`; do rpm --addsign "$i"; echo "Signed $i"; done
ls -l *.rpm *.deb "$WIN_DIR"/*.exe


