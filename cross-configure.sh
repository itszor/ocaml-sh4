#!/bin/sh

HERE=$(dirname "$0")
PARENT=$(readlink -f "$HERE/..")

PREFIX="$PARENT/prefix"
TARGETTMP="$PARENT/tmp"

if true; then
  TARGETARCH=sh4
  CROSS_COMPILE=sh-linux-gnu-
  TARGETSYS=mia
else
  TARGETARCH=arm
  CROSS_COMPILE=arm-none-linux-gnueabi-
  TARGETSYS=rexella
fi

OCAMLSRC="$HERE"

set -e

if [ -e "$TARGETTMP" ]; then
  echo "Target temp dir $TARGETTMP already exists (please remove)."
  exit 1
fi

# Clean up

pushd "$OCAMLSRC"
make distclean || true
popd

# Configure on target (which must share the same mountpoint we are running
# from).

mkdir -p "$TARGETTMP"

pushd "$OCAMLSRC"
cp -r configure config "$TARGETTMP"
popd

ssh "$TARGETSYS" "cd \"$TARGETTMP\"; ./configure"

cp -f "$TARGETTMP"/config/m.h "$OCAMLSRC"/config
cp -f "$TARGETTMP"/config/s.h "$OCAMLSRC"/config

rm -rf "$TARGETTMP"

sed -e 's,^\(BINDIR\)=\(.*\),\1='"$PREFIX"/bin',' \
    -e 's,^\(LIBDIR\)=\(.*\),\1='"$PREFIX"/lib',' \
    -e 's,^\(MANDIR\)=\(.*\),\1='"$PREFIX"/man',' \
    -e 's,^#\(ARCH\)=none,\1='"$TARGETARCH"',' \
    -e 's,^#\(MODEL=default\),\1,' \
    -e 's,^#\(SYSTEM=linux_elf\),\1,' \
    -e 's,^#\(TARGETRANLIB\)=\(ar rs\),\1='"$CROSS_COMPILE"'ar rs,' \
    -e 's,^#\(TARGETAR\)=\(.*\),\1='"$CROSS_COMPILE"'ar,' \
    -e 's,^#\(AS\)=as$,ASM='"$CROSS_COMPILE"'gcc -c -x assembler-with-cpp,' \
    -e 's,^#\(ASPP\)=gcc -c$,\1='"$CROSS_COMPILE"'gcc -c,' \
    -e 's,^#\(BYTECC\)=gcc$,\1=gcc,' \
    -e 's|^#\(BYTECCRPATH\)=\(-Wl,-rpath\)$|\1=|' \
    -e 's,^#\(NATIVECC\)=gcc$,\1='"$CROSS_COMPILE"'gcc,' \
    -e 's,^#\(NATIVECCLINKOPTS=\)$,\1,' \
    -e 's|^#\(NATIVECCRPATH\)=\(-Wl,-rpath\)$|\1=|' \
    -e 's,^\(PARTIALLD\)=\(.*\),\1='"$CROSS_COMPILE"'ld -r $(NATIVECCLINKOPTS),' \
    -e 's,^#\(MKEXE\)=\(.*\),\1=gcc,' \
    -e 's,^#\(MKDLL\)=\(.*\),\1=gcc -shared,' \
    -e 's,^#\(MKMAINDLL\)=\(.*\),\1=gcc -shared,' \
    -e 's,^#\(TARGETMKEXE\)=\(.*\),\1='"$CROSS_COMPILE"'gcc,' \
    -e 's,^#\(TARGETMKDLL\)=\(.*\),\1='"$CROSS_COMPILE"'gcc -shared,' \
    -e 's,^#\(TARGETMKMAINDLL\)=\(.*\),\1='"$CROSS_COMPILE"'gcc -shared,' \
    -e 's,^#\(PTHREAD_LINK=-cclib -lpthread\)$,\1,' \
    -e 's,^#\(SO=so\)$,\1,' \
    -e 's,^#\(CUSTOM_IF_NOT_SHARED=\)$,\1,' \
    -e 's,^#\(RANLIBCMD=\)$,\1,' \
    -e 's,^#\(CCLIBS\)=\(.*\)$,\1=\2,' \
    -e 's,^#\(SHAREDCCCOMPOPTS=-fPIC\)$,\1,' \
    -e 's,^#\(PROFILING=noprof\)$,\1,' \
    -e 's,^#\(MKSHAREDLIB\)=\(gcc -shared -o\)$,\1='"$CROSS_COMPILE"'gcc -shared -o,' \
    -e 's,^\(OTHERLIBRARIES\)=\(.*\)$,\1=unix str num threads dynlink bigarray,' \
    < "$OCAMLSRC"/config/Makefile-templ > "$OCAMLSRC"/config/Makefile

cat >> "$OCAMLSRC"/config/Makefile << EOF

# Is this missing from the cross-compile patch?
SYSTHREAD_SUPPORT=true

# This too?
SUPPORTS_SHARED_LIBRARIES=true

MKSHAREDLIBRPATH=

O=o
A=a
CMXS=cmxs

NATIVECCLIBS=-lm -ldl
BYTECCLIBS=-lm -ldl

#ml let ext_dll = ".so";;
#ml let ext_lib = ".a";;

MKLIB=\$(TARGETAR) rc \\\$(1) \\\$(2); \$(TARGETRANLIB) \\\$(1)
#ml let syslib x = "-l"^x;;
#ml let mklib out files opts = Printf.sprintf "${CROSS_COMPILE}ar rc %s %s %s; ${CROSS_COMPILE}ranlib %s" out opts files out;;
EOF

echo "Now run \"cd $OCAMLSRC; make cross && make installcross\"".
