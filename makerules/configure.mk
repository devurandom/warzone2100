include $(MAKERULES)/config.mk


# Check for unset config

ifeq ($(strip $(VERSION)),)
$(error You must set VERSION in $(MAKERULES)/config.mk)
else
$(info VERSION set to $(VERSION))
endif

ifeq ($(strip $(PLATFORM)),)
$(error You must set PLATFORM in $(MAKERULES)/config.mk)
else
$(info PLATFORM set to $(PLATFORM))
endif

ifeq ($(strip $(MODE)),)
$(error You must set MODE in $(MAKERULES)/config.mk)
else
$(info MODE set to $(MODE))
endif

ifeq ($(strip $(DEVDIR)),)
$(error You must set DEVDIR in $(MAKERULES)/config.mk)
else
$(info DEVDIR set to $(DEVDIR))
endif

ifeq ($(strip $(BISON)),)
$(error You must set BISON in $(MAKERULES)/config.mk)
else
$(info BISON is set to $(BISON))
endif

ifeq ($(strip $(FLEX)),)
$(error You must set FLEX in $(MAKERULES)/config.mk)
else
$(info FLEX is set to $(FLEX))
endif

ifneq ($(strip $(INSTALLER)),)
ifeq ($(strip $(MAKENSIS)),)
$(error You must set MAKENSIS in $(MAKERULES)/config.mk)
else
$(info MAKENSIS is set to $(MAKENSIS))
endif
endif


# Setup paths and static values

CFLAGS+=-DVERSION=\"$(VERSION)\" -DYY_STATIC -I.. -I../.. -I$(DEVDIR)/include
LDFLAGS+=-L$(DEVDIR)/lib


# Setup build environment with config values

ifeq ($(strip $(MODE)),debug)
CFLAGS+=-O0 -g2 -DDEBUG -Wall
else
CFLAGS+=-O2 -g0 -DNDEBUG
endif

ifeq ($(strip $(PLATFORM)),windows)
DIRSEP=\\
RMF=del /F
EXEEXT=.exe
AR=ar
CC=gcc
WINDRES=windres
CFLAGS+=-mwindows -DWIN32
LDFLAGS+=-lmingw32 -lSDLmain
else
ifeq ($(strip $(PLATFORM)),mingw32)
DIRSEP=/
RMF=rm -f
EXEEXT=.exe
AR=mingw32-ar
CC=mingw32-gcc
WINDRES=mingw32-windres
CFLAGS+=-mwindows -DWIN32
LDFLAGS+=-lmingw32 -lSDLmain
else
DIRSEP=/
RMF=rm -f
EXEEXT=
AR=ar
CC=gcc
WINDRES=
endif
endif

# Generic libs

LDFLAGS+=-lSDL -lSDL_net -ljpeg -lpng -lz -lmad -lvorbisfile -lvorbis -logg -lphysfs

# Additional platform-dependend libs

ifeq ($(strip $(PLATFORM)),windows)
LDFLAGS+=-lwsock32 -lwinmm -lglu32 -lopengl32 -lopenal32
else
ifeq ($(strip $(PLATFORM)),mingw32)
LDFLAGS+=-lwsock32 -lwinmm -lglu32 -lopengl32 -lopenal32
else
LDFLAGS+=-lGLU -lGL -lopenal
endif
endif

include $(MAKERULES)/common.mk
