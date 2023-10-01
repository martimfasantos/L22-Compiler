ROOT = ${HOME}/compiladores/root
RTS_LIB_DIR = $(ROOT)/usr/lib
COMPILER=../l22
ASM=yasm

LDFLAGS = -m elf_i386 -L$(RTS_LIB_DIR) -lrts
SOURCES = $(wildcard ./src/*.l22)
OBJECTS = $(SOURCES:%.l22=%.o)
EXECUTABLES = $(SOURCES:%.l22=%)
XMLS = $(SOURCES:%.l22=%.xml)
ASMS = $(SOURCES:%.l22=%.asm)

all: $(EXECUTABLES)

xml: $(XMLS)

asm: $(ASMS)

%.xml: %.l22
	$(COMPILER) --target xml $< -o $@

%.asm: %.l22
	$(COMPILER) --target asm $< -o $@

%.o: %.asm
	$(ASM) -felf32 $< -o $@

%:%.o
	$(LD) $< -o $@ $(LDFLAGS)

clean:
	$(RM) $(OBJECTS) $(EXECUTABLES) $(XMLS) $(ASMS)