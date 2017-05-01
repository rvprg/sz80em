PROGRAM_NAME = sz80em
DIST_FILES = $(PROGRAM_NAME).nw $(PROGRAM_NAME).ps $(PROGRAM_NAME).dvi $(PROGRAM_NAME).pdf $(PROGRAM_NAME).fs $(PROGRAM_NAME)r.fs LICENSE README $(PROGRAM_NAME)r.exe IMPORTANT 48k.rom
ROOT_NAME = "Z80 Emulator Implementation"

all: $(PROGRAM_NAME) dist 

$(PROGRAM_NAME).fs: $(PROGRAM_NAME).nw
	notangle -R$(PROGRAM_NAME).fs $(PROGRAM_NAME).nw > $(PROGRAM_NAME).fs

$(PROGRAM_NAME)r.fs: $(PROGRAM_NAME).nw
	notangle -R$(PROGRAM_NAME)r.fs $(PROGRAM_NAME).nw > $(PROGRAM_NAME)r.fs

$(PROGRAM_NAME).tex: 
	noweave -latex -index -delay $(PROGRAM_NAME).nw > $(PROGRAM_NAME).tex

$(PROGRAM_NAME).dvi: $(PROGRAM_NAME).tex
	latex -quiet $(PROGRAM_NAME).tex && latex -quiet $(PROGRAM_NAME).tex

$(PROGRAM_NAME).ps: $(PROGRAM_NAME).dvi
	dvips -q* $(PROGRAM_NAME).dvi

$(PROGRAM_NAME).pdf: $(PROGRAM_NAME).ps
	ps2pdf $(PROGRAM_NAME).ps

$(PROGRAM_NAME)r.exe: $(PROGRAM_NAME).fs $(PROGRAM_NAME)r.fs
	fsharpc $(PROGRAM_NAME).fs $(PROGRAM_NAME)r.fs --target:winexe --nowarn:40 --nowarn:9

dist:
	tar -cf $(PROGRAM_NAME).tar $(DIST_FILES) Makefile && gzip $(PROGRAM_NAME).tar

clean:
	-rm -f $(PROGRAM_NAME).toc $(PROGRAM_NAME).log $(PROGRAM_NAME).tex $(PROGRAM_NAME).aux $(PROGRAM_NAME).dvi $(PROGRAM_NAME).ps $(PROGRAM_NAME).pdf $(PROGRAM_NAME)r.exe

$(PROGRAM_NAME): $(DIST_FILES)

