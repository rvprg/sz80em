# Z80 emulator in F#

A Z80 emulator implementation in F#. Uses noweb and LaTeX tools. Run make to get F# source code, TeX, ps, pdf, and a binary file. For license see LICENSE file. Please see IMPORTANT for copyright note about ROM file.

Prerequisites:
 * noweb
 * LaTeX, and LaTeX packages: graphicx, noweb, mflogo, amsmath, textcomp
 * F# compiler

Precompiled binaries are available in [sz80em.tar.gz](https://github.com/rvprg/sz80em/blob/master/sz80em.tar.gz).

The [PDF](https://github.com/rvprg/sz80em/blob/master/sz80em.pdf) file of the book is available [here](https://github.com/rvprg/sz80em/blob/master/sz80em.pdf).

## Table Of Contents
* 1.  About ZX Spectrum
* 2.  Program layout
* 3.  Keyboard Input
* 4.  Display Output
* 5.  Microprocessor
  * 5.1. Registers set
  * 5.2. The Main Class
  * 5.3. ROM and RAM Memory
  * 5.4. Contended Memory
  * 5.5. Contended I/O
  * 5.6. Interrupts
  * 5.7. Instructions
  * 5.8. Instruction Table Initialization
  * 5.9. Extended Instructions
  * 5.10. Bit Instructions Table
  * 5.11. Index Instructions
  * 5.12. Index Bit Instructions
  * 5.13. Initialization 
* 6. The Machine
* 7. Notes
* 8. License

-----
## Images

Z80 emulator running:

![sz80emu](https://rvprg.files.wordpress.com/2016/05/untitled.png)

![sz80emu](https://rvprg.files.wordpress.com/2016/05/untitled1.png)
