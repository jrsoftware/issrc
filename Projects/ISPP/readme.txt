Inno Setup Preprocessor
Copyright (C) 2001-2004 Alex Yackimoff. All Rights Reserved.
Portions by Martijn Laan.

Source code README


1. Prerequisites
================

To compile Inno Setup Preprocessor, you need:

- Borland Delphi

  Unicode Inno Setup Preprocessor:

  We compile all of Inno Setup Preprocessor's projects under Delphi 2009 with Update 3.

  Non Unicode Inno Setup Preprocessor:

  We compile all of Inno Setup Preprocessor's projects under Delphi 7.


2. Overview
===========

Inno Setup Preprocessor consists of two projects:

ISPPCC.dpr - This is Inno Setup's ISCC command-line front-end with extra
preprocessor related parameters added.

ISPP.dpr - This is a DLL implementing Inno Setup's preprocessor interface

How do the projects link together?

- Various copies of Inno Setup files are used. To synch these run
  synch-isfiles.bat.


3. Source code tips
===================

- The "StripReloc" utility is used to decrease the size of the Non Unicode
  ISPPCC.EXE file.

  To download StripReloc, go to:
  http://www.jrsoftware.org/striprlc.php
