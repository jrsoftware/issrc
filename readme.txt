Inno Setup
Copyright (C) 1997-2012 Jordan Russell. All rights reserved.
Portions Copyright (C) 2000-2012 Martijn Laan. All rights reserved.
For conditions of distribution and use, see LICENSE.TXT.

Source code README

To compile Inno Setup run compile-unicode.bat for Unicode Inno Setup or
compile.bat for Non Unicode Inno Setup and follow the instructions.

Remarks:

1. Prerequisites
================

- Borland Delphi

  Unicode Inno Setup:

  We compile all of Inno Setup's projects under Delphi 2009 with Update 3.

  Non Unicode Inno Setup:

  We compile all of Inno Setup's projects under Delphi 2.01, with the
  exception of Compil32, which is compiled under Delphi 3.02 (for the better
  Far East character set support). These older versions of Delphi are used
  because their VCLs have a significantly smaller code size footprint than
  the later versions.

  If you do not have access to these old versions of Delphi, you should be
  able to compile the projects on later versions, however complete
  compatibility is NOT guaranteed. We try to make Inno Setup compilable on
  the later versions when possible, but do not have the resources to test
  every change on every Delphi version.

- RemObjects Pascal Script

  Unicode Inno Setup:

  You'll need to obtain the exact commit specified in ROPS-revision.txt
  from the ROPS Git repository. For example, if the commit were
  e5a93a963f785e89810e61e3eb5e2b6ee3efd3e7:

  > cd is\Components
  > git clone git://github.com/remobjects/pascalscript.git UniPs
  > cd UniPs
  > git checkout e5a93a963f785e89810e61e3eb5e2b6ee3efd3e7

  If you don't have the Git client (git), get it from:
  http://git-scm.com/

  The ROPS changelog can be found at:
  https://github.com/remobjects/pascalscript/commits/master

  Non Unicode Inno Setup:

  The ROPS Subversion server suffered a disk crash, grab the revision
  1045a sources here:
  http://files.jrsoftware.org/is/rops/rops-svn-rev-1045a.zip
    (md5sum: d9a646601ea9d7151b43b27cbf1c8ac5)
    
2. Delphi 2.0x users ONLY
=========================

Before you can successfully compile the projects in Delphi 2.0, you must 
do two things:

- Install the latest version of SYSTEM.DCU if you haven't already done so.
  It can be downloaded from Inprise's site at:

  http://www.borland.com/devsupport/delphi/downloads/index.html

  Look for "System.zip" in the "Delphi 2 Items" section.

  If you do not do this, you will probably get an "I/O Error" message
  when trying to run the projects.

- If the files SHLOBJ.DCU and REGSTR.DCU are missing from your Delphi\Lib
  directory, download them here:

  http://www.jrsoftware.org/files/delphi200units.zip
    (md5sum: 94530f3c400c728df897d7d740889487)


3. Component Installation
=========================

If you intend to view or modify the Setup project's forms, you must install
the following component units, which can be found in the Components
directory.

- BidiCtrls
- BitmapImage
- FolderTreeView
- NewCheckListBox
- NewNotebookReg
- NewProgressBar
- NewStaticText
- PasswordEdit
- RichEditViewer

If you intend to view or modify the Compil32 project's forms, you must
additionally install the following components. (Like the Compil32 project
itself, these require Delphi 3 or later.)

- DropListBox
- NewTabSet

If you only want to edit code, then you may skip installation of the
components, and choose "Cancel" if the Delphi IDE tells you a class can't
be found.


4. Overview
===========

Inno Setup consists of seven projects:

Compil32 - This is the GUI front-end for the compiler. Compil32 does not
do the actual compilation itself; it relegates it to ISCmplr.dll. If the
ISCmplr project is changed, you normally don't need to recompile Compil32
since it's essentially a text editor, and is not affected by internal
changes to the compiler.
Non Unicode Inno Setup note: This is the only project that I compile under
Delphi 3 (3.02 to be exact). The rest of the projects are compiled under
Delphi 2.01.

ISCC - This is the command-line front-end to the compiler. Like
Compil32, it depends on ISCmplr.dll to do the actual compiling.

ISCmplr - This is a DLL which is loaded by Compil32 and ISCC to compile
scripts. The actual compiler code is in Compile.pas. See CompInt.pas for the
various structures and function declarations used to interface to the DLL.

Setup - This is the actual "Setup" program. It displays the wizard, and
performs all (un)installation-related tasks.

SetupLdr - This is the "setup loader." It self-extracts a compressed
Setup program into the user's TEMP directory and runs it from there. It also
displays the "This will install..." message box.

ISPP & ISPPCC - See Projects\ISPP\readme.txt for more information.

How do the projects link together?

- ISCmplr, ISPP, Setup, and SetupLdr share the unit Struct.pas. This unit
  contains various data structures and constants shared by the projects. If
  Struct.pas is changed, you usually will need to recompile ISCmplr, ISPP,
  Setup, and SetupLdr so that everything is in synch.

- There are more units which are shared between projects. Search the 'uses'
  clauses of the projects and units if you aren't sure if a project uses a
  particular unit.


5. Source code tips
===================

- If you modify the Setup or SetupLdr projects and want to be able to compile
  your installations with the new code, you'll need to copy the new EXE
  file(s) to the Setup Compiler directory under the extension .E32.

- When debugging the Setup project you should set UseSetupLdr=no in your
  script, and copy the resulting setup-*.bin files to the source code
  directory. This way you can simulate an actual installation while running
  under the Delphi debugger.

- All of the forms in the Setup project, with the exception of Main.dfm, have
  Scaled set to False. This is because they dynamically scale themselves at
  run-time by calling a function named SetFormFont.

- When a Cardinal-type variable is compared against a constant expression,
  why is a Cardinal cast sometimes used around the constant expression?
  For example:

  var
    X: Cardinal;
  begin
    if X > Cardinal(1) then
      ...

  The cast is needed to to work around a bug in Delphi 2 and 3 compilers:
  without it, a *signed* comparison is done.

- A note for those curious: The Setup Compiler creates single EXE Setups by
  first creating the SETUP.EXE as usual, then concatenating the SETUP.0 and
  SETUP-1.BIN to the end of the SETUP.EXE, and finally modifying an internal
  data block in SETUP.EXE so it knows it's in "single EXE" form.

- Starting with the 32-bit version of Inno Setup 1.12.7, I am using my own
  "StripReloc" utility to decrease the size of the Non Unicode .EXE and
  .E32 files.

  To download StripReloc, go to:
  http://www.jrsoftware.org/striprlc.php


6. Precompiled executables and libraries
========================================

The source code contains several precompiled executables and libaries:

Files\isbunzip.dll, Files\isbzip.dll - Compiled by Visual Studio 2005 from
the bzlib directory in the Iscompress repository.

Files\isunzlib.dll, Files\iszlib.dll - Compiled by Visual Studio 2005 from
the zlib-dll directory in the Iscompress repository.

Files\islzma.dll, Files\islzma32.exe, Files\islzma64.exe - Compiled by Visual
Studio 2005 from the Projects\lzma2\Encoder directory.

Files\isscint.dll - Compiled by Visual Studio 2005 from Scintila 2.22 source
code with scintilla-2.22-patch.txt applied.

Projects\HelperEXEs.res - Compiled by Visual Studio 2005 from the
Projects\Helper directory and then stored in a compiled resource file.

Projects\LzmaDecode\LzmaDecodeInno.obj - See Projects\LzmaDecode\compiling.txt.

Projects\lzma2\Decoder\ISLzmaDec.obj, Projects\lzma2\Decoder\ISLzma2Dec.obj -
See Projects\lzma2\Decoder\compiling.txt.

Examples\MyProg.exe, Examples\MyProg-IA64.exe, Examples\MyProg-x64.exe -
Compiled by Visual Studio 2005 from the Examples\MyProg directory.

Examples\MyProg.chm

Examples\MyDll.dll