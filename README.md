Inno Setup
==========

Copyright (C) 1997-2012 Jordan Russell. All rights reserved.  
Portions Copyright (C) 2000-2012 Martijn Laan. All rights reserved.  
For conditions of distribution and use, see LICENSE.TXT.

Source code README

1. Getting Started
------------------

- Obtaining sources

  First you need to download the sources from Github. From the command line do:

  ```
  > git clone git://github.com/jrsoftware/issrc.git is
  > cd is
  > git submodule init
  > git submodule update
  ```

  If you don't have the Git client (`git`), get it from:

  http://git-scm.com/

  To update your sources from the command line do:
  ```
  > git pull
  > git submodule update
  ```

  To be able to contribute to Inno Setup, clone your own fork instead of
  cloning the main Inno Setup repository, commit your work on topic branches
  and make pull requests. See [CONTRIBUTING.md](https://www.github.com/jrsoftware/issrc/blob/master/CONTRIBUTING.md).

- Install Borland Delphi

  Unicode Inno Setup:

  We compile all of Inno Setup's projects under Delphi 2009 with Update 3.

  Non Unicode Inno Setup:

  We compile all of Inno Setup's projects under Delphi 2.01, with the
  exception of Compil32 which is compiled under Delphi 3.02 (for the better
  Far East character set support), and Inno Setup Preprocessor's projects which
  are compiled under Delphi 7.

  Delphi 2.01 is used for the main projects because its VCL has a significantly
  smaller code size footprint than the later versions.

  If you do not have access to these old versions of Delphi, you should be
  able to compile the projects on later versions, however complete
  compatibility is NOT guaranteed. We try to make Inno Setup compilable on
  the later versions when possible, but do not have the resources to test
  every change on every Delphi version.

- Install Microsoft MSXML

  Install Microsoft MSXML 4.0 SP2 if you haven't already done so.
  See http://www.microsoft.com/downloads/details.aspx?FamilyID=3144b72b-b4f2-46da-b4b6-c5d7485f2b42&DisplayLang=en

  If you are not sure whether you have MSXML 4.0 SP2 already, check for a
  file named msxml4.dll in your Windows System directory with a version number
  of 4.20.9818.0 (or later).

  Note: Microsoft MSXML is only needed to be able to compile the help files.

- Install Microsoft HTML Help Workshop

  Install Microsoft HTML Help Workshop if you haven't already done so.
  See http://www.microsoft.com/downloads/details.aspx?familyid=00535334-c8a6-452f-9aa0-d597d16580cc&displaylang=en

  Note: Microsoft HTML Help Workshop is only needed to be able to compile the
  help files.

- Building

  To build all files run build.bat and follow the instructions.

  To just compile Inno Setup run compile-unicode.bat for Unicode Inno Setup or
  compile.bat for Non Unicode Inno Setup and follow the instructions.

  To just compile the Inno Setup help file and its web version run
  ishelp\compile.bat and follow the instructions.

  To just compile the Inno Setup Preprocessor help file and its web version run
  Projects\Ispp\Help\compile.bat and follow the instructions.

2. Delphi 2.0x users ONLY
-------------------------

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
-------------------------

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
-----------

Inno Setup consists of seven projects:

**Compil32** - This is the GUI front-end for the compiler. Compil32 does not
do the actual compilation itself; it relegates it to ISCmplr.dll. If the
ISCmplr project is changed, you normally don't need to recompile Compil32
since it's essentially a text editor, and is not affected by internal
changes to the compiler.  
Non Unicode Inno Setup note: This is the only project that is compiled
under Delphi 3 (3.02 to be exact). The rest of the projects are compiled
under Delphi 2.01.

**ISCC** - This is the command-line front-end to the compiler. Like
Compil32, it depends on ISCmplr.dll to do the actual compiling.

**ISCmplr** - This is a DLL which is loaded by Compil32 and ISCC to compile
scripts. The actual compiler code is in Compile.pas. See CompInt.pas for the
various structures and function declarations used to interface to the DLL.

**Setup** - This is the actual "Setup" program. It displays the wizard, and
performs all (un)installation-related tasks.

**SetupLdr** - This is the "setup loader." It self-extracts a compressed
Setup program into the user's TEMP directory and runs it from there. It also
displays the "This will install..." message box.

**ISPP\ISPPCC** - This is Inno Setup's ISCC command-line front-end with extra
preprocessor related parameters added.

**ISPP\ISPP** - This is a DLL implementing Inno Setup's preprocessor interface

How do the projects link together?

- ISCmplr, ISPP, Setup, and SetupLdr share the unit Struct.pas. This unit
  contains various data structures and constants shared by the projects. If
  Struct.pas is changed, you usually will need to recompile ISCmplr, ISPP,
  Setup, and SetupLdr so that everything is in synch.

- There are more units which are shared between projects. Search the 'uses'
  clauses of the projects and units if you aren't sure if a project uses a
  particular unit.

- ISPP and ISPPCC use various copies of other Inno Setup files. To synch these
  run synch-isfiles.bat.

5. Source code tips
-------------------

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

  ```delphi
  var
      X: Cardinal;
  begin
      if X > Cardinal(1) then
          ...
  ```

  The cast is needed to to work around a bug in Delphi 2 and 3 compilers:
  without it, a *signed* comparison is done.

- A note for those curious: The Setup Compiler creates single EXE Setups by
  first creating the SETUP.EXE as usual, then concatenating the SETUP.0 and
  SETUP-1.BIN to the end of the SETUP.EXE, and finally modifying an internal
  data block in SETUP.EXE so it knows it's in "single EXE" form.

- Starting with the 32-bit version of Inno Setup 1.12.7, the "StripReloc"
  utility is used to decrease the size of the Non Unicode .EXE and .E32 files.

  To download StripReloc, go to:
  http://www.jrsoftware.org/striprlc.php

- Delphi versions prior to 5 store .dfm files in a binary format. To be able to
  track changes to the forms using ordinary "diff" commands, .dfm.txt "mirrors"
  of the binary .dfm files have been created. Each time a .dfm file is
  modified, dfm2text is ran and both the .dfm file and the updated .dfm.txt
  file are commited.

  To download dfm2text, go to:
  http://www.jrsoftware.org/files/misc/dfm2text.zip

6. Precompiled executables and libraries
----------------------------------------

The source code contains several precompiled executables and libaries:

**Files\isbunzip.dll**, **Files\isbzip.dll** - Compiled by Visual Studio 2005
from the bzlib directory in the Iscompress repository.

**Files\isunzlib.dll**, **Files\iszlib.dll** - Compiled by Visual Studio 2005
from the zlib-dll directory in the Iscompress repository.

**Files\islzma.dll**, **Files\islzma32.exe**, **Files\islzma64.exe** - Compiled
by Visual Studio 2005 from the Projects\lzma2\Encoder directory.

**Files\isscint.dll** - Compiled by Visual Studio 2005 from Scintila 2.22 source
code with scintilla-2.22-patch.txt applied.

**Projects\\_shfolder.res** - shfolder.dll from a fresh install of IE 5.5 SP2 on
NT 4.0 stored in a compiled resource file.

**Projects\HelperEXEs.res** - Compiled by Visual Studio 2005 from the
Projects\Helper directory and then stored in a compiled resource file.

**Projects\LzmaDecode\LzmaDecodeInno.obj** - See Projects\LzmaDecode\compiling.txt.

**Projects\lzma2\Decoder\ISLzmaDec.obj**, **Projects\lzma2\Decoder\ISLzma2Dec.obj** -
See Projects\lzma2\Decoder\compiling.txt.

**ishelp\ISHelpGen\ISHelpGen.exe** - See ishelp\ISHelpGen\compile.bat

**Examples\MyProg.exe**, **Examples\MyProg-IA64.exe**, **Examples\MyProg-x64.exe** -
Compiled by Visual Studio 2005 from the Examples\MyProg directory.

7. Inno Setup-specific editing guidelines for the help files
------------------------------------------------------------

- When mentioning something the user would type in a script, e.g. "MinVersion",
  surround it by `<tt></tt>` so that it's displayed in the Courier New font. This is
  a convention used throughout the help file. Example: `<tt>MinVersion</tt>`