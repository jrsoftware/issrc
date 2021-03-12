Inno Setup
==========

Copyright (C) 1997-2021 Jordan Russell. All rights reserved.  
Portions Copyright (C) 2000-2021 Martijn Laan. All rights reserved.  
For conditions of distribution and use, see LICENSE.TXT.

Source code README

Getting Started
---------------

1. **Obtain sources**

    First you need to download the sources from Github. From the command line do:

    ```
    > git clone https://github.com/jrsoftware/issrc.git is
    > cd is
    > git submodule init
    > git submodule update
    ```

    If you don't have the Git client (`git`), get it from:

    https://git-scm.com/

    To update your sources from the command line do:
    ```
    > git pull
    > git submodule update
    ```

    To be able to contribute to Inno Setup, clone your own fork instead of
    cloning the main Inno Setup repository, commit your work on topic branches
    and make pull requests. See [CONTRIBUTING.md].


2. **Install Embarcadero Delphi**

   We compile all of Inno Setup's projects under Delphi 10.3.3 Rio.

   If you do not have access to this version of Delphi, you should be
   able to compile the projects on later versions, however complete
   compatibility is NOT guaranteed. We try to make Inno Setup compilable on
   the later versions when possible, but do not have the resources to test
   every change on every Delphi version.
  
   There's a free version of Delphi available called the Community Edition.
   See https://www.embarcadero.com/products/delphi/starter/free-download


3. **Install Microsoft MSXML**

   Install Microsoft MSXML 4.0 SP2 if you haven't already done so.
   See https://www.microsoft.com/en-us/download/details.aspx?id=19662

   If you are not sure whether you have MSXML 4.0 SP2 already, check for a
   file named msxml4.dll in your Windows System directory with a version number
   of 4.20.9818.0 (or later).

   Note: Microsoft MSXML is only needed to be able to compile the help files.


4. **Install Microsoft HTML Help Workshop**

   Install Microsoft HTML Help Workshop if you haven't already done so.
   See https://www.microsoft.com/en-us/download/details.aspx?id=21138

   Note: Microsoft HTML Help Workshop is only needed to be able to compile the
   help files.


5. **Build Inno Setup**

   To build all files run **build.bat** and follow the instructions.

   To just compile Inno Setup run **compile.bat** and follow the instructions.

   To just compile the Inno Setup help file and its web version run
   **ISHelp\compile.bat** and follow the instructions.

   To just compile the Inno Setup Preprocessor help file and its web version run
   **Projects\Ispp\Help\compile.bat** and follow the instructions.


Component Installation
----------------------

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
additionally install the following components.

- DropListBox
- NewTabSet

The Components directory contains a Components project which you can use to
install all these components.

If you only want to edit code, then you may skip installation of the
components, and choose "Cancel" if the Delphi IDE tells you a class can't
be found.


Overview
--------

Inno Setup consists of six projects:

**Compil32** - This is the GUI front-end for the compiler. Compil32 does not
do the actual compilation itself; it relegates it to ISCmplr.dll. If the
ISCmplr project is changed, you normally don't need to recompile Compil32
since it's essentially a text editor, and is not affected by internal
changes to the compiler.

**ISCC** - This is the command-line front-end to the compiler. Like
Compil32, it depends on ISCmplr.dll to do the actual compiling.

**ISCmplr** - This is a DLL which is loaded by Compil32 and ISCC to compile
scripts. The actual compiler code is in Compile.pas. See CompInt.pas for the
various structures and function declarations used to interface to the DLL.

**Setup** - This is the actual "Setup" program. It displays the wizard, and
performs all (un)installation-related tasks.

**SetupLdr** - This is the "setup loader." It self-extracts a compressed
Setup program into the user's TEMP directory and runs it from there. It also
displays the "This will install..." and /HELP message boxes.

**ISPP\ISPP** - This is a DLL implementing Inno Setup's preprocessor interface.

How do the projects link together?

- ISCmplr, ISPP, Setup, and SetupLdr share the unit Struct.pas. This unit
  contains various data structures and constants shared by the projects. If
  Struct.pas is changed, you usually will need to recompile ISCmplr, ISPP,
  Setup, and SetupLdr so that everything is in synch.

- There are more units which are shared between projects. Search the 'uses'
  clauses of the projects and units if you aren't sure if a project uses a
  particular unit.

- The ISPP help file uses various copies of other Inno Setup files. To synch
  these run **synch-isfiles.bat**.


Source code tips
----------------

- If you modify the Setup or SetupLdr projects and want to be able to compile
  your installations with the new code, you'll need to copy the new EXE
  file(s) to the Setup Compiler directory under the extension .E32.

- When debugging the Setup project you should set ``UseSetupLdr=no`` and
  ``OutputBaseFilename=setup`` in your script, and copy the resulting setup-*.bin
  files to the source code directory. This way you can simulate an actual
  installation while running under the Delphi debugger.
  
- All of the forms in the Setup project, with the exception of Main.dfm, have
  Scaled set to False. This is because they dynamically scale themselves at
  run-time by calling a function named InitializeFont.

- A note for those curious: The Setup Compiler creates single EXE Setups by
  first creating the SETUP.EXE as usual, then concatenating the SETUP.0 and
  SETUP-1.BIN to the end of the SETUP.EXE, and finally modifying an internal
  data block in SETUP.EXE so it knows it's in "single EXE" form.

- For compiler debugging purposes define ``STATICCOMPILER`` in CompForm.pas
  and for preprocessor debugging also ``STATICPREPROC`` in Compile.pas.

- To debug the uninstaller first run Setup.exe to completion with the
  ``/DETACHEDMSG`` command line parameter set. Afterwards copy uninst000.dat and
  uninst000.msg as setup.dat and setup.msg to the Projects directory in your
  issrc path. Then open the Setup project and set the command line parameters to
  ``/UNINSTMODE "/SECONDPHASE=<your issrc path\Projects\Setup.exe"`` and start
  debugging. Note: each time setup.dat and setup.msg will be deleted if you
  allow the uninstaller to complete so make sure to keep copies.


Precompiled executables and libraries
-------------------------------------

The source code contains several precompiled and signed executables and libraries:

**Files\isbunzip.dll**, **Files\isbzip.dll** - Compiled by Visual Studio 2005
from the bzlib directory in the Iscompress repository.

**Files\isunzlib.dll**, **Files\iszlib.dll** - Compiled by Visual Studio 2005
from the zlib-dll directory in the Iscompress repository.

**Files\islzma.dll**, **Files\islzma32.exe**, **Files\islzma64.exe** - Compiled
by Visual Studio 2005 from the [Projects\Lzma2\Encoder] directory.

**Files\isscint.dll** - Compiled by Visual Studio 2005 from Scintilla 2.22 source
code with scintilla-2.22-patch.txt applied.

**Projects\Helper\x64\Release\Helper.exe**, **Projects\HelperEXEs.res** -
Compiled by Visual Studio 2005 from the [Projects\Helper] directory and then
stored in a compiled resource file.

**Projects\LzmaDecode\LzmaDecodeInno.obj** - See [Projects\LzmaDecode\compiling.txt].

**Projects\Lzma2\Decoder\ISLzmaDec.obj**, **Projects\Lzma2\Decoder\ISLzma2Dec.obj** -
See [Projects\Lzma2\Decoder\compiling.txt].

**ISHelp\ISHelpGen\ISHelpGen.exe** - See [ISHelp\ISHelpGen\compile.bat].

**Examples\MyProg.exe**, **Examples\MyProg-x64.exe** - Compiled by Visual Studio
2005 from the [Examples\MyProg] directory.

**Examples\MyProg-ARM64.exe** - Compiled by Visual Studio 2017 from the
[Examples\MyProg] directory.

Inno Setup-specific editing guidelines for the help files
---------------------------------------------------------

- When mentioning something the user would type in a script, e.g. "MinVersion",
  surround it by `<tt></tt>` so that it's displayed in the Courier New font. This is
  a convention used throughout the help file. Example: `<tt>MinVersion</tt>`

<!-- Link references -->
[CONTRIBUTING.md]: <CONTRIBUTING.md>
[Projects\Lzma2\Encoder]: <Projects/Lzma2/Encoder>
[Projects\Helper]: <Projects/Helper>
[Examples\MyProg]: <Examples/MyProg>
[Projects\LzmaDecode\compiling.txt]: <Projects/LzmaDecode/compiling.txt>
[Projects\Lzma2\Decoder\compiling.txt]: <Projects/Lzma2/Decoder/compiling.txt>
[ISHelp\ISHelpGen\compile.bat]: <ISHelp/ISHelpGen/compile.bat>
