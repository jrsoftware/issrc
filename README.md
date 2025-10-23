Inno Setup
==========

Copyright (C) 1997-2025 Jordan Russell. All rights reserved.
Portions Copyright (C) 2000-2025 Martijn Laan. All rights reserved.
For conditions of distribution and use, see LICENSE.TXT.

Source code README

Getting Started
---------------

1. **Obtain sources**

    First you need to download the sources from Github. From the command line do:

    ```
    > git clone https://github.com/jrsoftware/issrc.git issrc
    > cd issrc
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

   We compile all of Inno Setup's projects under Delphi 12.3 Athens with the
   May Patch installed.

   There's a free version of Delphi available called the Community Edition.
   See https://www.embarcadero.com/products/delphi/starter/free-download.

3. **Install Microsoft HTML Help Workshop**

   Install Microsoft HTML Help Workshop if you haven't already done so.
   See https://docs.microsoft.com/en-us/previous-versions/windows/desktop/htmlhelp/microsoft-html-help-downloads and
   http://web.archive.org/web/20160201063255/http://download.microsoft.com/download/0/A/9/0A939EF6-E31C-430F-A3DF-DFAE7960D564/htmlhelp.exe

   Note: Microsoft HTML Help Workshop is only needed to be able to compile the
   help file.


4. **Build Inno Setup**

   Unfortunately, Embarcadero has removed command line compilation support
   from the Community Edition, which means there's two different build
   scripts.

   Community Edition: To build all files in Release mode run **build-ce.bat**
   and follow the instructions.

   Otherwise: To build all files in Release mode run **build.bat** and follow
   the instructions.

   To just compile Inno Setup in Release mode run **compile.bat** and follow
   the instructions. This batch file cannot be used with the Community Edition,
   open Projects\Projects.groupproj instead.

   To just compile the Inno Setup help file and its web version run
   **ISHelp\ISHelpGen\compile.bat** and **ISHelp\compile.bat** and follow the
   instructions. The former batch file cannot be used with the
   Community Edition, open Projects\Projects.groupproj instead.


Component Installation
----------------------

If you intend to view or modify the Setup project's forms, you must install
the following component units, which can be found in the [Components]
directory.

- BidiCtrls
- BitmapButton
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

The [Components] directory contains a Components.dpk file which you can use to
install all these components.

If you only want to edit code, then you may skip installation of the
components, and choose "Cancel" if the Delphi IDE tells you a class can't
be found.

The [Components] directory also includes various units that are not
installable components; however, they are still considered components
because they can function independently from Inno Setup.

Overview
--------

Inno Setup consists of eight projects:

**Compil32** - This is the GUI front-end for the compiler, also known as
the Compiler IDE. Compil32 does not do the actual compilation itself; it
relegates it to ISCmplr.dll. If the ISCmplr project is changed, you
normally don't need to recompile Compil32 since it's essentially a text
editor, and is not affected by internal changes to the compiler.

**ISCC** - This is the command-line front-end to the compiler. Like
Compil32, it depends on ISCmplr.dll to do the actual compiling.

**ISCmplr** - This is a DLL which is loaded by Compil32 and ISCC to compile
scripts. The actual compiler code is in Compiler.SetupCompiler.pas. See
Shared.CompInt.pas for the various structures and function declarations used
to interface to the DLL.

**ISPP** - This is a DLL implementing Inno Setup's preprocessor interface.

**Setup** - This is the actual "Setup" program. It displays the wizard, and
performs all (un)installation-related tasks.

**SetupCustomStyle** - Identical to Setup, except that it includes code to
support VCL Styles.

**SetupLdr** - This is the "setup loader." It self-extracts a compressed
Setup program into the user's TEMP directory and runs it from there. It also
displays the "This will install..." and /HELP message boxes.

**ISSigTool** - This is a command-line utility which can be used to sign and verify
any of your files. Compil32, ISCC, and ISCmplr use these signatures to verify the
authenticity of a number of DLL, E32 and EXE files before loading them. Note: this
utility does not replace Microsoft's signtool.exe in any way and is in fact not
related to Authenticode Code Signing at all.

How do the projects link together?

- Compil32, ISCmplr, ISPP, Setup, SetupCustomStyle, and SetupLdr share the unit
  Shared.Struct.pas. This unit contains various data structures and constants
  shared by the projects. If Shared.Struct.pas is changed, you usually will need
  to recompile all these projects so that everything is in synch.

- There are more units which are shared between projects. Search the .dpr
  files of the projects if you aren't sure if a project uses a particular
  unit.

Source code tips
----------------

- When building the projects in Debug mode, it outputs to [Projects\Bin] and when
  debugging, it will run from within this directory. To prepare this directory
  with some extra files you must run **Projects\Bin\synch-isfiles.bat**. Running
  the aforementioned **build.bat** or **build-ce.bat** first is not necessary.

- When debugging the Setup project, you should first build all projects in Debug
  mode, then run the Compil32 project and compile the Debug.iss script which
  should open automatically, and finally open and run the Setup project.
  This way you can simulate an actual installation while running under the
  Delphi debugger.
  
- When building the projects in Release mode, it outputs to [Files]. Before
  running Compil32, ensure that all .issig files are up to date.
  
- All of the forms in the Setup project have Scaled set to False. This is
  because they dynamically scale themselves at run-time by calling a function
  named InitializeFont.

- A note for those curious: The Setup Compiler creates single exe Setups by
  first creating the Setup.exe as usual, then concatenating the Setup.0 and
  Setup-1.bin to the end of the Setup.exe, and finally modifying an internal
  data block in Setup.exe so it knows it's in "single exe" form.

- To debug the uninstaller first run Setup.exe to completion with the
  ``/DETACHEDMSG`` command line parameter set. Afterwards copy uninst000.dat and
  uninst000.msg as setup.dat and setup.msg to the [Projects\Bin] directory in your
  issrc path. Then open the Setup project and set the command line parameters to
  ``/UNINSTMODE "/SECONDPHASE=<your issrc path\Projects\Bin\Setup.exe"`` and start
  debugging. Note: each time setup.dat and setup.msg will be deleted if you
  allow the uninstaller to complete so make sure to keep copies.


Precompiled executables and libraries
-------------------------------------

The source code contains several precompiled and code-signed executables and libraries:

**Files\is7z.dll**, **Files\is7zxa.dll**, **Files\is7zxr.dll** - Compiled by
Visual Studio 2022 from 7-Zip source code in the [is7z] repository.

**Files\isbunzip.dll**, **Files\isbzip.dll** - Compiled by Visual Studio 2005
from the bzlib directory in the [iscompress] repository.

**Files\isunzlib.dll**, **Files\iszlib.dll** - Compiled by Visual Studio 2005
from the zlib-dll directory in the [iscompress] repository.

**Files\islzma.dll**, **Files\islzma32.exe**, **Files\islzma64.exe** - Compiled
by Visual Studio 2022 from the [Projects\Src\Compression.LZMACompressor\islzma] directory.

**Files\isscint.dll** - Compiled by Visual Studio 2022 from Scintilla source
code in the [isscint] repository.

**Projects\Src\Setup.HelperEXEs\Helper\x64\Release\Helper.exe**, **Projects\Src\Setup.HelperEXEs.res** -
Compiled by Visual Studio 2005 from the [Projects\Src\Setup.HelperEXEs\Helper] directory and then
stored in a compiled resource file.

**Projects\Src\Compression.LZMADecompressor\Lzma2Decode\ISLzmaDec.obj** -
Compiled by Visual Studio 2022 from the [Projects\Src\Compression.LZMADecompressor\Lzma2Decode] directory.

**Projects\Src\Compression.LZMA1SmallDecompressor\LzmaDecode\LzmaDecodeInno.obj** -
Compiled by Visual Studio 2022 from the [Projects\Src\Compression.LZMA1SmallDecompressor\LzmaDecode] directory.

**Projects\Src\Compression.SevenZipDecoder\7zDecode\IS7zDec.obj** -
Compiled by Visual Studio 2022 from the [Projects\Src\Compression.SevenZipDecoder\7zDecode] directory.

**Examples\MyProg.exe**, **Examples\MyProg-x64.exe**, **Examples\MyProg-Arm64.exe** -
Compiled by Visual Studio 2022 from the [Examples\MyProg] directory.

Each precompiled file is accompanied by an .issig signature file, and can be
verified using Inno Setup Signature Tool and [this public key file].

Inno Setup-specific editing guidelines for the help files
---------------------------------------------------------

- When mentioning something the user would type in a script, e.g. "MinVersion",
  surround it by `<tt></tt>` so that it's displayed in a monospaced font. This is
  a convention used throughout the help file. Example: `<tt>MinVersion</tt>`

Setting up Continuous Integration
---------------------------------

Inno Setup's source code includes a GitHub workflow that performs unattended builds
upon `push` events, it requires some setting up, though.

Note: The following instructions assume that you have a correctly-licensed version
of Delphi installed into `C:\Program Files (x86)\Embarcadero\Studio\23.0`. This may
not be a Community Edition because it does not support command line compilation.
Also ensure your current Delphi license still allows you to copy a subset of the
Delphi files to another machine for the specific purpose of supporting unattended
builds.

First, generate an encrypted `.zip` file containing the files needed to build
Inno Setup using [7-Zip]:

```
cd /d C:\Program Files (x86)\Embarcadero\Studio\23.0
"C:\Program Files\7-Zip\7z.exe" a -mx9 -mem=AES256 -p"<password>" ^
	%USERPROFILE%\issrc-build-env.zip ^
	bin\dcc32.exe bin\rlink32.dll bin\lnk*.dll ^
	lib/win32/release/Sys*.dcu lib/win32/release/*.res ^
	lib/win32/release/System.*.dcu lib/win32/release/System.Generics.*.dcu ^
	lib/win32/release/System.Internal.*.dcu lib/win32/release/System.Net.*.dcu ^
	lib/win32/release/System.Net.HttpClient.*.dcu lib/win32/release/System.Win.*.dcu ^
	lib/win32/release/Vcl.*.dcu lib/win32/release/Vcl.Imaging.*.dcu ^
	lib/win32/release/Winapi.*.dcu
```

Then, upload this encrypted file somewhere public. After that, add its URL as a new repository
[secret] (at https://github.com/YOUR-USER-NAME/issrc/settings/secrets/actions), under the name
`ISSRC_BUILD_ENV_ZIP_URL`, and the password as `ISSRC_BUILD_ENV_ZIP_PASSWORD`.

Finally, indicate that your fork of the repository has those secrets, by adding the
topic `has-issrc-build-env` (click the gear icon next to the "About" label at
https://github.com/YOUR-USER-NAME/issrc to add the topic).

Once that's done, you're set! The next time you push a branch to your fork, the
workflow will be triggered automatically.

To set up automatic synchronization for your fork, first create a Fine-Grained Personal
Access Token (at https://github.com/settings/personal-access-tokens) with access to your
fork or all repositories you own, ensuring it has Read and Write permissions for Contents
and (see below) optionally also for Pull Requests. After that, add this token as a new
repository secret, under the name `ISSRC_BUILD_ENV_SYNC_TOKEN`. Finally, indicate that your
fork has this secret, by adding the topic `has-issrc-build-env-sync-token`. Your fork will
now synchronize daily, and will automatically run the aforementioned build workflow on
changes, if it's configured.

If you also create a draft PR to merge `main` into a branch named `copilot-review`, and you
have included permissions for Pull Requests as well as access to Copilot reviews, then it
will automatically request a review from Copilot once the synchronization completes. You
should then regularly fast-forward your branch to the commit immediately preceding the
head of `main`. Do not fast-forward to the head of `main`, as this will close your PR and
you will not be able to recreate it until another commit is made.

To perform a second unattended build using a different Delphi version, add topic
`has-issrc-build2-env` and secrets `ISSRC_BUILD2_ENV_ZIP_URL` and
`ISSRC_BUILD2_ENV_ZIP_PASSWORD`. Unlike the main build, the second build does not produce
any artifacts.

<!-- Link references -->
[CONTRIBUTING.md]: <CONTRIBUTING.md>
[Projects\Bin]: <Projects/Bin>
[Components]: <Components>
[Files]: <Files>
[Projects\Src\Compression.LZMACompressor\islzma]: <Projects/Src/Compression.LZMACompressor/islzma>
[Projects\Src\Setup.HelperEXEs\Helper]: <Projects/Src/Setup.HelperEXEs/Helper>
[Examples\MyProg]: <Examples/MyProg>
[Projects\Src]: <Projects/Src>
[Projects\Src\Compil32]: <Projects/Src/Compil32>
[Projects\Src\Compression.LZMADecompressor\Lzma2Decode]: <Projects/Src/Compression.LZMADecompressor/Lzma2Decode>
[Projects\Src\Compression.LZMA1SmallDecompressor\LzmaDecode]: <Projects/Src/Compression.LZMA1SmallDecompressor/LzmaDecode>
[Projects\Src\Compression.SevenZipDecoder\7zDecode]: <Projects/Src/Compression.SevenZipDecoder/7zDecode>
[7-Zip]: https://www.7-zip.org/
[secret]: https://docs.github.com/en/actions/security-guides/encrypted-secrets
[this public key file]: https://files.jrsoftware.org/is/misc/def01.ispublickey
[is7z]: https://github.com/jrsoftware/is7z
[iscompress]: https://github.com/jrsoftware/iscompress
[isscint]: https://github.com/jrsoftware/isscint