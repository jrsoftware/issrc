; *** Inno Setup version 5.1.0+ Malay messages ***
;
; To download user-contributed translations of this file, go to:
;   http://www.jrsoftware.org/is3rdparty.php
;
; Note: When translating this text, do not add periods (.) to the end of
; messages that didn't have them already, because on those messages Inno
; Setup adds the periods automatically (appending a period would result in
; two periods being displayed).
;
; $jrsoftware: issrc/Files/Languages/Malaysia-5.1.0.isl,v 1.6 2005/09/17 02:23:56 smd Exp $
;    Diterjemah oleh (Translated by):
;    Shaiffulnizam Mohamad <shaifful at yahoo.com>
;    019-9763528 http://www.mymambo.org
;

[LangOptions]
; The following three entries are very important. Be sure to read and
; understand the '[LangOptions] section' topic in the help file.
LanguageName=Malay
LanguageID=$043E
LanguageCodePage=0
; If the language you are translating to requires special font faces or
; sizes, uncomment any of the following entries and change them accordingly.
;DialogFontName=
;DialogFontSize=8
;WelcomeFontName=Verdana
;WelcomeFontSize=12
;TitleFontName=Arial
;TitleFontSize=29
;CopyrightFontName=Arial
;CopyrightFontSize=8

[Messages]

; *** Application titles
SetupAppTitle=Pemasang Perisian
SetupWindowTitle=Pemasang Perisian - %1
UninstallAppTitle=Mengeluarkan Perisian dari Sistem
UninstallAppFullTitle=Mengeluarkan Perisian %1 dari Sistem

; *** Misc. common
InformationTitle=Maklumat
ConfirmTitle=Sahkan
ErrorTitle=Error

; *** SetupLdr messages
SetupLdrStartupMessage=Program ini akan memasang perisian %1. Adakah anda mahu teruskan?
LdrCannotCreateTemp=Tidak dapat menghasilkan fail sementara, pemasangan dihentikan!
LdrCannotExecTemp=Tidak dapat menjalankan fail dalam direktori sementara, pemasangan dihentikan!

; *** Startup error messages
LastErrorMessage=%1.%n%nError %2: %3
SetupFileMissing=Fail %1 didapati hilang dari direktori pemasangan. Sila perbetulkan masalah tersebut atau dapatkan salinan terbaru perisian ini.
SetupFileCorrupt=Fail Pemasangan ini didapati Rosak. Sila dapatkan salinan terbaru perisian ini.
SetupFileCorruptOrWrongVer=Fail Pemasangan ini didapati Rosak, atau tidak serasi dengan versi pemasangan perisian ini. Sila perbetulkan masalah tersebut atau dapatkan salinan terbaru perisian ini.
NotOnThisPlatform=Perisian ini tidak boleh dijalankan dalam %1.
OnlyOnThisPlatform=Perisian ini Mesti dijalankan dalam %1.
OnlyOnTheseArchitectures=Aplikasi ini hanya boleh dipasang pada versi Windows yang dibina untuk rekabentuk pemprosesan berikut:%n%n%1
MissingWOW64APIs=Versi Windows yang anda jalankan tidak memiliki fungsi yang dikehendaki oleh perisian pemasangan untuk melakukan pemasangan 64-bit. Untuk memperbaikinya, sila pasang Pek Khidmat %1.
WinVersionTooLowError=Perisian ini memerlukan %1 versi %2 atau yang terkemudian.
WinVersionTooHighError=Perisian ini tidak boleh dipasang pada %1 versi %2 atau yang terkemudian.
AdminPrivilegesRequired=Anda mesti masuk kedalam Sistem Komputer ini sebagai pentadbir, apabila memasang perisian ini.
PowerUserPrivilegesRequired=Anda mesti masuk kedalam Sistem Komputer ini sebagai pentadbir atau ahli dalam Kumpulan Power User, apabila memasang perisian ini.
SetupAppRunningError=Perisian Pemasangan ini telah mendapati bahawa terdapat %1 sedang berjalan.%n%nSila tutup semua pemasangan lain tersebut, kemudian klik Ok untuk teruskan atau batal untuk Keluar.
UninstallAppRunningError=Pengeluar Perisian ini telah mendapati bahawa terdapat %1 sedang berjalan.%n%nSila tutup semua pemasangan lain tersebut, kemudian klik Ok untuk teruskan atau batal untuk Keluar.

; *** Misc. errors
ErrorCreatingDir=Pemasang Perisian ini, tidak dapat menghasilkan Direktori "%1"
ErrorTooManyFilesInDir=tidak dapat menghasilkan Fail dalam Direktori "%1" kerana ia mempunyai terlalu banyak fail.

; *** Setup common messages
ExitSetupTitle=Keluar dari Pemasangan
ExitSetupMessage=Pemasangan masih belum sempurna. Jika anda berhenti sekarang, Perisian ini tidak akan dipasang.%n%nAnda boleh menjalankan Pemasangan Perisian ini pada masa akan datang untuk melengkapkan pemasangan.%n%nAdakah anda pasti ingin keluar dari Pemasangan?
AboutSetupMenuItem=&Tentang Pemasang Perisian...
AboutSetupTitle=Tentang Pemasang Perisian
AboutSetupMessage=%1 versi %2%n%3%n%n%1 laman web:%n%4
AboutSetupNote=
TranslatorNote=Dialih bahasa kepada Bahasa Melayu oleh :%nShaiffulnizam Mohamad.%nshaifful@yahoo.com%nHP : 019-9763528%n%nSila hubungi saya sekiranya and ingin ubah maksud atau ayat dalam terjemahan ini

; *** Buttons
ButtonBack=< &Kembali
ButtonNext=&Selepas >
ButtonInstall=&Pasang
ButtonOK=OK
ButtonCancel=&Batal
ButtonYes=&Ya
ButtonYesToAll=Ya Semu&a
ButtonNo=&No
ButtonNoToAll=&Tidak Semua
ButtonFinish=Se&lesai
ButtonBrowse=Li&hat...
ButtonWizardBrowse=Li&hat...
ButtonNewFolder=&Hasilkan Folder Baru

; *** "Select Language" dialog messages
SelectLanguageTitle=Pilih Bahasa Untuk Pemasangan
SelectLanguageLabel=Pilih Bahasa Yang Anda ingin Guna Dalam Pemasangan:

; *** Common wizard text
ClickNext=Klik Selepas untuk Teruskan, atau Batal untuk keluar dari Pemasangan Perisian ini.
BeveledLabel=
BrowseDialogTitle=Lihat untuk folder
BrowseDialogLabel=Pilih folder dari senarai dibawah dan klik OK.
NewFolderName=Folder Baru

; *** "Welcome" wizard page
WelcomeLabel1=Selamat Datang kepada Pemasang Perisian [name]
WelcomeLabel2=Perisian ini akan memasang perisian [name/ver] pada komputer anda.%n%nAdalah dinasihatkan untuk menghentikan Perisian-perisian lain sebelum anda meneruskan Pemasangan ini.

; *** "Password" wizard page
WizardPassword=Kata laluan
PasswordLabel1=Pemasangan ini dilindungi dengan kata laluan.
PasswordLabel3=Sila sediakan kata laluan, kemudian Klik Selepas untuk meneruskan pemasangan. Kata laluan adalah Huruf Sensitif. Cth : A tidak sama dengan a
PasswordEditLabel=&Kata laluan:
IncorrectPassword=Kata laluan yang anda berikan tidak Sah!, Sila cuba lagi.

; *** "License Agreement" wizard page
WizardLicense=Lesen Perjanjian
LicenseLabel=Sila baca Maklumat Penting berikut, sebelum anda meneruskan pemasangan perisian ini.
LicenseLabel3=Sila baca Lesen Perjanjian berikut. Anda mesti menerima terma dalam perjanjian ini, sebelum meneruskan pemasangan.
LicenseAccepted=Saya &Menerima Terma-terma dalam Perjanjian ini
LicenseNotAccepted=Saya &Tidak menerima Terma-terma dalam Perjanjian ini

; *** "Information" wizard pages
WizardInfoBefore=Maklumat
InfoBeforeLabel=Sila Baca Maklumat Penting Berikut, sebelum meneruskan Pemasangan.
InfoBeforeClickLabel=Jika anda bersedia untuk meneruskan pemasangan, klik Selepas.
WizardInfoAfter=Maklumat
InfoAfterLabel=Sila Baca Maklumat Penting berikut, sebelum meneruskan Pemasangan.
InfoAfterClickLabel=Jika anda bersedia untuk meneruskan Pemasangan, klik Selepas.

; *** "User Information" wizard page
WizardUserInfo=Maklumat Pengguna
UserInfoDesc=Sila masukkan maklumat Anda:
UserInfoName=&Nama:
UserInfoOrg=&Organisasi:
UserInfoSerial=Nombor &Siri:
UserInfoNameRequired=Anda mesti masukkan nama.

; *** "Select Destination Directory" wizard page
WizardSelectDir=Pilih Destinasi Tujuan bagi Direktori
SelectDirDesc=Dimanakah perisian [name] akan dipasang?
SelectDirLabel3=Setup akan memasang perisian [name] kedalam folder berikut.
SelectDirBrowseLabel=Untuk teruskan dengan pemasangan, klik Selepas. Jika anda ingin memilih folder berlainan, klik Browse.
DiskSpaceMBLabel=Perisian in memerlukan Sekurang-kurangnya [mb] MB ruangan Cakera Keras.
ToUNCPathname=Pemasangan tidak boleh dilakukan pada laluan UNC. Jika anda cuba untuk memasangnya kedalam rangkaian, anda perlu memetakan pemacu rangkaian.
InvalidPath=Anda mesti meletakkan Laluan Penuh dengan Huruf Pemacu; sebagai contoh:%n%nC:\APP%n%nataupun laluan bagi UNC dalam ruangan:%n%n\\server\share
InvalidDrive=Pemacu atau perkongsian UNC yang anda pilih, tidak wujud atau tidak boleh diakses. Sila pilih yang lain.
DiskSpaceWarningTitle=Ruangan Cakera Keras TIDAK MENCUKUPI!
DiskSpaceWarning=Pemasang Perisian memerlukan sekurang-kurangnya %1 KB daripada ruangan kosong untuk pemasangan, tetapi cakera keras/pemacu yang anda pilih hanya mempunyai %2 KB sahaja.%n%nAdakah anda ingin teruskan juga?
DirNameTooLong=Nama folder atau laluan terlalu panjang.
InvalidDirName=Nama folder tidak sah.
BadDirName32=Nama direktori, tidak boleh termasuk mana-mana huruf atau karektor berikut:%n%n%1
DirExistsTitle=Direktori telah pun Wujud
DirExists=Direktori:%n%n%1%n%ntelahpun Wujud. Adakah anda masih ingin memasang perisian ini ke Direktori tersebut?
DirDoesntExistTitle=Direktori tidak Wujud!
DirDoesntExist=Direktori:%n%n%1%n%ntidak Wujud!. Adakah anda ingin Direktori tersebut dihasilkan?

; *** "Select Components" wizard page
WizardSelectComponents=Pilih Komponen atau bahagian
SelectComponentsDesc=Komponen yang manakah perlu dipasang?
SelectComponentsLabel2=Pilih Komponen yang anda ingin pasangkan; Kosongkan Komponen yang anda tidak ingin pasangkan. Klik Selepas, jika anda telah bersedia untuk meneruskan pemasangan.
FullInstallation=Pemasangan Penuh
; if possible don't translate 'Compact' as 'Minimal' (I mean 'Minimal' in your language)
CompactInstallation=Pemasangan Asas
CustomInstallation=Pemasangan atas Pilihan
NoUninstallWarningTitle=Komponen Wujud
NoUninstallWarning=Pemasang Perisian ini telah mendapati bahawa komponen berikut telah dipasang pada Komputer anda:%n%n%1%n%nTidak memilih Komponen ini, tidak akan menyebabkan ianya dikeluarkan dari Pemasangan.%n%nAdakah anda ingin juga teruskan?
ComponentSize1=%1 KB
ComponentSize2=%1 MB
ComponentsDiskSpaceMBLabel=Pilihan Sekarang ini memerlukan sekurang-kurangnya [mb] MB Ruangan cakera keras.

; *** "Select Additional Tasks" wizard page
WizardSelectTasks=Pilih Tugas Tambahan
SelectTasksDesc=Apakah tugas tambahan yang anda ingin perisian pemasangan ini lakukan?
SelectTasksLabel2=Pilih Tugas Tambahan yang anda ingin lakukan semasa Proses Pemasangan [name], Kemudian pilih butang Selepas.

; *** "Select Start Menu Folder" wizard page
WizardSelectProgramGroup=Pilih Folder Start Menu
SelectStartMenuFolderDesc=Dimanakah Sepatutnya pemasang perisian ini harus meletakkan pintasan kepada Perisian ini?
SelectStartMenuFolderLabel3=Setup akan hasilkan pintasan dalam folder menu mula berikut.
SelectStartMenuFolderBrowseLabel=Untuk teruskan, klik selepas. Jika anda nak pilih folder berlainan, klik lihat.
MustEnterGroupName=Anda mesti masukkan nama Folder.
GroupNameTooLong=Nama folder atau laluan terlalu panjang.
InvalidGroupName=Nama folder tidak SAH.
BadGroupName=Nama folder tidak boleh mengandungi mana-mana huruf atau karektor berikut:%n%n%1
NoProgramGroupCheck2=&Jangan hasilkan folder Start Menu

; *** "Ready to Install" wizard page
WizardReady=Sedia untuk Pemasangan
ReadyLabel1=Pemasang Perisian telah bersedia untuk memulakan pemasangan [name] kedalam sistem komputer anda.
ReadyLabel2a=Klik Pasang untuk meneruskan Pemasangan Perisian, atau klik Kembali jika anda ingin Lihat atau Ubah mana-mana tetapan.
ReadyLabel2b=Klik Pasang untuk meneruskan Pemasangan.
ReadyMemoUserInfo=Maklumat Pengguna:
ReadyMemoDir=Direktori Destinasi:
ReadyMemoType=Jenis Pemasangan:
ReadyMemoComponents=Komponen terpilih:
ReadyMemoGroup=Folder Start Menu:
ReadyMemoTasks=Tugasan tambahan:

; *** "Preparing to Install" wizard page
WizardPreparing=Bersedia untuk memasang
PreparingDesc=Pemasang Perisian [name] sedang menyediakan Sistem komputer anda untuk proses pemasangan.
PreviousInstallNotCompleted=Pemasangan/Pengeluaran Perisian sebelumnya adalah tidak lengkap. Anda perlu memulakan kemmbali Komputer untuk menyiapkan pemasangan tersebut.%n%nSelpas memulakan kembali Komputer anda, jalankan Pemasang Perisian ini kembali untuk melengkapkan Pemasangan [name] ini.
CannotContinue=Pemasangan tidak boleh diteruskan, Sila klik batal untuk Keluar.

; *** "Installing" wizard page
WizardInstalling=Pemasangan Perisian
InstallingLabel=Sila tunggu sementara Pemasang Perisian memasang [name] pada komputer anda.

; *** "Setup Completed" wizard page
FinishedHeadingLabel=Melengkapkan Pemasangan [name]
FinishedLabelNoIcons=Pemasang Perisian telah selesai memasang perisian [name] pada komputer anda.
FinishedLabel=Pemasang Perisian telah selesai memasang perisian [name] pada komputer anda. Perisian tersebut boleh dimulakan dengan memilih Ikon yang dipasang.
ClickFinish=Klik Selesai untuk keluar dari Pemasang Perisian.
FinishedRestartLabel=Untuk melengkapkan pemasangan perisian [name], Pemasang perisian mesti memulakan kembali komputer anda, adakah anda ingin Mulakan Kembali Komputer anda sekarang?
FinishedRestartMessage=Untuk melengkapkan pemasangan perisian  [name], Pemasang perisian mesti memulakan kembali komputer anda.%n%nadakah anda mahu Mulakan Kembali Komputer anda sekarang??
ShowReadmeCheck=Ya, Saya nak baca fail READ ME
YesRadio=&Ya, mulakan kembali komputer ini sekarang
NoRadio=&Tidak, Saya akan mulakan kembali Komputer saya kemudian
; used for example as 'Run MyProg.exe'
RunEntryExec=Menjalankan %1
; used for example as 'View Readme.txt'
RunEntryShellExec=Lihat %1

; *** "Setup Needs the Next Disk" stuff
ChangeDiskTitle=Pemasang perisian memerlukan cakera selepas ini
SelectDiskLabel2=Sila masukkan cakera %1 dan Klik OK.%n%nJika fail pada Cakera @ Disk ini boleh ditemui dalam folder selain yang dinyatakan berikut, masukkan laluan yang betul atau klik lihat
PathLabel=&Laluan:
FileNotInDir2=Fail "%1" tidak ditemui dalam "%2". Sila masukkan Cakera yang betul atau pilih Folder lain.
SelectDirectoryLabel=Sila berikan Lokasi Cakera@Disk selepasnya.

; *** Installation phase messages
SetupAborted=Pemasangan Perisian tidak Lengkap.%n%nSila baiki masalah tersebut dan jalankan Pemasang Perisian  ini kembali..
EntryAbortRetryIgnore=Klik Retry untuk mencuba sekali lagi, atau Ignore untuk teruskan sahaja, atau Abort untuk batalkan pemasangan perisian.

; *** Installation status messages
StatusCreateDirs=Menghasilkan direktori...
StatusExtractFiles=Mengekstrak Fail-fail...
StatusCreateIcons=Menghasilkan ikon bagi perisian
StatusCreateIniEntries=Menghasilkan kemasukkan data INI 
StatusCreateRegistryEntries=Menghasilkan kemasukkan registry...
StatusRegisterFiles=Mendaftarkan Fail-fail
StatusSavingUninstall=Menyimpan maklumat pengeluaran semula...
StatusRunProgram=Menamatkan Pemasangan...
StatusRollback=Mengundurkan kembali semua perubahan..

; *** Misc. errors
ErrorInternal2=Masalah Dalaman: %1
ErrorFunctionFailedNoCode=%1 Gagal
ErrorFunctionFailed=%1 Gagal; Kod %2
ErrorFunctionFailedWithMessage=%1 Gagal; Kod %2.%n%3
ErrorExecutingProgram=Tidak dapat memulakan fail program:%n%1

; *** Registry errors
ErrorRegOpenKey=Terdapat masalah membuka kekunci registry :%n%1\%2
ErrorRegCreateKey=Masalah dalam menghasilkan kekunci registry:%n%1\%2
ErrorRegWriteKey=Masalah untuk menulis kekunci registry:%n%1\%2

; *** INI errors
ErrorIniEntry=Terdapat masalah menghasilkan kemasukan INI dalam fail "%1".

; *** File copying errors
FileAbortRetryIgnore=Klik Retry untuk mencuba kembali, Ignore untuk langkau fail ini (Tidak digalakkan), atau Henti untuk Batalkan Pemasangan.
FileAbortRetryIgnore2=Klik Retry untuk mencuba kembali, Ignore untuk teruskan juga (Tidak digalakkan), atau Henti untuk Batalkan Pemasangan.
SourceIsCorrupted=Sumber bagi fail ini Rosak
SourceDoesntExist=Sumber fail bagi "%1" tidak wujud
ExistingFileReadOnly=Fail yang telah wujud ditanda sebagai Baca-sahaja atau read-only.%n%nKlik Cuba Semula untuk keluarkan attribut baca-sahaja dan cuba sekali lagi. Ignore untuk langkau Fail ini, atau Abort untuk Batalkan pemasangan perisian.
ErrorReadingExistingDest=Masalah Berlaku apabila cuba membaca fail yang telah wujud.
FileExists=Fail tersebut telah wujud.%n%nAdakah anda mahu pemasang perisian untuk menulisnya kembali?
ExistingFileNewer=Fail sedia ada adalah lebih baru daripada fail yang cuba dipasang oleh pemasang perisian ini. Adalah dicadangkan supaya anda menyimpan fail sedia ada tersebut.%n%nAdakah anda ingin menyimpan fail sedia ada tersebut?
ErrorChangingAttr=Berlaku masalah apabila cuba mengubah atribut fail sedia ada:
ErrorCreatingTemp=Masalah berlaku apabila cuba menghasilkan fail dalam direktori destinasi:
ErrorReadingSource=Masalah berlaku apabila cuba membaca fail sumber:
ErrorCopying=Masalah berlaku apabila cuba menyalin fail:
ErrorReplacingExistingFile=Masalah berlaku apabila cuba menggantikan fail sedia ada:
ErrorRestartReplace=Gagal Mula Semula:
ErrorRenamingTemp=Masalah berlaku apabila cuba untuk menamakan fail dalam direktori destinasi:
ErrorRegisterServer=Tidak dapat daftarkan DLL/OCX: %1
ErrorRegisterServerMissingExport=Eksport DllRegisterServer tidak dijumpai
ErrorRegisterTypeLib=Tidak dapat mendaftarkan jenis library: %1

; *** Post-installation errors
ErrorOpeningReadme=Masalah berlaku apabila cuba membuka fail README.
ErrorRestartingComputer=Pemasang Perisian tidak dapat memulakan kembali komputer ini. Sila lakukan secara manual.

; *** Uninstaller messages
UninstallNotFound=Fail "%1" tidak wujud. Pengeluaran Perisian tidak boleh dilakukan.
UninstallOpenError=Fail "%1" tidak dapat dibuka. proses mengeluarkan perisian tidak dapat dilakukan
UninstallUnsupportedVer=Fail daftar pengeluaran "%1" berada dalam format yang tidak dikenali oleh Versi Pengeluar ini. Pengeluaran tidak boleh dilakukan
UninstallUnknownEntry=Daftar masuk yang tidak dikenali (%1) dijumpai dalam daftar pengeluaran.
ConfirmUninstall=Adakah anda pasti ingin mengeluarkan %1 sepenuhnya bersama kesemua komponennya?
UninstallOnlyOnWin64=Setup ini hanya boleh melakukan pengeluaran perisian pada Windows 64-bit.
OnlyAdminCanUninstall=Pengeluaran Perisian ini hanya boleh dilakukan oleh pengguna yang mempunyai kelebihan sebagai Pentadbir.
UninstallStatusLabel=Sila tunggu semasa %1 idikeluarkan dari komputer anda.
UninstalledAll=%1 Berjaya dikeluarkan dari Komputer anda.
UninstalledMost=Pengeluaran %1 telah selesai%n%nSesetengah fail tidak boleh dikeluarkan. Ianya boleh dikeluarkan secara manual..
UninstalledAndNeedsRestart=Untuk melengkapkan pengeluaran %1, komputer anda mesti dimulakan kembali.%n%nMahukah ada memulakan komputer kembali Sekarang?
UninstallDataCorrupted=Fail "%1" didapati rosak. Pengeluaran tidak boleh dilakukan.

; *** Uninstallation phase messages
ConfirmDeleteSharedFileTitle=Keluarkan Fail Kongsi?
ConfirmDeleteSharedFile2=Sistem telah mengenal pasti bahawa Fail berikut tidak lagi digunakan oleh mana-mana program. Adakah anda mahu mengeluarkan Fail ini?%n%nJika ada mana-mana perisian menggunakan Fail ini dan ianya telah dikeluarkan dari sistem, perisian tersebut mungkin tidak dapat berfungsi dengan baik atau tidak berfungsi langsung. Jika tidak pasti sila klik Tidak, membiarkan Fail ini pada sistem anda tidak akan memberi sebarang kesan.
SharedFileNameLabel=Nama Fail:
SharedFileLocationLabel=Lokasi:
WizardUninstalling=Status Pengeluaran
StatusUninstalling=%1 dikeluarkan...

; The custom messages below aren't used by Setup itself, but if you make
; use of them in your scripts, you'll want to translate them.

[CustomMessages]

NameAndVersion=%1 versi %2
AdditionalIcons=Ikon tambahan:
CreateDesktopIcon=Hasilkan ikon &desktop
CreateQuickLaunchIcon=Hasilkan ikon &Lancar Pantas
ProgramOnTheWeb=%1 di Internet
UninstallProgram=Uninstall %1
LaunchProgram=Jalankan perisian %1
AssocFileExtension=&Kaitkan %1 dengan akhiran fail %2
AssocingFileExtension=Mengaitkan %1 dengan akhiran fail %2 ...