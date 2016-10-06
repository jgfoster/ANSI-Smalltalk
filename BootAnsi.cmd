@ECHO OFF
del AnsiDev.*
cd ..
Dolphin7 DPRO.img7 -q -i ANSI\AnsiDev -d . -x
timeout /T 2 >nul
cd ANSI
start ..\Dolphin7 ANsiDev.img7 -q -f BootAnsi.st
