@echo off
F:\tlang\bin\fasm\fasm.exe "F:\tlang\bin\a.asm"
if %errorlevel% neq 0 exit /b %errorlevel%
"C:\Program Files (x86)\Microsoft Visual Studio\2019\Community\VC\Tools\MSVC\14.29.30133\bin\Hostx64\x64\link" "F:\tlang\bin\a.obj" /out:"F:\tlang\bin\a.exe" /nodefaultlib /entry:"main" /subsystem:console /DEBUG:FULL /LIBPATH:"C:\Program Files (x86)\Windows Kits\10\Lib\10.0.18362.0\um\x64" kernel32.lib user32.lib kernel32.lib