@echo off
F:\tlang\bin\nasm -f win64 -g "F:\tlang\tests\b.asm" -o "F:\tlang\tests\b.obj"
if %errorlevel% neq 0 exit /b %errorlevel%
"C:\Program Files (x86)\Microsoft Visual Studio\2019\Community\VC\Tools\MSVC\14.29.30037\bin\Hostx64\x64\link" "F:\tlang\tests\b.obj" /out:"F:\tlang\tests\b.exe" /entry:"main" /subsystem:console /LIBPATH:"C:\Program Files (x86)\Windows Kits\10\Lib\10.0.18362.0\um\x64" kernel32.lib