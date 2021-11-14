
@echo off
call "C:\Program Files (x86)\Microsoft Visual Studio\2019\Community\VC\Auxiliary\Build\vcvarsall.bat" x64
nasm -f win64 -g "F:\tlang\bin\a.asm" -o "F:\tlang\bin\a.obj"
if %errorlevel% neq 0 exit /b %errorlevel%
link "F:\tlang\bin\a.obj" /out:"F:\tlang\bin\a.exe" /entry:"main" /subsystem:console kernel32.lib