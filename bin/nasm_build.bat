
@echo off
call "C:\Program Files (x86)\Microsoft Visual Studio\2019\Community\VC\Auxiliary\Build\vcvarsall.bat" x64
nasm -g -f win64 "F:\tlang\bin\a.nasm" -o "F:\tlang\bin\a.obj"
if %errorlevel% neq 0 exit /b %errorlevel%
link kernel32.lib "F:\tlang\bin\a.obj" /out:"F:\tlang\bin\a.exe" /entry:"main" /subsystem:console