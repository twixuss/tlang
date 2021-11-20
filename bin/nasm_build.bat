
@echo off
call "C:\Program Files (x86)\Microsoft Visual Studio\2019\Community\VC\Auxiliary\Build\vcvarsall.bat" x64
F:\tlang\bin\nasm -f win64 -g "F:\tlang\tests\b.asm" -o "F:\tlang\tests\b.obj"
if %errorlevel% neq 0 exit /b %errorlevel%
link "F:\tlang\tests\b.obj" /out:"F:\tlang\tests\b.exe" /entry:"main" /subsystem:console