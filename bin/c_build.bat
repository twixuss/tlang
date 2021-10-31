
@echo off
call "C:\Program Files (x86)\Microsoft Visual Studio\2019\Community\VC\Auxiliary\Build\vcvarsall.bat" x64
cl F:\tlang\bin\a.tl.nasm /ZI > compile_log.txt