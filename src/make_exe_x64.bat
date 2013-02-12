@echo off

rem make sure we use the correct version of python
set PYTHONROOT="c:\soft\Python27"
set PYTHONPATH=%PYTHONROOT%\Lib\site-packages\;

%PYTHONROOT%\python.exe setup.py py2exe