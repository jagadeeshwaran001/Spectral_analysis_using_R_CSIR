@echo off 

net session>nul 2>&1
if %errorlevel%==0 goto main
echo CreateObject("Shell.Application").ShellExecute "%~f0", "", "", "runas">"%temp%/elevate.vbs"
"%temp%/elevate.vbs"
del "%temp%/elevate.vbs"
cls
exit

:main
  @echo off
  ASSOC .Rexec=RScriptExecutable
  FTYPE RScriptExecutable=C:\Program Files\R\R-3.6.1\binx64\Rscript.exe %1 %*
  cls
  echo "select the R(.r) script file or R executable file(.Rexec)"
  pause
  set dialog="about:<input type=file id=FILE><script>FILE.click();new ActiveXObject
  set dialog=%dialog%('Scripting.FileSystemObject').GetStandardStream(1).WriteLine(FILE.value);
  set dialog=%dialog%close();resizeTo(0,0);</script>"
  cls
  for /f "tokens=* delims=" %%p in ('mshta.exe %dialog%') do set "file=%%p"
  cls
  Rscript %file%
pause