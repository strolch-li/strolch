@echo off
REM #######################################################################
REM ##
REM ## Script to pull all projects from upstream
REM ##
REM #######################################################################

setlocal
setlocal ENABLEDELAYEDEXPANSION

echo.
echo INFO: Pulling all projects...
echo.
for /F  %%i in ('type projects.lst') do (
  set project=%%i

  if exist "..\!project!" (
      echo INFO: Pullin project !project! ...
      cd ..\!project!
      git status pull
      if !ERRORLEVEL! NEQ 0 goto :FAIL
      echo.
    ) else (
      echo INFO: Project !project! does not exist. Maybe you need to bootstrap?
    )
)

echo.
echo INFO: Done.
echo.
endlocal
exit /b 0

:FAIL
echo INFO: Failed to pull a project.
exit /b !ERRORLEVEL!
