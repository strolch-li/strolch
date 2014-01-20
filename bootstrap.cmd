@echo off
REM #######################################################################
REM ##
REM ## Bootstrap developing of projects by cloning all the projects.
REM ##
REM #######################################################################

setlocal
setlocal ENABLEDELAYEDEXPANSION

echo.
echo INFO: Cloning all projects...
echo.
for /F  %%i in ('type projects.lst') do (
    set project=%%i

    if exist "..\!project!" (
      echo INFO: Project !project! already exists. Not cloning
    ) else (
      echo INFO: Cloning project !project! ...
      git clone git@github.com:eitch/!project!.git ..\!project!
      if !ERRORLEVEL! NEQ 0 goto :FAIL
      echo.
    )
)

echo.
echo INFO: Done.
echo.
endlocal
exit /b 0

:FAIL
echo INFO: Failed to clone a project.
exit /b !ERRORLEVEL!