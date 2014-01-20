@echo off
REM #######################################################################
REM ##
REM ## Show status of all projects
REM ##
REM #######################################################################

setlocal
setlocal ENABLEDELAYEDEXPANSION

echo.
echo INFO: Showing status of all projects...
echo.
for /F  %%i in ('type projects.lst') do (
  set project=%%i

  if exist "..\!project!" (
      echo INFO: Status of project !project!:
      cd ..\!project!
      git status -s
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
echo INFO: Failed to show status a project.
exit /b !ERRORLEVEL!
