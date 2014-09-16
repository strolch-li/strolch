@echo off
REM #######################################################################
REM ##
REM ## Shows how far ahead each project is against origin
REM ##
REM #######################################################################

setlocal
setlocal ENABLEDELAYEDEXPANSION

echo.
echo INFO: Showing ahead status of all projects...
echo.
for /F  %%i in ('type projects.lst') do (
  set project=%%i

  if not exist "..\!project!" (
    echo INFO: Project !project! does not exist. Maybe you need to bootstrap?
  ) else (
    cd ..\!project!
    set tmpFile=%RANDOM%%RANDOM%.tmp
    git rev-list origin..master --count > !tmpFile!
    if !ERRORLEVEL! NEQ 0 (
      del !tmpFile!
      goto :FAIL
    )
    for /F %%i in ('type !tmpFile!') do (
      set aheadCount=%%i
      if !aheadCount! NEQ 0 (
        echo INFO: !aheadCount! commits not pushed for project !project!
      ) else (
        echo INFO: Project !project! is clean.
      )
    )
    del !tmpFile!
    if !ERRORLEVEL! NEQ 0 goto :FAIL
  )
)

echo.
echo INFO: Done.
echo.
endlocal
exit /b 0

:FAIL
echo INFO: Failed to show ahead status a project.
exit /b !ERRORLEVEL!