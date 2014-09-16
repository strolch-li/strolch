@echo off
REM #######################################################################
REM ##
REM ## Distribute all projects by pushing to upstream
REM ##
REM #######################################################################

setlocal
setlocal ENABLEDELAYEDEXPANSION

echo.
echo INFO: Pushing all projects...
echo.
for /F  %%i in ('type projects.lst') do (
  set project=%%i

  if exist "..\!project!" (
      echo INFO: Pushing project !project! ...
      cd ..\!project!
      git push
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
echo INFO: Failed to push a project.
exit /b !ERRORLEVEL!
