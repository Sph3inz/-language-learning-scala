@echo off
echo ==== Language Learning Bot ====

rem Clean up previous compilation
rmdir /s /q bin 2>nul
mkdir bin

rem Compile the Scala code
echo Compiling Scala files...
scalac -d bin src\main\scala\Main.scala src\main\scala\ChatbotCore.scala src\main\scala\AnalyticsDashboard.scala src\main\scala\Models.scala src\main\scala\QuizGenerator.scala

if %ERRORLEVEL% NEQ 0 (
  echo Compilation failed with error code %ERRORLEVEL%.
  goto end
)

echo Compilation successful!
echo Starting Language Learning Bot...
echo.

rem Run the application
scala -cp bin languagelearningbot.Main

:end
echo.
pause
