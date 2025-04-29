@echo off
echo ========== Language Learning Bot Debug Run ==========

:: Create directories for compiled classes
mkdir bin 2>nul
mkdir bin\languagelearningbot 2>nul

:: Compile all Scala files
echo [Step 1] Compiling Scala files...
scalac -d bin -cp scala-library.jar src\main\scala\Main.scala src\main\scala\ChatbotCore.scala src\main\scala\AnalyticsDashboard.scala src\main\scala\Models.scala src\main\scala\QuizGenerator.scala

if %ERRORLEVEL% NEQ 0 (
  echo ERROR: Compilation failed with error code %ERRORLEVEL%.
  goto end
)

echo [Step 2] Compilation successful. Class files generated:
dir bin\languagelearningbot\Main*.class

echo [Step 3] Running application using scala...
echo Command: scala -cp "bin;scala-library.jar" languagelearningbot.Main
echo --------------- Application Output Start ---------------
scala -cp "bin;scala-library.jar" languagelearningbot.Main
echo --------------- Application Output End -----------------
echo Scala exit code: %ERRORLEVEL%

if %ERRORLEVEL% NEQ 0 (
  echo [Step 4] Scala execution failed. Trying Java...
  echo Command: java -cp "bin;scala-library.jar" languagelearningbot.Main
  echo --------------- Application Output Start ---------------
  java -cp "bin;scala-library.jar" languagelearningbot.Main
  echo --------------- Application Output End -----------------
  echo Java exit code: %ERRORLEVEL%
)

:end
echo ========== Debug Run Complete ==========
pause
