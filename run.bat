@echo off
echo Compiling and running Language Learning Bot...

:: Create directories for compiled classes
mkdir bin 2>nul
mkdir bin\languagelearningbot 2>nul

:: Download Scala components if needed
if not exist "scala-compiler.jar" (
  echo Downloading Scala compiler (2.13.8)...
  powershell -Command "Invoke-WebRequest -Uri 'https://repo1.maven.org/maven2/org/scala-lang/scala-compiler/2.13.8/scala-compiler-2.13.8.jar' -OutFile 'scala-compiler.jar'"
)

if not exist "scala-library.jar" (
  echo Downloading Scala library (2.13.8)...
  powershell -Command "Invoke-WebRequest -Uri 'https://repo1.maven.org/maven2/org/scala-lang/scala-library/2.13.8/scala-library-2.13.8.jar' -OutFile 'scala-library.jar'"
)

if not exist "scala-reflect.jar" (
  echo Downloading Scala reflect (2.13.8)...
  powershell -Command "Invoke-WebRequest -Uri 'https://repo1.maven.org/maven2/org/scala-lang/scala-reflect/2.13.8/scala-reflect-2.13.8.jar' -OutFile 'scala-reflect.jar'"
)

:: Compile all Scala files
echo Compiling Scala files...
java -cp "scala-library.jar;scala-compiler.jar;scala-reflect.jar" scala.tools.nsc.Main -d bin src\main\scala\Main.scala src\main\scala\ChatbotCore.scala src\main\scala\AnalyticsDashboard.scala src\main\scala\Models.scala src\main\scala\QuizGenerator.scala

:: Run the application 
echo Running application...
java -cp "bin;scala-library.jar" languagelearningbot.Main

echo Done!
pause
