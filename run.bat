@echo off
echo Compiling and running Language Learning Bot...

:: Create directories for compiled classes
mkdir bin 2>nul

:: Download Scala library if needed
if not exist "scala-library.jar" (
  echo Downloading Scala library...
  powershell -Command "Invoke-WebRequest -Uri 'https://repo1.maven.org/maven2/org/scala-lang/scala-library/2.13.8/scala-library-2.13.8.jar' -OutFile 'scala-library.jar'"
)

:: Compile all Scala files
echo Compiling Scala files...
javac -cp "scala-library.jar" -d bin src\main\scala\*.scala

:: Run the application 
echo Running application...
java -cp "bin;scala-library.jar" languagelearningbot.Main

echo Done!
pause
