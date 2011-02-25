
javac -cp .;..\java\maxent.jar mebridge.java meversion.java
jar cf dismo.jar mebridge.class meversion.class

copy dismo.jar ..\java\dismo.jar /Y
del dismo.jar
del *.class
