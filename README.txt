Vorraussetzungen: JRE 1.6

zum Starten je nach Betriebssystem "run" bzw "run.bat" ausführen. Es wird automatisch kompiliert.
Das Skript "rerun" lauscht auf Dateiänderungen und kompiliert und startet das Programm bei jeder Änderung. Dies ist u.a. beim exportieren vom NoiseEditor in die GameEngine sinnvoll, da dann die Engine automatisch mit der neuen Welt gestartet wird.

Die Simple-Build-Tool-Konsole wird mit sbt gestartet. Befehle dafür: run, ~run, compile, clean. Weitere Informationen dazu: https://github.com/harrah/xsbt/wiki

Der Datei src/main/scala/Config.scala lassen sich die Tastaturbelegungen entnehmen und ändern. Zudem lassen sich hier weitere Änderungen vornehmen. Die Standardbelegungen sind:



Bewegung des Spielers: W-A-S-D, Springen mit SPACE, umherschauen mit Maus

Die Tasten 0-9 wählen einen vordefinierten Hexaeder zum Bauen 

Mouse-Grab: G
Spielerposition zurücksetzen: R
Turbo-Modues (zum Bauen und bewegen)
Engine beenden: Escape
Physik aktivieren/deaktivieren: P
zwischen Freiflug-Kamera und Spieler wechseln: Tabulator

Weitere Toggles:
Debugdraw: F1
Wireframe: F2
Frustumculling: F3
Streaming: F4
Vollbild: F11

Rechtsklick togglet, ob man sich im Bau- oder Grabmodus befindet, und linksklick 
führt die entsprechende Aktion aus.

Bauen:
Beim bauen wird immer der selectierte Hexaeder gebaut. Rotieren lässt sich der 
Hexaeder, indem man die kamera dreht, also von der entsprechenden Seite aus baut.

