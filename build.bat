del target/*.d81
del target/*.lst
del target/*.lbl
del target/plus4
64tass --cbm-prg -a src\main.asm -l target\main.lbl -L target\main.lst -o target\fcm
cd target
c1541 -format "fcm,01" d81 fcm.d81
c1541 -attach fcm.d81 -write fcm fcm
cd ..