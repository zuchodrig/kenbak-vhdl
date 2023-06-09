@echo off
rem replace path to kenbak-1-isa.yaml with actual path
bespokeasm compile -c bespokeasm\examples\kenbak-1\kenbak-1-isa.yaml test-shifts.kb1 -o test-shifts.bin
..\utils\bin2mem.py  test-shifts.bin > ..\gddl\kenbak.out




 