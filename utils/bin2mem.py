# to load in memreg_loadfile vhdl
import sys

with open(sys.argv[1], "rb") as f:
  blob = f.read()
  for i in range(0, len(blob)):
    if blob[i] != 0:
      print('{0:02x} {1:02x}'.format(i, blob[i]))