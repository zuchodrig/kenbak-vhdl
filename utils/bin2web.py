# for web emulator
import sys
with open(sys.argv[1], "rb") as f:
  blob = f.read()
  for i in range(0, len(blob)):
    print('{0:03o}'.format(blob[i]))