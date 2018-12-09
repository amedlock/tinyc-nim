# tinyc-nim
Tinyc ported to Nim

Original Tinyc is  Copyright (C) 2001 by Marc Feeley, All Rights Reserved.

Just wanted to port tinyc to Nim to understand/experiment.

to compile:
```
nim c tinyc.nim
```

to interpret input directly
```
nim -i:"a = 100"
nim --interpret:"a = 100"
```

to interpret a file
```
nim -f:myfile.c
nim --file:myfile.c
```

to run tests
```
nim -t
nim --test
```

