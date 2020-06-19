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
tinyc -i:"a = 100"
tinyc --interpret:"a = 100"
```

to interpret a file
```
tinyc -f:myfile.c
tinyc --file:myfile.c
```

to run tests
```
tinyc -t
tinyc --test
```

