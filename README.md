Simple compiler
==========================

Just a very basic compiler example. Written in Haskell.

Compilation will happen in three fases:

1. Tokinizer / lexer (`Tokenizer.hs`)
2. Parser (`Parser.hs`)
3. Code generation for Sprockell (`CodeGenerator.hs`)

Usage with ghci
----

```bash
$ ghci Compiler.hs
> execute sampleProg1
```

Sample program
---
```
var n, s;
{ n = 10;
  s = 0;
  if n == 10 { n = 5; } else { n = 11; };
  while n>0
    { s = s+n*2;
      n = n-1;
    };
  write s;
}
```

License
---

This source is subject to the BSD License. Please see the LICENSE file for more information.

All other rights reserved.

THIS CODE AND INFORMATION ARE PROVIDED "AS IS" WITHOUT WARRANTY OF ANY KIND, EITHER EXPRESSED OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE IMPLIED WARRANTIES OF MERCHANTABILITY AND/OR FITNESS FOR A PARTICULAR PURPOSE.

Copyright (c) 2013 Thijs Scheepers.
