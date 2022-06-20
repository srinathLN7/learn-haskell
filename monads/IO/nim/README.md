# Nim

Nim -> Remove stars at end of single row. Player that empties the board first wins.

### Output


```
Prelude> :l nim.hs
[1 of 1] Compiling Nim              ( nim.hs, interpreted )
Ok, one module loaded.
*Nim> nim

1: "*****"

2: "****"

3: "***"

4: "**"

5: "*"

"Playing now --> Player:1"

Enter row number: 1
Enter number of stars to remove: 5

1: ""

2: "****"

3: "***"

4: "**"

5: "*"

"Playing now --> Player:2"

Enter row number: 2
Enter number of stars to remove: 4

1: ""

2: ""

3: "***"

4: "**"

5: "*"

"Playing now --> Player:1"

Enter row number: 3
Enter number of stars to remove: 3

1: ""

2: ""

3: ""

4: "**"

5: "*"

"Playing now --> Player:2"

Enter row number: 4
Enter number of stars to remove: 2

1: ""

2: ""

3: ""

4: ""

5: "*"

"Playing now --> Player:1"

Enter row number: 5
Enter number of stars to remove: 1

1: ""

2: ""

3: ""

4: ""

5: ""

Player 1 wins!!!
*Nim>  
```
