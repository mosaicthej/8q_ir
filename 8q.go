// a program to solve the 8-queens problem
const N = 8
type boolArray []boolean
type intArray []int
var row boolArray = [N]boolean
var col intArray = [N]int
var diag1 boolArray = [N+N-1]boolean
var diag2 boolArray = [N+N-1]boolean

func printBoard() {
  for i:=0; i<N; i=i+1 {
    for j:=0; j<N; j=j+1 {
      if col[i]==j { write("O")
      } else { write(".") }
    }
    write("\n")
  }
  write("\n")
}

func try (c int) {
  if c==N { 
    printBoard()
  } else {
    for r:=0; r<N; r=r+1 {
      if !row[r] && !diag1[r+c] && !diag2[r+7-c] {
        row[r] = true; diag1[r+c] = true; diag2[r+7-c] = true
        col[c] = r
        try(c+1)
        row[r] = false; diag1[r+c] = false; diag2[r+7-c] = false
      }
    }
  }
}
// main statement block
try(0);
