open Printf

let rec factorial n = if n <= 1 then 1 else n * factorial (n - 1)
let max n m = if n > m then n else m;;

printf "Should be 5: %d\n" (max 5 4);
printf "Should be 4: %d\n" (max 3 4);
printf "Should be 4: %d\n" (max 4 4);
printf "Should be 6 (3!): %d\n" (factorial 3)
