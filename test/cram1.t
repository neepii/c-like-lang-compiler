This test require RISCV64 toolchain, specifically:
 - riscv64-linux-gnu-as
 - riscv64-linux-gnu-ld

  $ test_program () { \
  > dune exec compiler temp_source object > /dev/null; \
  > riscv64-linux-gnu-as -march=rv64gc object -o main_temp.o && riscv64-linux-gnu-as -march=rv64gc $INSIDE_DUNE/../../asm/lib.s -o lib_temp.o && riscv64-linux-gnu-ld main_temp.o lib_temp.o; \
  > qemu-riscv64 -L /usr/riscv64-linux-gnu -cpu rv64 ./a.out
  > }

  $ cat > temp_source << EOF
  > main() begin
  >   return 0;
  > end;
  > EOF
  $ test_program

  $ cat > temp_source << EOF
  > main() begin
  >   return 143;
  > end;
  > EOF
  $ test_program
  [143]

  $ cat > temp_source << EOF
  > main() begin
  >   n := 1 + 2 * 3 + 7 / 7; return n; 
  > end;
  > EOF
  $ test_program
  [8]

  $ cat > temp_source << EOF
  > main() begin
  >   n := 0; 
  > if 1 == 1 then 
  >   begin 
  >     n := 1; 
  >   end;
  >   return n;
  > end;
  > EOF
  $ test_program
  [1]

  $ cat > temp_source << EOF
  > main() begin
  >   n := 0; 
  >   while n < 143 do begin 
  >     n := n + 1; 
  >   end; 
  >   return n;
  > end;
  > EOF
  $ test_program
  [143]

  $ cat > temp_source << EOF
  > extern print_int(n); main() begin
  >   n := 1; print_int(n); return 0; 
  > end;
  > EOF
  $ test_program
  1

Test simple function from asm library
  $ cat > temp_source << EOF
  > extern print_hw(); 
  > main() begin
  >   print_hw(); return 0; 
  > end;
  > EOF
  $ test_program
  Hello, World!

Test imperative-style fibonacci
  $ cat > temp_source << EOF
  > extern print_int(n);
  > main() begin
  >   a := 0;
  >   b := 1;
  >   n := 9;
  >   while n > 1 do begin
  >     b := a + b;
  >     a := b - a;
  >     n := n - 1;
  >   end;
  >   print_int(b);
  >   return 0;
  > end;
  > EOF
  $ test_program
  34

Test imperative-style factorial
  $ cat > temp_source << EOF
  > extern print_int(n);
  > main() begin
  >   acc:=1; n:=6; 
  >   while n > 1 do begin
  >     acc:=acc*n; n:=n-1; 
  >   end; 
  >   print_int(acc);
  >   return 0;
  > end;
  > EOF
  $ test_program
  720

Test constant function
  $ cat > temp_source << EOF
  > exit() begin
  >  return 143 ; 
  > end;
  > main()begin
  >   var := exit(); return var; 
  > end;
  > EOF
  $ test_program
  [143]

Test identity function
  $ cat > temp_source << EOF
  > id(n) begin
  >   return n; 
  > end;
  > main() begin
  >   var := id(142); return var; 
  > end;
  > EOF
  $ test_program
  [142]

  $ cat > temp_source << EOF
  > extern print_int(n); 
  > arith(n) begin
  >   n := n + 1; n := n + 1; b := n + n; return b;
  > end;
  > main () begin
  >  var := arith(5);
  >  print_int(var);
  >  return 0; 
  > end;
  > EOF
  $ test_program
  14

  $ cat > temp_source << EOF
  > exit(n) begin
  >   if n == 0 then begin
  >     return 143;
  >   end;
  >   return 1;
  > end; 
  > 
  > main()begin 
  >   return exit(0); 
  > end;
  > EOF
  $ test_program
  [143]

Test recursive-style factorial
  $ cat > temp_source << EOF
  > extern print_int(n);
  > 
  > fact(n) begin
  >   if n == 1 then begin
  >     return 1;
  >   end;
  >   return (n * fact(n - 1));
  > end;
  > 
  > main()begin 
  >   print_int(fact(5));
  >   return 0;
  > end;
  $ test_program
  120


Test recursive-style fibonacci
  $ cat > temp_source << EOF
  > extern print_int(n);
  > 
  > fib(n) begin
  >   if n == 1 then begin
  >     return 1;
  >   end;
  >   if n == 0 then begin
  >     return 0;
  >   end;
  >   return fib(n-1) + fib(n-2);
  > end;
  > 
  > main() begin
  >   print_int(fib(13));
  >   return 0;
  > end;
  $ test_program
  233

  $ cat > temp_source << EOF
  > extern print_int(integer);
  > 
  > fact(n, x)
  > begin
  >   if n == 0 then
  >   begin
  >     return x;
  >   end;
  >   else
  >   begin
  > 	  return fact(n - 1, n * x);
  >   end;
  > end;
  > 
  > main()
  > begin
  >   print_int(fact(7, 1));
  > 	return 0;
  > end;
  $ test_program
  5040
