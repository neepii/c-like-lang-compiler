This test require RISCV64 toolchain, specifically:
 - riscv64-linux-gnu-as
 - riscv64-linux-gnu-ld

  $ test_program () { \
  > dune exec compiler temp_source object > /dev/null; \
  > riscv64-linux-gnu-as -march=rv64gc object -o main_temp.o && riscv64-linux-gnu-as -march=rv64gc $INSIDE_DUNE/../../asm/lib.s -o lib_temp.o && riscv64-linux-gnu-ld main_temp.o lib_temp.o; \
  > qemu-riscv64 -L /usr/riscv64-linux-gnu -cpu rv64 ./a.out
  > }

  $ cat > temp_source << EOF
  > main() {
  >   return 0;
  > }
  > EOF
  $ test_program

  $ cat > temp_source << EOF
  > main() {
  >   return 143;
  > }
  > EOF
  $ test_program
  [143]

  $ cat > temp_source << EOF
  > main() {
  >   n = 1 + 2 * 3 + 7 / 7; return n; 
  > }
  > EOF
  $ test_program
  [8]

  $ cat > temp_source << EOF
  > main() {
  >   n = 0; if (1 == 1) { n = 1; 
  > } return n; }
  > EOF
  $ test_program
  [1]

  $ cat > temp_source << EOF
  > main() {
  >   n = 0; 
  >   while (n < 143) { 
  >     n = n + 1; 
  >   } 
  >   return n;
  > }
  > EOF
  $ test_program
  [143]

  $ cat > temp_source << EOF
  > extern print_int(n); main() {
  >   n = 1; print_int(n); return 0; 
  > }
  > EOF
  $ test_program
  1

Test simple function from asm library
  $ cat > temp_source << EOF
  > extern print_hw(); 
  > main() {
  >   print_hw(); return 0; 
  > }
  > EOF
  $ test_program
  Hello, World!
Test imperative-style fibonacci
  $ cat > temp_source << EOF
  > extern print_int(n);
  > main() {
  >   a = 0;
  >   b = 1;
  >   n = 9;
  >   while (n > 1) {
  >     b = a + b;
  >     a = b - a;
  >     n = n - 1;
  >   }
  >   print_int(b);
  >   return 0;
  > }
  $ test_program
  34

Test imperative-style factorial
  $ cat > temp_source << EOF
  > extern print_int(n);
  > main() {
  >   acc=1; n=6; 
  >   while (n>1) {
  >     acc=acc*n; n=n-1; 
  >   } 
  >   print_int(acc);
  >   return 0;
  > }
  > EOF
  $ test_program
  720

Test constant function
  $ cat > temp_source << EOF
  > exit() {
  >  return 143 ; 
  > }
  > main(){
  >   var = exit(); return var; 
  > }
  > EOF
  $ test_program
  [143]

Test identity function
  $ cat > temp_source << EOF
  > id(n) {
  >   return n; 
  > }
  > main() {
  >   var = id(142); return var; 
  > }
  > EOF
  $ test_program
  [142]

  $ cat > temp_source << EOF
  > extern print_int(n); 
  > arith(n) {
  >   n = n + 1; n = n + 1; b = n + n; return b;
  > }
  > main () {
  >  var = arith(5);
  >  print_int(var);
  >  return 0; 
  > }
  > EOF
  $ test_program
  14

  $ cat > temp_source << EOF
  > exit(n) {
  >   if (n == 0) {
  >     return 143;
  >   }
  >   return 1;
  > } 
  > 
  > main(){ 
  >   return exit(0); 
  > }
  > EOF
  $ test_program
  [143]

Test recursive-style factorial
  $ cat > temp_source << EOF
  > extern print_int(n);
  > 
  > fact(n) {
  >   if (n == 1) {
  >     return 1;
  >   }
  >   return (n * fact(n - 1));
  > }
  > 
  > main(){ 
  >   print_int(fact(5));
  >   return 0;
  > }
  $ test_program
  120


Test recursive-style fibonacci
  $ cat > temp_source << EOF
  > extern print_int(n);
  > 
  > fib(n) {
  >   if (n == 1) {
  >     return 1;
  >   }
  >   if (n == 0) {
  >     return 0;
  >   }
  >   return fib(n-1) + fib(n-2);
  > }
  > 
  > main(){ 
  >   print_int(fib(13));
  >   return 0;
  > }
  $ test_program
  233

