/* The GCD algorithm in MicroC */
int a;
int b;

int gcd(int a, int b) {
  while (a != b) {
    if (b < a) a = a - b;
    else b = b - a;
  }
  return a;
}

int main() {
  int x;
  int y;
  a = 18;
  b = 9;
  x = 2;
  y = 14;
  gcd(x,y);
  return 0;
}
