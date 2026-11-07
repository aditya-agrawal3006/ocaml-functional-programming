int sumTo(int limit) {
  int sum;
  sum = 0;
  for (index from 1 to limit) {
    sum = sum + index;
  }
  return sum;
}

int main() {
  int result;
  result = sumTo(5);
  printf(result);
  return result;
}
