int main() {
  int count;
  count = 0;
  while (count < 3) {
    printf(count);
    count = count + 1;
  }
  if (count == 3) {
    printf(true);
  } else {
    printf(false);
  }
  return count;
}
