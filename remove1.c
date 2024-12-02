int sum(int *a, int n) {
  int i, sum;
  sum = 0;
  for (i = 0; i < n; i = i + 1) {
    sum = sum + a[i];
  }
  return sum;
}

int main(void) {
  int arr[3];
  arr[0] = 1;
  arr[1] = 2;
  arr[2] = 3;
  int result;
  result = sum(arr, 3);
  return result;
}
