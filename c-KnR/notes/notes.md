In C all function arguments are passed by value. (1.8)

### Passing n-D arrays to functions

```
int arr2d[3][2] = {{1, 2}, {3, 4}, {5, 6}};
int ind = maxpair(arr2d, 2, 2);

int maxpair(int* arr, int len, int size) {
    ...
    for (int i = 0; i < len; i++) {
        int a = arr[i*size];
        int b = arr[(i*size)+1];
        ...
    }

    return ...;
}
```
