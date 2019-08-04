#include <stdio.h>
#include <stdbool.h>


void int_printer(int x) {
    printf("%d\n", x);
}

int int_reader() {
    int value;
    puts("Enter integer:");
    scanf("%d", &value);
    return value;
}
