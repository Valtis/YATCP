#include <stdio.h>
#include <stdbool.h>


void int_printer(int x) {
    printf("%d\n", x);
}

void newline_printer() {
    printf("\n");
}

int int_reader() {
    int value;
    puts("Enter integer:");
    scanf("%d", &value);
    return value;
}


void fizz() {
        printf("Fizz\n");
}

void buzz() {
        printf("Buzz\n");
}

void fizzbuzz() {
        printf("FizzBuzz\n");
}
