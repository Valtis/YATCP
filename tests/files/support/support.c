#include <stdio.h>

#ifdef INT_FUNCTION 
int INT_FUNCTION();
#define RUN printf("%d\n", INT_FUNCTION());
#elif defined(BYTE_FUNCTION )
char BYTE_FUNCTION();
#define RUN printf("%d\n", (int)BYTE_FUNCTION());
#elif defined(VOID_FUNCTION)
void VOID_FUNCTION();
#define RUN VOID_FUNCTION();
#elif defined(no_callable)
#define RUN 
#else
#define RUN 
#error Appropriate function type not defined
#endif


#ifndef no_callable
int main() {
    RUN
}
#endif

void c_printer(int a) {
    printf("External C function call: %d\n", a);
}

void int_printer(int a) {
    printf("%d\n", a);
}

void byte_printer(char a) {
    printf("%d\n", (int)a);
}

void boolean_printer(char a) {
    printf("%s\n", a ? "true" : "false");
}

int returns_12345(void) {
    return 12345;
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
