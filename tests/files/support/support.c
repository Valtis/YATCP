#include <stdio.h>

#ifdef INT_FUNCTION 

int INT_FUNCTION();
#define RUN printf("%d\n", INT_FUNCTION());
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

int returns_12345(void) {
    return 12345;
}
