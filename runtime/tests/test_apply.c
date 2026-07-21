#include "coal/apply.h"
#include "coal/closure.h"
#include "coal/runtime.h"
#include "coal/value.h"
#include <stdio.h>
#include <assert.h>

/* Test function: add two numbers */
static void *
add_fn(void **args)
{
    int32_t a = rt_int32_unbox(args[0]);
    int32_t b = rt_int32_unbox(args[1]);
    return rt_int32_box(a + b);
}

/* Test function: add three numbers */
static void *
add3_fn(void **args)
{
    int32_t a = rt_int32_unbox(args[0]);
    int32_t b = rt_int32_unbox(args[1]);
    int32_t c = rt_int32_unbox(args[2]);
    return rt_int32_box(a + b + c);
}

/* Test function that returns a closure (for over-application test) */
static void *
make_adder_fn(void **args)
{
    /* This function takes one arg and returns a closure that adds that value */
    int32_t x = rt_int32_unbox(args[0]);

    /* Create a new closure that will add x to its argument */
    rt_closure_t *closure = rt_closure_new((void *) add_fn, 2);

    /* Partially apply with x */
    void *x_boxed = rt_int32_box(x);
    closure = rt_closure_extend(closure, 1, &x_boxed);

    return (void *) closure;
}

/* Test function: returns constant 42 */
static void *
const42_fn(void **args)
{
    (void) args;
    return rt_int32_box(42);
}

static void
test_exact_application(void)
{
    /* Create a closure for add_fn which takes 2 arguments */
    rt_closure_t *closure = rt_closure_new((void *) add_fn, 2);

    /* Apply exactly 2 arguments */
    void *args[2];
    args[0] = rt_int32_box(10);
    args[1] = rt_int32_box(20);

    void *result = rt_apply((void *) closure, 2, args);
    int32_t value = rt_int32_unbox(result);

    assert(value == 30);
    printf("test_exact_application: PASS (10 + 20 = 30)\n");
}

static void
test_partial_application(void)
{
    /* Create a closure for add3_fn which takes 3 arguments */
    rt_closure_t *closure = rt_closure_new((void *) add3_fn, 3);

    /* Partially apply with 1 argument */
    void *arg1 = rt_int32_box(5);
    void *partial1 = rt_apply((void *) closure, 1, &arg1);

    /* Partially apply with 1 more argument */
    void *arg2 = rt_int32_box(10);
    void *partial2 = rt_apply(partial1, 1, &arg2);

    /* Finally apply the last argument */
    void *arg3 = rt_int32_box(15);
    void *result = rt_apply(partial2, 1, &arg3);

    int32_t value = rt_int32_unbox(result);
    assert(value == 30);
    printf("test_partial_application: PASS (5 + 10 + 15 = 30)\n");
}

static void
test_partial_application_multiple_args(void)
{
    /* Create a closure for add3_fn which takes 3 arguments */
    rt_closure_t *closure = rt_closure_new((void *) add3_fn, 3);

    /* Partially apply with 2 arguments at once */
    void *args[2];
    args[0] = rt_int32_box(100);
    args[1] = rt_int32_box(200);
    void *partial = rt_apply((void *) closure, 2, args);

    /* Apply the final argument */
    void *arg3 = rt_int32_box(300);
    void *result = rt_apply(partial, 1, &arg3);

    int32_t value = rt_int32_unbox(result);
    assert(value == 600);
    printf("test_partial_application_multiple_args: PASS (100 + 200 + 300 = "
           "600)\n");
}

static void
test_over_application(void)
{
    /* Create a closure that returns a closure (curried function) */
    rt_closure_t *closure = rt_closure_new((void *) make_adder_fn, 1);

    /* Over-apply: provide 2 arguments when it only takes 1 */
    /* The first arg creates an adder, the second arg is applied to that adder
     */
    void *args[2];
    args[0] = rt_int32_box(10); /* Creates an "add 10" function */
    args[1] = rt_int32_box(5);  /* Applies 5 to that function */

    void *result = rt_apply((void *) closure, 2, args);
    int32_t value = rt_int32_unbox(result);

    assert(value == 15); /* 10 + 5 = 15 */
    printf("test_over_application: PASS (make_adder(10)(5) = 15)\n");
}

static void
test_zero_arguments_exact(void)
{
    /* Test a function that takes no arguments */
    rt_closure_t *closure = rt_closure_new((void *) const42_fn, 0);

    void *result = rt_apply((void *) closure, 0, NULL);
    int32_t value = rt_int32_unbox(result);

    assert(value == 42);
    printf("test_zero_arguments_exact: PASS (const 42)\n");
}

int
main(void)
{
    rt_runtime_init();

    printf("Running apply tests...\n");
    test_exact_application();
    test_partial_application();
    test_partial_application_multiple_args();
    test_over_application();
    test_zero_arguments_exact();
    printf("All apply tests passed!\n");

    return 0;
}
