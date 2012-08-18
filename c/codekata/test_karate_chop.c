/*
 * Code Kata Two - Karate Chop
 * http://codekata.pragprog.com/2007/01/kata_two_karate.html
 *
 * Chad Gibbons
 * dcgibbons@gmail.com
 * August 17, 2012
 */

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>

#include <CUnit/CUnit.h>
#include <CUnit/Basic.h>

extern int chop(const int key, const int list[], const int length);
extern int chop2(const int key, const int list[], const int length);

void test_chop_generic(int (*fn)(const int key, const int list[], const int length))
{
    int list[0] = { };
    CU_ASSERT(-1 == fn(3, list, 0));

    int list1[1] = { 1 };
    CU_ASSERT(-1 == fn(3, list1, 1));
    CU_ASSERT(0 == fn(1, list1, 1));

    int list2[3] = { 1, 3, 5 };
    CU_ASSERT(0 == fn(1, list2, 3));
    CU_ASSERT(1 == fn(3, list2, 3));
    CU_ASSERT(2 == fn(5, list2, 3));
    CU_ASSERT(-1 == fn(0, list2, 3));
    CU_ASSERT(-1 == fn(2, list2, 3));
    CU_ASSERT(-1 == fn(4, list2, 3));
    CU_ASSERT(-1 == fn(6, list2, 3));

    int list3[4] = { 1, 3, 5, 7 };
    CU_ASSERT(0 == fn(1, list3, 4));
    CU_ASSERT(1 == fn(3, list3, 4));
    CU_ASSERT(2 == fn(5, list3, 4));
    CU_ASSERT(3 == fn(7, list3, 4));
    CU_ASSERT(-1 == fn(0, list3, 4));
    CU_ASSERT(-1 == fn(2, list3, 4));
    CU_ASSERT(-1 == fn(2, list3, 4));
    CU_ASSERT(-1 == fn(4, list3, 4));
    CU_ASSERT(-1 == fn(6, list3, 4));
    CU_ASSERT(-1 == fn(8, list3, 4));
}

void test_chop(void)
{
    test_chop_generic(chop);
}

void test_chop2(void)
{
    test_chop_generic(chop2);
}

static CU_TestInfo chop_tests[] = {
  { "test_chop1", test_chop },
  { "test_chop2", test_chop2 },
  CU_TEST_INFO_NULL,
};

static CU_SuiteInfo suites[] = 
{
    { "karate_chop_suite", NULL, NULL, chop_tests },
    CU_SUITE_INFO_NULL
};

int main(void)
{
    if (CU_initialize_registry()) 
    {
        fprintf(stderr, "initialization of Test Registry failed.");
        exit(EXIT_FAILURE);
    }

    assert(CU_get_registry() != NULL);
    assert(!CU_is_test_running());

    if (CU_register_suites(suites) != CUE_SUCCESS)
    {
        fprintf(stderr, "test suite registration failed - %s\n", 
            CU_get_error_msg());
        exit(EXIT_FAILURE);
    }

    CU_ErrorCode error_code = CU_basic_run_tests();
    return error_code;
}
