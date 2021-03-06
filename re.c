#include <stdio.h>
#include <string.h>

#define NUM_TESTS 22
#define TRUE 1
#define FALSE 0

int match(char*, char*);
int match_here(char*, char*);
int match_star(int, char*, char*);

int main(char **args) {
  typedef struct TEST_CASE {
    char* str;
    char* regex;
    int expected;
  } TEST_CASE; 
  
  TEST_CASE test_cases[NUM_TESTS] = {
    {"abc", "abc", TRUE},
    {"acb", "cde", FALSE},
    {"abc", "a", TRUE},
    {"a", "abc", FALSE},
    {"abc", "", TRUE},
    {"abc", "^abc", TRUE},
    {"abc", "^a", TRUE},
    {"abc", "^b", FALSE},
    {"abc", "b$", FALSE},
    {"abc", "c$", TRUE},
    {"abc", "^b$", FALSE},
    {"b", "^b$", TRUE},
    {"", "^$", TRUE},
    {"abc", "a*bc", TRUE},
    {"bc", "a*bc", TRUE},
    {"aaaabc", "a*bc", TRUE},
    {"aaabbbc", "a*b*c", TRUE},
    {"aaac", "a*b*c", TRUE},
    {"", "", TRUE},
    {"abc", "a.c", TRUE},
    {"abc", "^a.*$", TRUE},
    {"aaa", "a*", TRUE}
  };

  int i;
  int j;
  for (j = 0; j < 10000000; j++) {
    for (i = 0; i < NUM_TESTS; i++) {
      if (match(test_cases[i].str, test_cases[i].regex) != test_cases[i].expected) {
        printf("Test failure: %s -> %s\n", test_cases[i].str, test_cases[i].regex);
      }
    }
  }
  return 0;
}

int match(char *str, char *regex) {
  if (*regex == '^') return match_here(str, regex + 1);
  do {
    if (match_here(str, regex)) {
      return TRUE;
    }
  } while (*str++ != '\0');
  return FALSE;
}

int match_here(char *str, char *regex) {
  if (*regex == '\0') return TRUE;
  if (regex[1] == '*') return match_star(regex[0], str, regex + 2);
  if (*str == '\0') return *regex == '$' && regex[1] == '\0';
  if (*regex == *str || *regex == '.') return match_here(str + 1, regex + 1);
  return FALSE;
}

int match_star(int c, char *str, char *regex) {
  do {
    if (match_here(str, regex)) {
      return TRUE;
    }
  } while ((*str == c || c == '.') && *str++ != '\0');
  return FALSE;
}
