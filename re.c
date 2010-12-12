#include <stdio.h>
#include <string.h>

#define NUM_TESTS 18
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
    {"aaac", "a*b*c", TRUE}
  };

  int i;
  for (i = 0; i < NUM_TESTS; i++) {
    if (match(test_cases[i].str, test_cases[i].regex) != test_cases[i].expected) {
      printf("Test failure: %s -> %s\n", test_cases[i].str, test_cases[i].regex);
    }
  }
  return 0;
}

int match(char *str, char *regex) {
  if (*regex == '^') return match_here(str, ++regex);
  while (*str != '\0') {
    if (match_here(str++, regex)) {
      return TRUE;
    }
  }
  return FALSE;
}

int match_here(char *str, char *regex) {
  if (*regex == '\0') return TRUE;
  if (*(regex + 1) == '*') return match_star(regex[0], str, regex + 2);
  if (*str == '\0') return *regex == '$' && *++regex == '\0';
  if (*regex == *str) return match_here(++str, ++regex);
  return FALSE;
}

int match_star(int c, char *str, char *regex) {
  do {
    if (match_here(str, regex)) {
      return TRUE;
    }
  } while (*str == c && *str++ != '\0');
  return FALSE;
}
