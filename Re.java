public class Re {

    private static class TestCase {
        String str;
        String regex;
        boolean expected;
        TestCase(String str, String regex, boolean expected) {
            this.str = str;
            this.regex = regex;
            this.expected = expected;
        }
    }
    
    public static void main(String[] args) {
        TestCase test_cases[] = {
            new TestCase("abc", "abc", true),
            new TestCase("acb", "cde", false),
            new TestCase("abc", "a", true),
            new TestCase("a", "abc", false),
            new TestCase("abc", "", true),
            new TestCase("abc", "^abc", true),
            new TestCase("abc", "^a", true),
            new TestCase("abc", "^b", false),
            new TestCase("abc", "b$", false),
            new TestCase("abc", "c$", true),
            new TestCase("abc", "^b$", false),
            new TestCase("b", "^b$", true),
            new TestCase("", "^$", true),
            new TestCase("abc", "a*bc", true),
            new TestCase("bc", "a*bc", true),
            new TestCase("aaaabc", "a*bc", true),
            new TestCase("aaabbbc", "a*b*c", true),
            new TestCase("aaac", "a*b*c", true),
            new TestCase("", "", true),
            new TestCase("abc", "a.c", true),
            new TestCase("abc", "^a.*$", true)
        };
    
        for (int i = 0; i < test_cases.length; i++) {
            if (match(test_cases[i].str, test_cases[i].regex) != test_cases[i].expected) {
                System.out.println(String.format("Test failure: %s -> %s", test_cases[i].str, test_cases[i].regex));
            }
        }
    }

    private static boolean match(String str, String regex) {
        if (regex.length() == 0) return true;
        if (regex.charAt(0) == '^') return match_here(str, regex.substring(1));
        int i = 0;
        do {
            if (match_here(str.substring(i), regex)) {
                return true;
            }
        } while (++i < str.length());
        return false;
    }
    
    private static boolean match_here(String str, String regex) {
        if (regex.length() == 0) return true;
        if (regex.charAt(0) == '*') return match_star(regex.charAt(0), str, regex.substring(2));
        if (str.length() == 0) return regex.charAt(0) == '$' && regex.length() == 2;
        if (regex.charAt(0) == str.charAt(0) || regex.charAt(0) == '.') return match_here(str.substring(1), regex.substring(1));
        return false;
    }
    
    private static boolean match_star(char c, String str, String regex) {
        int i = 0;
        do {
            if (match_here(str, regex)) {
                return true;
            }
        } while ((str.charAt(i) == c || c == '.') && ++i < str.length());
        return false;
    }

}

