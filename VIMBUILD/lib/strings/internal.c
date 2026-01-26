#include <string.h>
#include "../equality.h"
#include "../internal.h"

char char_to_lower(char c) {
	if (c < 91 && c > 64)
		return c + 32;
	else
		return c;
}

int vim_strcmp(vim_var str1, vim_var str2, vim_case_sensitivity mode) {
	int i;
	int len = strlen(str1.val.String);
	if (mode == CASE_INSENSITIVE)
		for (i = 0; i < len; i++) {
			if (char_to_lower(str1.val.String[i]) != char_to_lower(str2.val.String[i]))
				return 0;
		}
	else
		for (i = 0; i < len; i++) {
			if (str1.val.String[i] != str2.val.String[i])
				return 0;
		}
	return 1;
}
