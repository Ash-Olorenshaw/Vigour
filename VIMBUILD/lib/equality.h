#ifndef EQ_VIM_H
#define EQ_VIM_H

#include "internal.h"

typedef enum {
	CASE_SENSITIVE,
	CASE_INSENSITIVE
} vim_case_sensitivity;

vim_var vim_eq(vim_var elem1, vim_var elem2, vim_case_sensitivity mode);
vim_var vim_ne(vim_var elem1, vim_var elem2, vim_case_sensitivity mode);
vim_var vim_lt(vim_var elem1, vim_var elem2, vim_case_sensitivity mode);
vim_var vim_lte(vim_var elem1, vim_var elem2, vim_case_sensitivity mode);
vim_var vim_gt(vim_var elem1, vim_var elem2, vim_case_sensitivity mode);
vim_var vim_gte(vim_var elem1, vim_var elem2, vim_case_sensitivity mode);

#endif
