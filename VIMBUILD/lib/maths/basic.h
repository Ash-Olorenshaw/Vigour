#ifndef MATHS_BASIC_VIM_H
#define MATHS_BASIC_VIM_H

#include "../internal.h"

vim_var vim_add(vim_var elem1, vim_var elem2);
vim_var vim_sub(vim_var elem1, vim_var elem2);
vim_var vim_mult(vim_var elem1, vim_var elem2);
vim_var vim_div(vim_var elem1, vim_var elem2);

#endif
