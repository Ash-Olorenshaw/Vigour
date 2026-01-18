#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include "./internal.h"

void raise_err(const char *msg) {
	printf("Runtime error - %s\n", msg);
	exit(1);
}

void vim_print(int count, ...) {
	int arg_i = 0;
	vim_var arg;
	va_list argptr;

	va_start(argptr, count);

	for (arg_i = 0; arg_i < count; arg_i++) {
		arg = va_arg(argptr, vim_var);
		switch (arg.type) {
			case Number:
				printf("%ld ", arg.val.Number);
				break;
			case Float:
				printf("%f ", arg.val.Float);
				break;
			case String:
				printf("%s ", arg.val.String);
				break;
			default:
				printf("unprintable");
				break;
		}
	}

	va_end(argptr);
}

