#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "./internal.h"

void *arena_reserve(arena *target_arena, int space_size) {
	if (target_arena->size < target_arena->max_size) {
		void *new_space = malloc(space_size);
		if (new_space == NULL) {
			fprintf(stderr, "Err - Failed to allocate space for arena.");
			exit(1);
		}
		memset(new_space, 0, space_size);
		target_arena->arena[target_arena->size] = new_space;
		target_arena->size++;
		return new_space;
	}
	else 
		return NULL;
}

void arena_free(arena *target_arena) {
	for (int i = 0; i < target_arena->size; i++) {
		free(target_arena->arena[i]);
		target_arena->arena[i] = NULL;
	}
	target_arena->size = 0;
}

