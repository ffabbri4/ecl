/* -*- mode: c; c-basic-offset: 8 -*- */
/*
    gfun.c -- Dispatch for generic functions.
*/
/*
    Copyright (c) 1990, Giuseppe Attardi.

    ECL is free software; you can redistribute it and/or
    modify it under the terms of the GNU Library General Public
    License as published by the Free Software Foundation; either
    version 2 of the License, or (at your option) any later version.

    See file '../Copyright' for full details.
*/

#include <string.h>
#include <ecl/ecl.h>
#include <ecl/ecl-inl.h>
#include <ecl/internal.h>
#include <ecl/cache.h>

static void
no_applicable_method(cl_env_ptr env, cl_object gfun, cl_object args)
{
	env->values[0] = cl_funcall(3, @'no-applicable-method', gfun, args);
}

static cl_object
fill_spec_vector(cl_object vector, cl_object gfun, cl_object instance)
{
	cl_object *argtype = vector->vector.self.t;
	argtype[0] = gfun;
	argtype[1] = CLASS_OF(instance);
	vector->vector.fillp = 2;
	return vector;
}

static cl_object
slot_method_name(cl_object gfun, cl_object args)
{
	cl_object methods = cl_funcall(3, @'compute-applicable-methods',
				       gfun, args);
	unlikely_if (Null(methods)) {
		return OBJNULL;
	} else {
		cl_object first = ECL_CONS_CAR(methods);
		cl_object slotd = cl_funcall(3, @'slot-value', first,
					     @'clos::slot-definition');
		return cl_funcall(2, @'clos::slot-definition-name', slotd);
	}
}

static cl_object
slot_method_index(cl_object gfun, cl_object instance, cl_object args)
{
	cl_object slot_name = slot_method_name(gfun, args);
	unlikely_if (slot_name == OBJNULL)
		return OBJNULL;
	else {
		cl_object table = cl_funcall(3, @'slot-value',
					     CLASS_OF(instance),
					     @'clos::slot-table');
		cl_object slotd = ecl_gethash_safe(slot_name, table, OBJNULL);
		return cl_funcall(2, @'clos::slot-definition-location', slotd);
	}
}

static ecl_cache_record_ptr
search_slot_index(const cl_env_ptr env, cl_object gfun, cl_object instance)
{
	ecl_cache_ptr cache = env->slot_cache;
	cl_object vector = fill_spec_vector(cache->keys, gfun, instance);
	return ecl_search_cache(cache);
}

static cl_object
add_new_index(const cl_env_ptr env, cl_object gfun, cl_object instance, cl_object args)
{
	/* The keys and the cache may change while we compute the
	 * applicable methods. We must save the keys and recompute the
	 * cache location if it was filled. */
	cl_object index = slot_method_index(gfun, instance, args);
	unlikely_if (index == OBJNULL) {
		no_applicable_method(env, gfun, args);
		return OBJNULL;
	}
	{
		ecl_cache_ptr cache = env->slot_cache;
		cl_object vector = fill_spec_vector(cache->keys, gfun, instance);
		ecl_cache_record_ptr e = ecl_search_cache(cache);
		e->key = cl_copy_seq(cache->keys);
		return e->value = index;
	}
}

cl_object
ecl_slot_reader_dispatch(cl_narg narg, cl_object instance)
{
	const cl_env_ptr env = ecl_process_env();
	cl_object gfun = env->function;
	cl_object index, value;
	ecl_cache_record_ptr e;

	unlikely_if (narg != 1)
		FEwrong_num_arguments(gfun);
	unlikely_if (!ECL_INSTANCEP(instance)) {
		no_applicable_method(env, gfun, ecl_list1(instance));
		return env->values[0];
	}

	e = search_slot_index(env, gfun, instance);
	if (e->key != OBJNULL) {
		index = e->value;
	} else {
		cl_object args = ecl_list1(instance);
		index = add_new_index(env, gfun, instance, args);
		/* no_applicable_method() was called */
		unlikely_if (index == OBJNULL) {
			return env->values[0];
		}
	}
	if (ECL_CONSP(index)) {
		value = ECL_CONS_CAR(index);
	} else unlikely_if (!FIXNUMP(index)) {
		FEerror("Corrupt database ~A", 1, gfun);
	} else {
		cl_fixnum i = fix(index);
		if (i >= instance->instance.length || i < 0)
			FEtype_error_index(instance, i);
		value = instance->instance.slots[i];
	}
	unlikely_if (value == ECL_UNBOUND) {
		cl_object slot_name = slot_method_name(gfun, ecl_list1(instance));
		value = cl_funcall(4, @'slot-unbound',
				   CLASS_OF(instance),
				   instance,
				   slot_name);
	}
	@(return value)
}

cl_object
ecl_slot_writer_dispatch(cl_narg narg, cl_object value, cl_object instance)
{
	const cl_env_ptr env = ecl_process_env();
	cl_object gfun = env->function;
	ecl_cache_record_ptr e;
	cl_object index;

	unlikely_if (narg != 2) {
		FEwrong_num_arguments(gfun);
	}
	unlikely_if (!ECL_INSTANCEP(instance)) {
		no_applicable_method(env, gfun, cl_list(2, value, instance));
		return env->values[0];
	}
	e = search_slot_index(env, gfun, instance);
	if (e->key != OBJNULL) {
		index = e->value;
	} else {
		cl_object args = cl_list(2, value, instance);
		index = add_new_index(env, gfun, instance, args);
		/* no_applicable_method() was called */
		unlikely_if (index == OBJNULL) {
			return env->values[0];
		}
	}
	if (ECL_CONSP(index)) {
		ECL_RPLACA(index, value);
	} else unlikely_if (!FIXNUMP(index)) {
		FEerror("Corrupt database ~A", 1, gfun);
	} else {
		cl_fixnum i = fix(index);
		if (i >= instance->instance.length || i < 0)
			FEtype_error_index(instance, i);
		instance->instance.slots[i] = value;
	}
	@(return value)
}
