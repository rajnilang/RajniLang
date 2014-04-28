#include "mpc.h"
#include <math.h>

#ifdef _WIN32

static char buffer[2048];

char* readline(char* prompt) {
  fputs(prompt, stdout);
  fgets(buffer, 2048, stdin);
  char* cpy = malloc(strlen(buffer)+1);
  strcpy(cpy, buffer);
  cpy[strlen(cpy)-1] = '\0';
  return cpy;
}

void add_history(char* unused) {}

#else

#include <editline/readline.h>

#endif

mpc_parser_t* Number; 
mpc_parser_t* Symbol; 
mpc_parser_t* String; 
mpc_parser_t* Comment;
mpc_parser_t* Sexpr;  
mpc_parser_t* Qexpr;  
mpc_parser_t* Expr; 
mpc_parser_t* Rajini;

#define RASSERT(args, cond, fmt, ...) \
  if (!(cond)) { \
    rajnival* err = rajnival_err(fmt, ##__VA_ARGS__); \
    rajnival_del(args); \
    return err; \
  }

#define RASSERT_TYPE(func, args, index, expect) \
  RASSERT(args, args->cell[index]->type == expect, \
    "Function '%s' passed incorrect type for argument %i. Got %s, Expected %s.", \
    func, index, rajnitype_name(args->cell[index]->type), rajnitype_name(expect))

#define RASSERT_NUM(func, args, num) \
  RASSERT(args, args->count == num, \
    "Function '%s' passed incorrect number of arguments. Got %i, Expected %i.", \
    func, args->count, num)

struct rajnival;
struct rajnienv;
typedef struct rajnival rajnival;
typedef struct rajnienv rajnienv;

enum { RVAL_ERR, RVAL_NUM, RVAL_SYM, RVAL_STR, RVAL_SEXPR, RVAL_QEXPR, RVAL_FUN};

typedef rajnival*(*rajnibuiltin)(rajnienv*, rajnival*);

typedef struct rajnival {
  int type;

  /* Basic */
  long num;
  char* err;
  char* sym;
  char* str;

  /* Function */
  rajnibuiltin builtin;
  rajnienv* env;
  rajnival* formals;
  rajnival* body;

  /* Expression */
  int count;
  rajnival** cell;
  
};

struct rajnienv {
  rajnienv* par;
  int count;
  char** syms;
  rajnival** vals;
};


/* Create a new number type rajnival */
rajnival* rajnival_num(long x) {
  rajnival* v = malloc(sizeof(rajnival));
  v->type = RVAL_NUM;
  v->num = x;
  return v;
}

rajnival* rajnival_str(char* s) {
  rajnival* v = malloc(sizeof(rajnival));
  v->type = RVAL_STR;
  v->str = malloc(strlen(s) + 1);
  strcpy(v->str, s);
  return v;
}


/* Create a new error type rajnival */
rajnival* rajnival_err(char* fmt, ...) {
  rajnival* v = malloc(sizeof(rajnival));
  v->type = RVAL_ERR;

  /* Create a va list and initialize it */
  va_list va;
  va_start(va, fmt);

  /* Allocate 512 bytes of space */
  v->err = malloc(512);

  /* printf into the error string with a maximum of 511 characters */
  vsnprintf(v->err, 511, fmt, va);

  /* Reallocate to number of bytes actually used */
  v->err = realloc(v->err, strlen(v->err)+1);

  /* Cleanup our va list */
  va_end(va);

  return v;
}

rajnival* rajnival_sym(char* s) {
  rajnival* v = malloc(sizeof(rajnival));
  v->type = RVAL_SYM;
  v->sym = malloc(strlen(s) + 1);
  strcpy(v->sym, s);
  return v;
}

rajnival* rajnival_builtin(rajnibuiltin func) {
  rajnival* v = malloc(sizeof(rajnival));
  v->type = RVAL_FUN;
  v->builtin = func;
  return v;
}

rajnival* rajnival_sexpr(void) {
  rajnival* v = malloc(sizeof(rajnival));
  v->type = RVAL_SEXPR;
  v->count = 0;
  v->cell = NULL;
  return v;
}

rajnival* rajnival_qexpr(void) {
  rajnival* v = malloc(sizeof(rajnival));
  v->type = RVAL_QEXPR;
  v->count = 0;
  v->cell = NULL;
  return v;
}

rajnienv* rajnienv_new(void) {
  rajnienv* e = malloc(sizeof(rajnienv));
  e->par = NULL;
  e->count = 0;
  e->syms = NULL;
  e->vals = NULL;
  return e;
}


rajnival* rajnival_function(rajnival* formals, rajnival* body) {
  rajnival* v = malloc(sizeof(rajnival));
  v->type = RVAL_FUN;

  /* Set Builtin to Null */
  v->builtin = NULL;

  /* Build new environment */
  v->env = rajnienv_new();

  /* Set Formals and Body */
  v->formals = formals;
  v->body = body;
  return v;  
}

void rajnienv_del(rajnienv* e);

void rajnival_del(rajnival* v) {

  switch (v->type) {
    case RVAL_NUM: break;
    case RVAL_ERR: free(v->err); break;
    case RVAL_SYM: free(v->sym); break;
    
    /* If Qexpr or Sexpr then delete all elements inside */
    case RVAL_QEXPR:
    case RVAL_SEXPR:
      for (int i = 0; i < v->count; i++) {
        rajnival_del(v->cell[i]);
      }
      /* Also free the memory allocated to contain the pointers */
      free(v->cell);
    break;
    case RVAL_FUN: 
      if (!v->builtin) {
        rajnienv_del(v->env);
        rajnival_del(v->formals);
        rajnival_del(v->body);
      }
    break;
    case RVAL_STR: free(v->str); break;
  }
  
  free(v);
}

void rajnienv_del(rajnienv* e) {
  for (int i = 0; i < e->count; i++) {
    free(e->syms[i]);
    rajnival_del(e->vals[i]);
  }
  free(e->syms);
  free(e->vals);
  free(e);
}

rajnienv* rajnienv_copy(rajnienv* e);

rajnival* rajnival_copy(rajnival* v) {
  
  rajnival* x = malloc(sizeof(rajnival));
  x->type = v->type;
  
  switch (v->type) {
    
    /* Copy Functions and Numbers Directly */
    case RVAL_FUN: if (v->builtin) {
        x->builtin = v->builtin;
      } else {
        x->builtin = NULL;
        x->env = rajnienv_copy(v->env);
        x->formals = rajnival_copy(v->formals);
        x->body = rajnival_copy(v->body);
      }
      break;
    case RVAL_NUM: x->num = v->num; break;
    
    /* Copy Strings using malloc and strcpy */
    case RVAL_ERR: x->err = malloc(strlen(v->err) + 1); strcpy(x->err, v->err); break;
    case RVAL_SYM: x->sym = malloc(strlen(v->sym) + 1); strcpy(x->sym, v->sym); break;

    /* Copy Lists by copying each sub-expression */
    case RVAL_SEXPR:
    case RVAL_QEXPR:
      x->count = v->count;
      x->cell = malloc(sizeof(rajnival*) * x->count);
      for (int i = 0; i < x->count; i++) {
        x->cell[i] = rajnival_copy(v->cell[i]);
      }
    break;
    case RVAL_STR: x->str = malloc(strlen(v->str) + 1); strcpy(x->str, v->str); break;
  }
  
  return x;
}


rajnival* rajnienv_get(rajnienv* e, rajnival* k) {

  for (int i = 0; i < e->count; i++) {
    if (strcmp(e->syms[i], k->sym) == 0) { return rajnival_copy(e->vals[i]); }
  }

  /* If no symbol check in parent otherwise error */
  if (e->par) {
    return rajnienv_get(e->par, k);
  } else {
    return rajnival_err("Unbound Symbol '%s'", k->sym);
  }
}

void rajnienv_put(rajnienv* e, rajnival* k, rajnival* v) {

  /* Iterate over all items in environment */
  /* This is to see if variable already exists */
  for (int i = 0; i < e->count; i++) {

    /* If variable is found delete item at that position */
    /* And replace with variable supplied by user */
    if (strcmp(e->syms[i], k->sym) == 0) {
      rajnival_del(e->vals[i]);
      e->vals[i] = rajnival_copy(v);
      return;
    }
  }

  /* If no existing entry found then allocate space for new entry */
  e->count++;
  e->vals = realloc(e->vals, sizeof(rajnival*) * e->count);
  e->syms = realloc(e->syms, sizeof(char*) * e->count);

  /* Copy contents of rajnival and symbol string into new location */
  e->vals[e->count-1] = rajnival_copy(v);
  e->syms[e->count-1] = malloc(strlen(k->sym)+1);
  strcpy(e->syms[e->count-1], k->sym);
}

rajnienv* rajnienv_copy(rajnienv* e) {
  rajnienv* n = malloc(sizeof(rajnienv));
  n->par = e->par;
  n->count = e->count;
  n->syms = malloc(sizeof(char*) * n->count);
  n->vals = malloc(sizeof(rajnival*) * n->count);
  for (int i = 0; i < e->count; i++) {
    n->syms[i] = malloc(strlen(e->syms[i]) + 1);
    strcpy(n->syms[i], e->syms[i]);
    n->vals[i] = rajnival_copy(e->vals[i]);
  }
  return n;
}

void rajnienv_def(rajnienv* e, rajnival* k, rajnival* v) {
  /* Iterate till e has no parent */
  while (e->par) { e = e->par; }
  /* Put value in e */
  rajnienv_put(e, k, v);
}

rajnival* rajnival_add(rajnival* v, rajnival* x) {
  v->count++;
  v->cell = realloc(v->cell, sizeof(rajnival*) * v->count);
  v->cell[v->count-1] = x;
  return v;
}

rajnival* rajnival_pop(rajnival* v, int i) {
  /* Find the item at "i" */
  rajnival* x = v->cell[i];
  
  /* Shift the memory following the item at "i" over the top of it */
  memmove(&v->cell[i], &v->cell[i+1], sizeof(rajnival*) * (v->count-i-1));
  
  /* Decrease the count of items in the list */
  v->count--;
  
  /* Reallocate the memory used */
  v->cell = realloc(v->cell, sizeof(rajnival*) * v->count);
  return x;
}

rajnival* rajnival_take(rajnival* v, int i) {
  rajnival* x = rajnival_pop(v, i);
  rajnival_del(v);
  return x;
}

char* rajnitype_name(int t) {
  switch(t) {
    case RVAL_FUN: return "Function";
    case RVAL_NUM: return "Number";
    case RVAL_ERR: return "Error";
    case RVAL_SYM: return "Symbol";
    case RVAL_SEXPR: return "S-Expression";
    case RVAL_QEXPR: return "Q-Expression";
    case RVAL_STR:  return "String";
    default: return "Unknown";
  }
}


rajnival* builtin_var(rajnienv* e, rajnival* a, char* func) {
  RASSERT_TYPE(func, a, 0, RVAL_QEXPR);
  
  rajnival* syms = a->cell[0];
  for (int i = 0; i < syms->count; i++) {
    RASSERT(a, (syms->cell[i]->type == RVAL_SYM),
      "Function '%s' cannot define non-symbol. Got %s, Expected %s.",
      func, rajnitype_name(syms->cell[i]->type), rajnitype_name(RVAL_SYM));
  }
  
  RASSERT(a, (syms->count == a->count-1),
    "Function '%s' passed too many arguments for symbols. Got %i, Expected %i.",
    func, syms->count, a->count-1);
    
  for (int i = 0; i < syms->count; i++) {
    /* If 'def' define in global scope. If 'put' define in local scope */
    if (strcmp(func, "style") == 0) { rajnienv_def(e, syms->cell[i], a->cell[i+1]); }
    if (strcmp(func, "=")   == 0) { rajnienv_put(e, syms->cell[i], a->cell[i+1]); } 
  }
  
  rajnival_del(a);
  return rajnival_sexpr();
}

rajnival* builtin_def(rajnienv* e, rajnival* a) { return builtin_var(e, a, "style"); }
rajnival* builtin_put(rajnienv* e, rajnival* a) { return builtin_var(e, a, "="); }



rajnival* builtin_head(rajnival* env,rajnival* a) {
  RASSERT(a, (a->count == 1                 ), "Function 'head' passed too many arguments!");
  RASSERT(a, (a->cell[0]->type == RVAL_QEXPR), 
  "Function 'head' passed incorrect type for argument 0. Got %s, Expected %s.",
  rajnitype_name(a->cell[0]->type), rajnitype_name(RVAL_QEXPR));
  RASSERT(a, (a->cell[0]->count != 0        ), "Function 'head' passed {}!");

  rajnival* v = rajnival_take(a, 0);  
  while (v->count > 1) { rajnival_del(rajnival_pop(v, 1)); }
  return v;
}

rajnival* builtin_tail(rajnival* env,rajnival* a) {
  RASSERT(a, (a->count == 1                 ), "Function 'tail' passed too many arguments!");
  RASSERT(a, (a->cell[0]->type == RVAL_QEXPR), "Function 'tail' passed incorrect type!");
  RASSERT(a, (a->cell[0]->count != 0        ), "Function 'tail' passed {}!");

  rajnival* v = rajnival_take(a, 0);  
  rajnival_del(rajnival_pop(v, 0));
  return v;
}

rajnival* builtin_list(rajnival* env,rajnival* a) {
  a->type = RVAL_QEXPR;
  return a;
}
rajnival* rajnival_eval(rajnienv* e, rajnival* v);

rajnival* builtin_eval(rajnival* env,rajnival* a) {
  RASSERT(a, (a->count == 1                 ), "Function 'eval' passed too many arguments!");
  RASSERT(a, (a->cell[0]->type == RVAL_QEXPR), "Function 'eval' passed incorrect type!");

  rajnival* x = rajnival_take(a, 0);
  x->type = RVAL_SEXPR;
  return rajnival_eval(env,x);
}

rajnival* rajnival_call(rajnienv* e, rajnival* f, rajnival* a) {

  /* If Builtin then simply apply that */
  if (f->builtin) { return f->builtin(e, a); }

  /* Record Argument Counts */
  int given = a->count;
  int total = f->formals->count;

  /* While arguments still remain to be processed */
  while (a->count) {

    /* If we've ran out of formal arguments to bind */
    if (f->formals->count == 0) {
      rajnival_del(a); return rajnival_err("Function passed too many arguments. Got %i, Expected %i.", given, total); 
    }

    /* Pop the first symbol from the formals */
    rajnival* sym = rajnival_pop(f->formals, 0);

    /* Pop the next argument from the list */
    rajnival* val = rajnival_pop(a, 0);

    /* Bind a copy into the function's environment */
    rajnienv_put(f->env, sym, val);

    /* Delete symbol and value */
    rajnival_del(sym); rajnival_del(val);
  }

  /* Argument list is now bound so can be cleaned up */
  rajnival_del(a);

  /* If all formals have been bound evaluate */
  if (f->formals->count == 0) {

    /* Set Function Environment parent to current evaluation Environment */
    f->env->par = e;

    /* Evaluate and return */
    return builtin_eval(f->env, rajnival_add(rajnival_sexpr(), rajnival_copy(f->body)));
  } else {
    /* Otherwise return partially evaluated function */
    return rajnival_copy(f);
  }

}

rajnival* rajnival_eval_sexpr(rajnienv* e, rajnival* v) {

  for (int i = 0; i < v->count; i++) { v->cell[i] = rajnival_eval(e, v->cell[i]); }
  for (int i = 0; i < v->count; i++) { if (v->cell[i]->type == RVAL_ERR) { return rajnival_take(v, i); } }

  if (v->count == 0) { return v; }  
  if (v->count == 1) { return rajnival_take(v, 0); }

  /* Ensure first element is a function after evaluation */
  rajnival* f = rajnival_pop(v, 0);
  if (f->type != RVAL_FUN) {
    rajnival* err = rajnival_err(
      "S-Expression starts with incorrect type. Got %s, Expected %s.",
      rajnitype_name(f->type), rajnitype_name(RVAL_FUN));
    rajnival_del(f); rajnival_del(v);
    return err;
  }
  /* If so call function to get result */
  rajnival* result = rajnival_call(e, f, v);
  rajnival_del(f);
  return result;
}

rajnival* rajnival_eval(rajnienv* e, rajnival* v) {
  if (v->type == RVAL_SYM) {
    rajnival* x = rajnienv_get(e, v);
    rajnival_del(v);
    return x;
  }
  if (v->type == RVAL_SEXPR) { return rajnival_eval_sexpr(e, v); }
  return v;
}

rajnival* rajnival_join(rajnival* x, rajnival* y) {

  /* For each cell in 'y' add it to 'x' */
  while (y->count) {
    x = rajnival_add(x, rajnival_pop(y, 0));
  }

  /* Delete the empty 'y' and return 'x' */
  rajnival_del(y);  
  return x;
}

rajnival* builtin_join(rajnival* env,rajnival* a) {

  for (int i = 0; i < a->count; i++) {
    RASSERT(a, (a->cell[i]->type == RVAL_QEXPR), "Function 'join' passed incorrect type.");
  }

  rajnival* x = rajnival_pop(a, 0);

  while (a->count) {
    x = rajnival_join(x, rajnival_pop(a, 0));
  }

  rajnival_del(a);
  return x;
}

rajnival* builtin_op(rajnienv* env, rajnival* a, char* op) {
  
  /* Ensure all arguments are numbers */
  for (int i = 0; i < a->count; i++) {
    if (a->cell[i]->type != RVAL_NUM) {
      rajnival_del(a);
      return rajnival_err("Cannot operator on non number!");
    }
  }
  
  /* Pop the first element */
  rajnival* x = rajnival_pop(a, 0);
  
  /* If no arguments and sub then perform unary negation */
  if ((strcmp(op, "-") == 0) && a->count == 0) { x->num = -x->num; }
  
  /* While there are still elements remaining */
  while (a->count > 0) {
  
    /* Pop the next element */
    rajnival* y = rajnival_pop(a, 0);
    
    /* Perform operation */
    if (strcmp(op, "+") == 0) { x->num += y->num; }
    if (strcmp(op, "-") == 0) { x->num -= y->num; }
    if (strcmp(op, "*") == 0) { x->num *= y->num; }
    if (strcmp(op, "/") == 0) {
      if (y->num == 0) {
        rajnival_del(x); rajnival_del(y);
        x = rajnival_err("Division By Zero.");
        break;
      }
      x->num /= y->num;
    }
    if (strcmp(op, "%") == 0) {
      x->num %= y->num;
    }
    if (strcmp(op, "^") == 0) {
      x->num = pow(x->num,y->num);
    }
    if (strcmp(op, "==") == 0) {
      x->num = x->num == y->num;
    }
    if (strcmp(op, ">") == 0) {
      x->num = x->num > y->num;
    }
    if (strcmp(op, ">=") == 0) {
      x->num = x->num >= y->num;
    }
    if (strcmp(op, "<") == 0) {
      x->num = x->num < y->num;
    }
    if (strcmp(op, "<=") == 0) {
      x->num = x->num <= y->num;
    }
    if (strcmp(op, "&&") == 0) {
      x->num = 1;
      if(x->num == 0){
        x->num =0;
      }
      if(y->num == 0){
        x->num = 0;
      }
    }
    if (strcmp(op, "||") == 0) {
      x->num = 0;
      if(x->num == 1){
        x->num =1;
      }
      if(y->num == 1){
        x->num = 1;
      }
    }
    
    /* Delete element now finished with */
    rajnival_del(y);
  }
  
  /* Delete input expression and return result */
  rajnival_del(a);
  return x;
}


rajnival* builtin_if(rajnienv* e, rajnival* a) {
  RASSERT_NUM("yen_vazhi_thani_vazhi", a, 3);
  RASSERT_TYPE("yen_vazhi_thani_vazhi", a, 0, RVAL_NUM);
  RASSERT_TYPE("yen_vazhi_thani_vazhi", a, 1, RVAL_QEXPR);
  
  /* Mark Both Expressions as evaluable */
  rajnival* x;
  a->cell[1]->type = RVAL_SEXPR;
  a->cell[2]->type = RVAL_SEXPR;
  
  if (a->cell[0]->num) {
    /* If condition is true evaluate first expression */
    x = rajnival_eval(e, rajnival_pop(a, 1));
  } else {
    /* Otherwise evaluate second expression */
    x = rajnival_eval(e, rajnival_pop(a, 2));
  }
  
  /* Delete argument list and return */
  rajnival_del(a);
  return x;
}
rajnival* builtin_while(rajnienv* e, rajnival* a) {
  RASSERT_NUM("naa", a, 2);
  RASSERT_TYPE("naa", a, 0, RVAL_QEXPR);
  
  /* Mark Both Expressions as evaluable */
  rajnival* x;
  a->cell[1]->type = RVAL_SEXPR;
  int i=0;
  x = rajnival_eval(e, rajnival_pop(a, 1));
  // for(i;i < a->cell[0]->num; i++) {
  //    If condition is true evaluate first expression 
  //   printf("In for loop");
  //   x = rajnival_eval(e, rajnival_pop(a, 1));
  // } 
  
  /* Delete argument list and return */
  rajnival_del(a);
  return x;
}

rajnival* builtin_add(rajnienv* e, rajnival* a) { return builtin_op(e, a, "+"); }
rajnival* builtin_power(rajnienv* e, rajnival* a) { return builtin_op(e, a, "^"); }
rajnival* builtin_sub(rajnienv* e, rajnival* a) { return builtin_op(e, a, "-"); }
rajnival* builtin_mul(rajnienv* e, rajnival* a) { return builtin_op(e, a, "*"); }
rajnival* builtin_div(rajnienv* e, rajnival* a) { return builtin_op(e, a, "/"); }
rajnival* builtin_equal(rajnienv* e, rajnival* a) { return builtin_op(e, a, "=="); }
rajnival* builtin_greaterthan(rajnienv* e, rajnival* a) { return builtin_op(e, a, ">"); }
rajnival* builtin_greaterthanequal(rajnienv* e, rajnival* a) { return builtin_op(e, a, ">="); }
rajnival* builtin_lessthan(rajnienv* e, rajnival* a) { return builtin_op(e, a, "<"); }
rajnival* builtin_lessthanequal(rajnienv* e, rajnival* a) { return builtin_op(e, a, "<="); }
rajnival* builtin_and(rajnienv* e, rajnival* a) { return builtin_op(e, a, "&&"); }
rajnival* builtin_or(rajnienv* e, rajnival* a) { return builtin_op(e, a, "||"); }

void rajnienv_add_builtin(rajnienv* e, char* name, rajnibuiltin func) {
  rajnival* k = rajnival_sym(name);
  rajnival* v = rajnival_builtin(func);
  rajnienv_put(e, k, v);
  rajnival_del(k); rajnival_del(v);
}


rajnival* builtin_function(rajnienv* e, rajnival* a) {
  /* Check Two arguments, each of which are Q-Expressions */
  RASSERT_NUM("elam_maye", a, 2);
  RASSERT_TYPE("elam_maye", a, 0, RVAL_QEXPR);
  RASSERT_TYPE("elam_maye", a, 1, RVAL_QEXPR);
  
  /* Check first Q-Expression contains only Symbols */
  for (int i = 0; i < a->cell[0]->count; i++) {
    RASSERT(a, (a->cell[0]->cell[i]->type == RVAL_SYM),
      "Cannot define non-symbol. Got %s, Expected %s.",
      rajnitype_name(a->cell[0]->cell[i]->type), rajnitype_name(RVAL_SYM));
  }
  
  /* Pop first two arguments and pass them to rajnival_lambda */
  rajnival* formals = rajnival_pop(a, 0);
  rajnival* body = rajnival_pop(a, 0);
  rajnival_del(a);
  
  return rajnival_function(formals, body);
}

void rajnival_print(rajnival* v);

void rajnival_expr_print(rajnival* v, char open, char close) {
  putchar(open);
  for (int i = 0; i < v->count; i++) {
    
    /* Print Value contained within */
    rajnival_print(v->cell[i]);
    
    /* Don't print trailing space if last element */
    if (i != (v->count-1)) {
      putchar(' ');
    }
  }
  putchar(close);
}

void rajnival_print_str(rajnival* v) {
  /* Make a Copy of the string */
  char* escaped = malloc(strlen(v->str)+1);
  strcpy(escaped, v->str);
  /* Pass it through the escape function */
  escaped = mpcf_escape(escaped);
  /* Print it between " characters */
  printf("\"%s\"", escaped);
  /* free the copied string */
  free(escaped);
}

void rajnival_print(rajnival* v) {
  switch (v->type) {
    case RVAL_NUM:   printf("%li", v->num); break;
    case RVAL_STR:   rajnival_print_str(v); break;
    case RVAL_ERR:   printf("Error: %s", v->err); break;
    case RVAL_SYM:   printf("%s", v->sym); break;
    case RVAL_SEXPR: rajnival_expr_print(v, '(', ')'); break;
    case RVAL_QEXPR: rajnival_expr_print(v, '{', '}'); break;
    case RVAL_FUN:
      if (v->builtin) {
        printf("<builtin>");
      } else {
        printf("(\\ "); rajnival_print(v->formals); putchar(' '); rajnival_print(v->body); putchar(')');
      }
      break;
  }
}

void rajnival_println(rajnival* v) { rajnival_print(v); putchar('\n'); }

rajnival* builtin_print(rajnienv* e, rajnival* a) {

  /* Print each argument followed by a space */
  for (int i = 0; i < a->count; i++) {
    rajnival_print(a->cell[i]); putchar(' ');
  }

  /* Print a newline and delete arguments */
  putchar('\n');
  rajnival_del(a);

  return rajnival_sexpr();
}

rajnival* builtin_printloop(rajnienv* e, rajnival* a) {
  RASSERT_NUM("oru_thadava_sonna_noru_thadava_sona_madiri", a, 1);
  RASSERT_TYPE("oru_thadava_sonna_noru_thadava_sona_madiri", a, 0, RVAL_STR);
  /* Construct Error from first argument */
  for(int i=0;i< 100; ){
    printf("%s \n",a->cell[0]->str); putchar(' ');
    i++;
  }
  return rajnival_sexpr();
}
rajnival* rajnival_read_num(mpc_ast_t* t) {
  errno = 0;
  long x = strtol(t->contents, NULL, 10);
  return errno != ERANGE ? rajnival_num(x) : rajnival_err("invalid number");
}

rajnival* builtin_error(rajnienv* e, rajnival* a) {
  RASSERT_NUM("ena_pathi_uanku_theriyadhu", a, 1);
  RASSERT_TYPE("ena_pathi_uanku_theriyadhu", a, 0, RVAL_STR);

  /* Construct Error from first argument */
  rajnival* err = rajnival_err(a->cell[0]->str);

  /* Delete arguments and return */
  rajnival_del(a);
  return err;
}


rajnival* rajnival_read_str(mpc_ast_t* t) {
  /* Cut off the final quote character */
  t->contents[strlen(t->contents)-1] = '\0';
  /* Copy the string missing out the first quote character */
  char* unescaped = malloc(strlen(t->contents+1)+1);
  strcpy(unescaped, t->contents+1);
  /* Pass through the unescape function */
  unescaped = mpcf_unescape(unescaped);
  /* Construct a new rajnival using the string */
  rajnival* str = rajnival_str(unescaped);
  /* Free the string and return */
  free(unescaped);
  return str;
}

rajnival* rajnival_read(mpc_ast_t* t) {
  
  /* If Symbol or Number return conversion to that type */
  if (strstr(t->tag, "number")) { return rajnival_read_num(t); }
  if (strstr(t->tag, "string")) {  return rajnival_read_str(t); }
  if (strstr(t->tag, "symbol")) { return rajnival_sym(t->contents); }
  
  /* If root (>) or sexpr then create empty list */
  rajnival* x = NULL;
  if (strcmp(t->tag, ">") == 0) { x = rajnival_sexpr(); } 
  if (strstr(t->tag, "sexpr"))  { x = rajnival_sexpr(); }
  if (strstr(t->tag, "qexpr"))  { 
    x = rajnival_qexpr();
  }
  
  /* Fill this list with any valid expression contained within */
  for (int i = 0; i < t->children_num; i++) {
    if (strcmp(t->children[i]->contents, "(") == 0) { continue; }
    if (strcmp(t->children[i]->contents, ")") == 0) { continue; }
    if (strcmp(t->children[i]->contents, "}") == 0) { continue; }
    if (strcmp(t->children[i]->contents, "{") == 0) { continue; }
    if (strcmp(t->children[i]->tag,  "regex") == 0) { continue; }
    if (strstr(t->children[i]->tag, "comment")) { continue; }
    x = rajnival_add(x, rajnival_read(t->children[i]));
  }
  
  return x;
}

rajnival* builtin_load(rajnienv* e, rajnival* a) {
  RASSERT_NUM("load", a, 1);
  RASSERT_TYPE("load", a, 0, RVAL_STR);
  
  /* Parse File given by string name */
  mpc_result_t r;
  printf("The value of the %s ",a->cell[0]->str);
  // if (mpc_parse_contents(a->cell[0]->str, Rajini, &r)) {
  //   printf("Yes \n");
  // }
  // if (mpc_parse_contents(a->cell[0]->str, Rajini, &r)) {
    
  //   /* Read contents */
  //   rajnival* expr = rajnival_read(r.output);
  //   mpc_ast_delete(r.output);

  //   /* Evaluate each Expression */
  //   while (expr->count) {
  //     rajnival* x = rajnival_eval(e, rajnival_pop(expr, 0));
  //     /* If Evaluation leads to error print it */
  //     if (x->type == RVAL_ERR) { rajnival_println(x); }
  //     rajnival_del(x);
  //   }
    
  //    Delete expressions and arguments 
  //   rajnival_del(expr);    
  //   rajnival_del(a);
    
  //   /* Return empty list */
  //   return rajnival_sexpr();
    
  // } else {
  //   /* Get Parse Error as String */
  //   char* err_msg = mpc_err_string(r.error);
  //   mpc_err_delete(r.error);
    
  //   /* Create new error message using it */
  //   rajnival* err = rajnival_err("Could not load Library %s", err_msg);
  //   free(err_msg);
  //   rajnival_del(a);
    
  //   /* Cleanup and return error */
  //   return err;
  // }
}


void rajnienv_add_builtins(rajnienv* e) {  
  /* List Functions */
  rajnienv_add_builtin(e, "list", builtin_list);
  rajnienv_add_builtin(e, "head", builtin_head); rajnienv_add_builtin(e, "tail",  builtin_tail);
  rajnienv_add_builtin(e, "eval", builtin_eval); rajnienv_add_builtin(e, "join",  builtin_join);

  /* Mathematical Functions */
  rajnienv_add_builtin(e, "kooti_kalichu_paru_sariya_varum_+",    builtin_add); rajnienv_add_builtin(e, "kooti_kalichu_paru_sariya_varum_-",     builtin_sub);
  rajnienv_add_builtin(e, "sathigam",    builtin_mul); rajnienv_add_builtin(e, "prachodhagam",     builtin_div);
  rajnienv_add_builtin(e, "sakthikodu",    builtin_power);
  rajnienv_add_builtin(e, "style", builtin_def);
  rajnienv_add_builtin(e, "elam_maye", builtin_function);
  rajnienv_add_builtin(e, "lagalaga",   builtin_put);
  rajnienv_add_builtin(e, "lagalagalagalaga",    builtin_equal);
  rajnienv_add_builtin(e, "adhigama_asai_padra_ambalayum",    builtin_greaterthan);
  rajnienv_add_builtin(e, ">=",    builtin_greaterthanequal);
  rajnienv_add_builtin(e, "kamiya_kovapadadha_pombalayum",    builtin_lessthan);
  rajnienv_add_builtin(e, "<=",    builtin_lessthanequal);
  rajnienv_add_builtin(e, "anbuna_enanu_theriyuma",    builtin_and);
  rajnienv_add_builtin(e, "aarupadaiyappa",    builtin_or);
  rajnienv_add_builtin(e, "yen_vazhi_thani_vazhi",   builtin_if);
  rajnienv_add_builtin(e, "load", builtin_load);
  rajnienv_add_builtin(e, "andavan_solran_arunachalam_mudikran",   builtin_print);
  rajnienv_add_builtin(e, "oru_thadava_sonna_noru_thadava_sona_madiri",  builtin_printloop);
  rajnienv_add_builtin(e, "ena_pathi_uanku_theriyadhu",   builtin_error);
  //rajnienv_add_builtin(e, "naa",   builtin_while);
}




int main(int argc, char** argv) {
  
  /* Create Some Parsers */
  mpc_parser_t* Number   = mpc_new("number");
  mpc_parser_t* Symbol = mpc_new("symbol");
  mpc_parser_t* String   = mpc_new("string");
  mpc_parser_t* Comment   = mpc_new("comment");
  mpc_parser_t* Sexpr  = mpc_new("sexpr");
  mpc_parser_t* Qexpr  = mpc_new("qexpr");
  mpc_parser_t* Expr     = mpc_new("expr");
  mpc_parser_t* Rajini    = mpc_new("rajini");

  
  /* Define them with the following Language */
   mpca_lang(MPC_LANG_DEFAULT,
    "                                          \
      number : /-?[0-9]+/ ;                    \
      symbol : /[a-zA-Z0-9_+\\-*\\/\\\\=<>!&|]+/ ; \
      string : /\"(\\\\.|[^\"])*\"/ ; \
      comment : /sonna sonnadadha[^\\r\\n]*/ ; \
      sexpr  : '(' <expr>* ')' ;               \
      qexpr  : '{' <expr>* '}' ;                         \
      expr   : <number> | <string> | <comment> | <symbol> | <sexpr> | <qexpr> ; \
      rajini  : /^/ <expr>* /$/ ;               \
    ",
    Number, Symbol, String, Comment, Sexpr, Qexpr, Expr, Rajini);
  
  rajnienv* e = rajnienv_new();
  rajnienv_add_builtins(e);

  if (argc >= 2) {
    /* loop over each supplied filename (starting from 1) */
    for (int i = 1; i < argc; i++) {
      // mpc_parse_file
      FILE * input;
      input = fopen (argv[i], "rb");
      mpc_result_t r;
       if ( mpc_parse_file(argv[i], input, Rajini, &r)) {
        
        rajnival* x = rajnival_eval(e, rajnival_read(r.output));
        rajnival_println(x);
        rajnival_del(x);
        
        mpc_ast_delete(r.output);
      } else {    
        mpc_err_print(r.error);
        mpc_err_delete(r.error);
      }

      fclose(input);
      /* Create an argument list with a single argument being the filename */
      // rajnival* args = rajnival_add(rajnival_sexpr(), rajnival_str(argv[i]));

      // /* Pass to builtin load and get the result */
      // rajnival* x = builtin_load(e, args);

      // /* If the result is an error be sure to print it */
      // if (x->type == RVAL_ERR) { rajnival_println(x); }
      // rajnival_del(x);


    }
  }
  else{
    puts("rajini Version 0.0.0.0.1");
    puts("Press Ctrl+c to Exit\n");
  
    while (1) {

      char* input = readline("rajni> ");
      add_history(input);

      mpc_result_t r;
      if (mpc_parse("<stdin>", input, Rajini, &r)) {
        
        rajnival* x = rajnival_eval(e, rajnival_read(r.output));
        rajnival_println(x);
        rajnival_del(x);
        
        mpc_ast_delete(r.output);
      } else {    
        mpc_err_print(r.error);
        mpc_err_delete(r.error);
      }

      free(input);

    }
  }

  rajnienv_del(e);
  
  /* Undefine and delete our parsers */
  mpc_cleanup(8, Number, Symbol, String, Comment, Sexpr, Qexpr, Expr, Rajini);
  
  return 0;
}