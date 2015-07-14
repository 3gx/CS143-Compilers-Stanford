

#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#include "semant.h"
#include "utilities.h"


extern int semant_debug;
extern char *curr_filename;



std::map<Symbol, SymTable> ENVIRONMENT;
std::map<Symbol, Class_> class_map;
ClassTable *classtable;


static ostream& semantic_error(Class_ c);
static ostream& semantic_error(Symbol filename, tree_node *t);


typedef MethodInfo *Method;


static Symbol lookup_object(Symbol name,
                            Class_ c,
                            bool current_scope_only = false,
                            bool exclude_current_scope = false);
static Method lookup_method(Symbol name,
                            Class_ c,
                            bool current_scope_only = false,
                            bool exclude_current_scope = false);

static void enterscope(Symbol class_name);
static void exitscope(Symbol class_name);

static void enter_method_scope(Symbol class_name);
static void exit_method_scope(Symbol class_name);

void add_object(Symbol name, Symbol type, Class_ c);
void add_method(Symbol name, Method method, Class_ c);

static bool class_defined(Symbol class_name);
static bool type_reserved(Symbol type_name);

static bool types_compatible(Symbol child, Symbol parent, Class_ c);

static Symbol find_common_ancestor(Symbol s1, Symbol s2, Class_ c);



bool valid_method_call(const std::vector<Symbol>&types,
                       const Method &method,
                       Class_ c);



void print_path(Symbol class_name) {
  printf("Path for %s!\n", class_name->get_string());
  std::vector<Symbol>path = classtable->get_path(class_name);

  if (!path.empty()) {
    for (int i = 0; i < path.size() - 1; i++) {
      std::cout << path[i]->get_string() << " -> ";
    }
    std::cout << path[path.size()-1] << std::endl;
  }
}

//////////////////////////////////////////////////////////////////////
//
// Symbols
//
// For convenience, a large number of symbols are predefined here.
// These symbols include the primitive type and method names, as well
// as fixed names used by the runtime system.
//
//////////////////////////////////////////////////////////////////////
static Symbol 
    arg,
    arg2,
    Bool,
    concat,
    cool_abort,
    copy,
    Int,
    in_int,
    in_string,
    IO,
    length,
    Main,
    main_meth,
    No_class,
    No_type,
    Object,
    out_int,
    out_string,
    prim_slot,
    self,
    SELF_TYPE,
    Str,
    str_field,
    substr,
    type_name,
    val;
//
// Initializing the predefined symbols.
//
static void initialize_constants(void)
{
    arg         = idtable.add_string("arg");
    arg2        = idtable.add_string("arg2");
    Bool        = idtable.add_string("Bool");
    concat      = idtable.add_string("concat");
    cool_abort  = idtable.add_string("abort");
    copy        = idtable.add_string("copy");
    Int         = idtable.add_string("Int");
    in_int      = idtable.add_string("in_int");
    in_string   = idtable.add_string("in_string");
    IO          = idtable.add_string("IO");
    length      = idtable.add_string("length");
    Main        = idtable.add_string("Main");
    main_meth   = idtable.add_string("main");
    //   _no_class is a symbol that can't be the name of any 
    //   user-defined class.
    No_class    = idtable.add_string("_no_class");
    No_type     = idtable.add_string("_no_type");
    Object      = idtable.add_string("Object");
    out_int     = idtable.add_string("out_int");
    out_string  = idtable.add_string("out_string");
    prim_slot   = idtable.add_string("_prim_slot");
    self        = idtable.add_string("self");
    SELF_TYPE   = idtable.add_string("SELF_TYPE");
    Str         = idtable.add_string("String");
    str_field   = idtable.add_string("_str_field");
    substr      = idtable.add_string("substr");
    type_name   = idtable.add_string("type_name");
    val         = idtable.add_string("_val");
}




ostream& semantic_error(Class_ c) {
  return classtable->semant_error(c);
}


static ostream& semantic_error(Symbol filename, tree_node *t) {
  return classtable->semant_error(filename, t);
}





Symbol lookup_object(Symbol name,
                     Class_ c,
                     bool current_scope_only,
                     bool exclude_current_scope) {
  
  Symbol *prevdef = NULL;
  Symbol c_name = c->get_name();

  if (!exclude_current_scope) {
    prevdef = ENVIRONMENT[c_name].object_table->lookup(name);
  }

  if (current_scope_only) {
    return (prevdef != NULL) ? *prevdef : NULL;
  }
  
  if (prevdef) return *prevdef;

  while ((c_name = classtable->get_parent(c_name))) {
    prevdef = ENVIRONMENT[c_name].object_table->lookup(name);
    if (prevdef) return *prevdef;
  }
  
  return NULL;
}




Method lookup_method(Symbol name,
                     Class_ c,
                     bool current_scope_only,
                     bool exclude_current_scope) {
  Method prevdef = NULL;
  Symbol c_name = c->get_name();

  if (!exclude_current_scope)
    prevdef = ENVIRONMENT[c_name].method_table->lookup(name);

  if (current_scope_only)
    return prevdef;
  
  if (prevdef) return prevdef;

  while ((c_name = classtable->get_parent(c_name))) {
    prevdef = ENVIRONMENT[c_name].method_table->lookup(name);
    if (prevdef) return prevdef;
  }
  
  return NULL;
}



void enterscope(Symbol class_name) {
  ENVIRONMENT[class_name].object_table->enterscope();
}


void exitscope(Symbol class_name) {
  ENVIRONMENT[class_name].object_table->exitscope();
}


void enter_method_scope(Symbol class_name) {
  ENVIRONMENT[class_name].method_table->enterscope();
}


void exit_method_scope(Symbol class_name) {
  ENVIRONMENT[class_name].method_table->exitscope();
}


void add_object(Symbol name, Symbol type, Class_ c) {
  ENVIRONMENT[c->get_name()].object_table->addid(name, new Symbol(type));
}


void add_method(Symbol name, Method method, Class_ c) {
  ENVIRONMENT[c->get_name()].method_table->addid(name, Method(method));
}



bool class_defined(Symbol class_name) {
  return classtable->class_defined(class_name);
}


bool type_reserved(Symbol type_name) {
  int i;
  Symbol reserved[1] = {prim_slot};
  for (i = 0; i < 1; i++)
    if (reserved[i] == type_name)
      return true;
  return false;
}



/* Return true if achild is a subclass of a parent */
bool types_compatible(Symbol child, Symbol parent, Class_ c) {
  
  // No_type is a subclass of any type
  if (child == No_type) return true;

  if (child == SELF_TYPE) {
    if (parent == SELF_TYPE) return true;
    else
      child = c->get_name();
  }
  
  std::vector<Symbol>path = classtable->get_path(child);
  int i;
  for (i = 0; i < path.size(); i++)
    if (path[i] == parent)
      return true;
  return false;
}





bool valid_method_call(const std::vector<Symbol>&types,
                       const Method &method,
                       Class_ c) {
  
  if (types.size() != method->number_of_params()) {
    semantic_error(c) << "Method " << method->name->get_string() <<
        " called with wrong number of arguments.\n";
    return false;
  }

  bool error_occured = false;
  for (int i = 0; i < method->number_of_params(); i++) {
    if (!types_compatible(types[i], method->parameters[i].type, c)) {
      semantic_error(c) << "In call of method " << method->name->get_string() <<
          " , type " << types[i]->get_string() << " of parameter " <<
          method->parameters[i].name->get_string() << " does not conform to " <<
          "declared type " << method->parameters[i].type->get_string() << ".\n";
      error_occured = true;
    }
  }
  
  return !error_occured;
}





void ClassTable::add_pair(Symbol child, Symbol parent) {
  child_to_parent[child] = parent;
  parent_to_children[parent].push_back(child);
}


bool ClassTable::has_parent(Symbol child) {
  return (this->class_defined(child) &&
          (child != Object) &&
          (child != SELF_TYPE));
}


Symbol ClassTable::get_parent(Symbol child) {
  return (has_parent(child)) ? child_to_parent[child] : NULL;
}


std::vector<Symbol> ClassTable::get_path(Symbol c) {
  std::vector<Symbol>result;
  result.push_back(c);
  while ((c = get_parent(c)) != NULL) {
    result.push_back(c);
  }
  return result;
}


bool ClassTable::is_cyclic(Symbol start_class) {
  std::vector<Symbol>stack;
  stack.push_back(start_class);
  std::set<Symbol>visited;

  while (!stack.empty()) {
    Symbol top = stack.back();
    stack.pop_back();
    if (visited.find(top) != visited.end()) return true;
    visited.insert(top);
    Symbol parent = get_parent(top);
    if (parent) stack.push_back(parent);
  }

  return false;
}


bool ClassTable::class_defined(Symbol class_name) {
  return ((child_to_parent.find(class_name) != child_to_parent.end()) ||
          class_name == SELF_TYPE);
}



Symbol find_common_ancestor(Symbol s1, Symbol s2, Class_ c) {
  if (s1 == SELF_TYPE || s2 == SELF_TYPE) {
    if (s1 == SELF_TYPE && s2 == SELF_TYPE) return SELF_TYPE;
    if (s1 == SELF_TYPE) s1 = c->get_name();
    else
      s2 = c->get_name();
  }

  if (s1 == No_type) return s2;
  if (s2 == No_type) return s1;

  std::vector<Symbol>p1, p2;
  p1 = classtable->get_path(s1), p2 = classtable->get_path(s2);

  int i, j;
  if (p1.size() > p2.size()) {
    j = 0, i = p1.size() - p2.size();
    while (j < p2.size()) {
      if (p1[i] == p2[j]) return p1[i];
      i++, j++;
    }
  } else {
    i = 0, j = p2.size() - p1.size();
    while (i < p1.size()) {
      if (p1[i] == p2[j]) return p1[i];
      i++, j++;
    }
  }
  
  return Object;
}





ClassTable::ClassTable(Classes classes) : semant_errors(0),
                                          error_stream(cerr) {
  
  install_basic_classes();

  std::set<Symbol>parents;
  Class_ curr;

  int i;
  for (i = classes->first(); classes->more(i); i = classes->next(i)) {
    curr = classes->nth(i);
    
    if (class_defined(curr->get_name())) {
      semant_error(curr) << "redefinition of class " <<
          curr->get_name()->get_string() << ".\n";
    }

    add_pair(curr->get_name(),
             curr->get_parent());
    class_map[curr->get_name()] = curr;
    parents.insert(curr->get_parent());
  }


  std::set<Symbol>::iterator it;
  for (it = parents.begin(); it != parents.end(); it++) {
    Symbol name = *it;
    
    if (name == Int || name == Bool || name == Str || name == SELF_TYPE) {
      semant_error(curr) << "cannot inherit from basic " <<
          "class " << name->get_string() << ".\n";
    }

    if (!class_defined(name)) {
      semant_error(curr) << "inherit from undefined " <<
          "class " << name->get_string() << ".\n";
    }
  }


  for (i = classes->first(); classes->more(i); i = classes->next(i)) {
    curr = classes->nth(i);

    if (is_cyclic(curr->get_name())) {
      semant_error(curr) << "inherit cycle (" <<
          curr->get_name()->get_string() << ").\n";
    }
  }
}



void ClassTable::install_basic_classes() {

    // The tree package uses these globals to annotate the classes built below.
    // curr_lineno  = 0;
    Symbol filename = stringtable.add_string("<basic class>");
    
    // The following demonstrates how to create dummy parse trees to
    // refer to basic Cool classes.  There's no need for method
    // bodies -- these are already built into the runtime system.
    
    // IMPORTANT: The results of the following expressions are
    // stored in local variables.  You will want to do something
    // with those variables at the end of this method to make this
    // code meaningful.

    // 
    // The Object class has no parent class. Its methods are
    //        abort() : Object    aborts the program
    //        type_name() : Str   returns a string representation of class name
    //        copy() : SELF_TYPE  returns a copy of the object
    //
    // There is no need for method bodies in the basic classes---these
    // are already built in to the runtime system.

    Class_ Object_class =
	class_(Object, 
	       No_class,
	       append_Features(
			       append_Features(
					       single_Features(method(cool_abort, nil_Formals(), Object, no_expr())),
					       single_Features(method(type_name, nil_Formals(), Str, no_expr()))),
			       single_Features(method(copy, nil_Formals(), SELF_TYPE, no_expr()))),
	       filename);

    // 
    // The IO class inherits from Object. Its methods are
    //        out_string(Str) : SELF_TYPE       writes a string to the output
    //        out_int(Int) : SELF_TYPE            "    an int    "  "     "
    //        in_string() : Str                 reads a string from the input
    //        in_int() : Int                      "   an int     "  "     "
    //
    Class_ IO_class = 
	class_(IO, 
	       Object,
	       append_Features(
			       append_Features(
					       append_Features(
							       single_Features(method(out_string, single_Formals(formal(arg, Str)),
										      SELF_TYPE, no_expr())),
							       single_Features(method(out_int, single_Formals(formal(arg, Int)),
										      SELF_TYPE, no_expr()))),
					       single_Features(method(in_string, nil_Formals(), Str, no_expr()))),
			       single_Features(method(in_int, nil_Formals(), Int, no_expr()))),
	       filename);  

    //
    // The Int class has no methods and only a single attribute, the
    // "val" for the integer. 
    //
    Class_ Int_class =
	class_(Int, 
	       Object,
	       single_Features(attr(val, prim_slot, no_expr())),
	       filename);

    //
    // Bool also has only the "val" slot.
    //
    Class_ Bool_class =
	class_(Bool, Object, single_Features(attr(val, prim_slot, no_expr())),filename);

    //
    // The class Str has a number of slots and operations:
    //       val                                  the length of the string
    //       str_field                            the string itself
    //       length() : Int                       returns length of the string
    //       concat(arg: Str) : Str               performs string concatenation
    //       substr(arg: Int, arg2: Int): Str     substring selection
    //       
    Class_ Str_class =
	class_(Str, 
	       Object,
	       append_Features(
			       append_Features(
					       append_Features(
							       append_Features(
									       single_Features(attr(val, Int, no_expr())),
									       single_Features(attr(str_field, prim_slot, no_expr()))),
							       single_Features(method(length, nil_Formals(), Int, no_expr()))),
					       single_Features(method(concat, 
								      single_Formals(formal(arg, Str)),
								      Str, 
								      no_expr()))),
			       single_Features(method(substr, 
						      append_Formals(single_Formals(formal(arg, Int)), 
								     single_Formals(formal(arg2, Int))),
						      Str, 
						      no_expr()))),
	       filename);


    Class_ basic_classes[] = {Object_class,
                              Str_class,
                              Int_class,
                              IO_class,
                              Bool_class};

    for (int i = 0; i < 5; i++) {
      add_pair(basic_classes[i]->get_name(),
               basic_classes[i]->get_parent());
      class_map[basic_classes[i]->get_name()] = basic_classes[i];
    }
}


ostream& ClassTable::semant_error(Class_ c) {
  return semant_error(c->get_filename(), c);
}


ostream& ClassTable::semant_error(Symbol filename, tree_node *t) {
    error_stream << filename << ":" << t->get_line_number() << ": ";
    return semant_error();
}


ostream& ClassTable::semant_error() {
    semant_errors++;
    return error_stream;
}





Symbol assign_class::get_expr_type(Class_ c) {
  if (get_name() == self) {
    semantic_error(c->get_filename(), this) << "Cannot assign to 'self'.\n";
  }
  
  Symbol id_type = lookup_object(get_name(), c);

  // undefined id
  if (!id_type) {
    semantic_error(c->get_filename(), this) << "Assignment to undeclared variable " <<
        get_name()->get_string() << ".\n";
    type = Object;
    return type;
  }

  Symbol e_type = get_expression()->get_expr_type(c);

  if (!types_compatible(e_type, id_type, c)) {
    semantic_error(c->get_filename(), this) << " Inferred type " << e_type->get_string() <<
        " of initialization of " << get_name()->get_string() <<
        " does not conform to declared type " << id_type->get_string() << ".\n";
    type = Object;
    return type;
  }

  type = e_type;
  
  return type;
}






Symbol static_dispatch_class::get_expr_type(Class_ c) {
  Symbol e0_type = get_e0()->get_expr_type(c);
  Expressions en = get_en();
  Symbol method_name = get_method_name();
  std::vector<Symbol>types;
  Method method;
  
  
  if (get_type() == SELF_TYPE) {
    semantic_error(c->get_filename(), this) << "Static dispatch to SELF_TYPE.\n";
    type = Object;
    return type;
  }


  if (!class_defined(get_type())) {
    semantic_error(c->get_filename(), this) << "Static dispatch to undefined class " <<
        get_type()->get_string() << ".\n";
    type = Object;
    return type;
  }


  if (!types_compatible(e0_type, get_type(), c)) {

    semantic_error(c->get_filename(), this) << "Expression type " << e0_type->get_string() <<
        " does not conform to declared static dispatch " <<
        get_type()->get_string() << ".\n";
    type = Object;
    return type;
  }
  

  for (int i = en->first(); en->more(i); i = en->next(i)) {
    types.push_back(en->nth(i)->get_expr_type(c));
  }

  
  method = lookup_method(get_method_name(), class_map[get_type()]);
    
  if (!method) {
    semantic_error(c->get_filename(), this) << "Dispatch to undefined method " <<
        get_method_name()->get_string() << ".\n";
    type = Object;
    return type;
  }

  method->name = method_name;  // required for error display
  // check whether arguments' types conform to parameters' types
  if (valid_method_call(types, method, c)) {
    type = (method->return_type == SELF_TYPE) ? e0_type : method->return_type;
  } else {
    type = Object;
  }

  return type;
}





Symbol dispatch_class::get_expr_type(Class_ c) {
  Expression e0 = get_e0();
  Expressions en = get_en();
  Symbol method_name, e0_type, dispatch_class;
  method_name = get_method_name();
  e0_type = e0->get_expr_type(c);
  std::vector<Symbol>types;  // holds types of argument-expressions
  

  // collect arguments' types
  for (int i = en->first(); en->more(i); i = en->next(i)) {
    types.push_back(en->nth(i)->get_expr_type(c));
  }

  
  dispatch_class = (e0_type == SELF_TYPE) ? c->get_name() : e0_type;
  Method method = lookup_method(method_name, class_map[dispatch_class]);
  
  if (!method) {
    semantic_error(c->get_filename(), this) <<
        "Dispatch to undefined method " <<
        method_name->get_string() << ".\n";
    type = Object;
    return type;
  }
  

  method->name = method_name;  // required for error display
  // check whether arguments' types conform to parameters' types
  if (valid_method_call(types, method, c)) {
    type = (method->return_type == SELF_TYPE) ? e0_type : method->return_type;
  } else {
    type = Object;
  }
  
  
  return type;
}





Symbol cond_class::get_expr_type(Class_ c) {
  Symbol pred, e1, e2;

  pred = get_pred()->get_expr_type(c);
  e1 = get_then()->get_expr_type(c);
  e2 = get_else()->get_expr_type(c);

  // predicate not Bool
  if (pred != Bool) {
    semantic_error(c->get_filename(), this) << "Predicate of 'if' does not have type Bool.\n";
  }

  type = find_common_ancestor(e1, e2, c);
  
  return type;
}





Symbol loop_class::get_expr_type(Class_ c) {
  Symbol pred, body;

  pred = get_pred()->get_expr_type(c);

  if (pred != Bool) {
    semantic_error(c->get_filename(), this) << "Loop condition does not have type Bool.\n";
  }

  get_body()->get_expr_type(c);
  
  type = Object;
  return type;
}





Symbol typcase_class::get_expr_type(Class_ c) {
  Cases cases = get_cases();
  std::set<Symbol>types;  // holds types of case branches
  std::set<Symbol>duplicates;
  int i;
  
  get_expression()->get_expr_type(c);

  
  for (i = cases->first(); cases->more(i); i = cases->next(i)) {
    Case ca = cases->nth(i);
    
    if (duplicates.find(ca->get_type_decl()) != duplicates.end()) {
      semantic_error(c->get_filename(), this) << "Duplicate branch " <<
          ca->get_type_decl()->get_string() << " in case statement.\n";
    }
    duplicates.insert(ca->get_type_decl());
    
    Symbol t = process_case(ca, c);
    types.insert(t);
  }

  type = No_type;
  std::set<Symbol>::iterator it;
  for (it = types.begin(); it != types.end(); it++) {
    type = find_common_ancestor(type, *it, c);
  }
  
  return type;
}





Symbol typcase_class::process_case(Case c, Class_ cl) {
  if (c->get_name() == self) {
    semantic_error(cl) << "'self' bound in 'case'.\n";
  }

  if (c->get_type_decl() == SELF_TYPE) {
    semantic_error(cl) << "Identifier " << c->get_name()->get_string() <<
        " declared with type SELF_TYPE in case branch.\n";
  }

  enterscope(cl->get_name());
  add_object(c->get_name(), c->get_type_decl(), cl);

  Symbol e_type = c->get_expression()->get_expr_type(cl);

  exitscope(cl->get_name());

  return e_type;
}






Symbol block_class::get_expr_type(Class_ c) {
  int i;
  for (i = get_body()->first(); get_body()->more(i); i = get_body()->next(i)) {
    type = get_body()->nth(i)->get_expr_type(c);
  }
  return type;
}






Symbol let_class::get_expr_type(Class_ c) {
  if (get_id() == self) {
    semantic_error(c->get_filename(), this) << "'self' cannot be bound in a 'let' expression.\n";
  }

  
  if (!class_defined(get_type_decl())) {
    semantic_error(c->get_filename(), this) << "Class " << get_type_decl()->get_string() <<
        " of let-bound identifier " << get_id()->get_string() <<
        " is undefined.\n";
  }

  
  Symbol init_type = get_init()->get_expr_type(c);
  
  if (init_type != No_type) {
    
    if (!types_compatible(init_type, get_type_decl(), c)) {
      
      semantic_error(c->get_filename(), this) << "Inferred type " << init_type->get_string() <<
          " of initialization of " << get_id()->get_string() <<
          " does not conform to identifier's declared type " <<
          get_type_decl()->get_string() << ".\n";
    }
  }

  
  // make newly introduced variable visible to the body
  
  enterscope(c->get_name());
  add_object(get_id(),
             get_type_decl(),
             c);

  type = get_body()->get_expr_type(c);

  exitscope(c->get_name());
  
  return type;
}






Symbol plus_class::get_expr_type(Class_ c) {
  Symbol e1, e2;
  type = Int;
  e1 = get_e1()->get_expr_type(c), e2 = get_e2()->get_expr_type(c);

  if (!(e1 == Int && e2 == Int)) {

    semantic_error(c->get_filename(), this) << "Non-Int arguments: " << e1->get_string() <<
        " + " << e2->get_string() << ".\n";
    type = Object;
  }
  
  return type;
}





Symbol sub_class::get_expr_type(Class_ c) {
  Symbol e1, e2;
  type = Int;
  e1 = get_e1()->get_expr_type(c), e2 = get_e2()->get_expr_type(c);

  if (!(e1 == Int && e2 == Int)) {

    semantic_error(c->get_filename(), this) << "Non-Int arguments: " << e1->get_string() <<
        " - " << e2->get_string() << ".\n";
    type = Object;
  }
  
  return type;
}





Symbol mul_class::get_expr_type(Class_ c) {
  Symbol e1, e2;
  type = Int;
  e1 = get_e1()->get_expr_type(c), e2 = get_e2()->get_expr_type(c);

  if (!(e1 == Int && e2 == Int)) {

    semantic_error(c->get_filename(), this) << "Non-Int arguments: " << e1->get_string() <<
        " * " << e2->get_string() << ".\n";
    type = Object;
  }
  
  return type;
}






Symbol divide_class::get_expr_type(Class_ c) {
  Symbol e1, e2;
  e1 = get_e1()->get_expr_type(c), e2 = get_e2()->get_expr_type(c);
  type = Int;

  if (!(e1 == Int && e2 == Int)) {

    semantic_error(c->get_filename(), this) << "Non-Int arguments: " << e1->get_string() <<
        " / " << e2->get_string() << ".\n";
    type = Object;
  }
  
  return type;
}






Symbol neg_class::get_expr_type(Class_ c) {
  type = get_expression()->get_expr_type(c);

  if (type != Int) {
    semantic_error(c->get_filename(), this) << "Non-Int argument: ~" << type->get_string() << ".\n";
    type = Object;
  }
  
  return type;
}






Symbol lt_class::get_expr_type(Class_ c) {
  Symbol e1, e2;
  type = Bool;
  e1 = get_e1()->get_expr_type(c), e2 = get_e2()->get_expr_type(c);

  if (!(e1 == Int && e2 == Int)) {
    
    semantic_error(c->get_filename(), this) << "Non-Int arguments: " << e1->get_string() <<
        " < " << e2->get_string() << ".\n";
    type = Object;
  }
  
  return type;
}






Symbol eq_class::get_expr_type(Class_ c) {
  Symbol e1, e2;
  e1 = get_e1()->get_expr_type(c);
  e2 = get_e2()->get_expr_type(c);

  if (e1 == Int || e1 == Bool || e1 == Str ||
      e2 == Int || e2 == Bool || e2 == Str) {

    if (e1 != e2)
      semantic_error(c->get_filename(), this) << "Illegal comparison with a basic type.\n";
  }
  
  type = Bool;
  return type;
}





Symbol leq_class::get_expr_type(Class_ c) {
  Symbol e1, e2;
  type = Bool;
  e1 = get_e1()->get_expr_type(c), e2 = get_e2()->get_expr_type(c);

  if (!(e1 == Int && e2 == Int)) {
    
    semantic_error(c->get_filename(), this) << "Non-Int arguments: " << e1->get_string() <<
        " <= " << e2->get_string() << ".\n";
    type = Object;
  }
  
  return type;
}






Symbol comp_class::get_expr_type(Class_ c) {
  type = get_expression()->get_expr_type(c);

  if (type != Bool) {
    semantic_error(c->get_filename(), this) << "Argument of 'not' has type " << type->get_string() <<
        " instead of Bool.\n";
    type = Object;
  }
  
  return type;
}





Symbol int_const_class::get_expr_type(Class_ c) {
  type = Int;
  return type;
}






Symbol bool_const_class::get_expr_type(Class_ c) {
  type = Bool;
  return type;
}





Symbol string_const_class::get_expr_type(Class_ c) {
  type = Str;
  return type;
}





Symbol new__class::get_expr_type(Class_ c) {
  type = get_type();

  if (!class_defined(type)) {
    semantic_error(c->get_filename(), this) << "'new' used with undefined class " <<
        type->get_string() << ".\n";
    type = Object;
  }
  
  return type;
}





Symbol isvoid_class::get_expr_type(Class_ c) {
  get_expression()->get_expr_type(c);
  type = Bool;
  return type;
}





Symbol no_expr_class::get_expr_type(Class_ c) {
  type = No_type;
  return type;
}





Symbol object_class::get_expr_type(Class_ c) {
  type = lookup_object(get_name(), c);

  // variable undefined
  if (!type) {
    semantic_error(c->get_filename(), this) << "Undeclared identifier " <<
        get_name()->get_string() << ".\n";
    type = Object;
  }
  
  return type;
}





void method_class::add_feature(Class_ c) {
  Method method = new MethodInfo(get_return_type());
  Formals formals = get_formals();
  std::set<Symbol> names_defined;

  
  // look for same name in the current scope
  Method prevdef = lookup_method(get_method_name(), c, true);
  if (prevdef != NULL) {
    semantic_error(c->get_filename(), this) << "Redefinition of method " <<
                      get_method_name()->get_string() << ".\n";
  }
  

  if (!class_defined(method->return_type)) {
    semantic_error(c->get_filename(), this) << "Return type " << get_return_type()->get_string() <<
        " of method " << get_method_name()->get_string() <<
        " is undefined.\n";
  }

  
  for (int i = formals->first(); formals->more(i); i = formals->next(i)) {
    FormalPair p(formals->nth(i)->get_name(),
                 formals->nth(i)->get_type());

    if (names_defined.find(p.name) != names_defined.end()) {
      semantic_error(c->get_filename(), this) << "Formal parameter " << p.name->get_string() <<
          " is multiply defined.\n";
    }
    names_defined.insert(p.name);

    if (p.name == self) {
      semantic_error(c->get_filename(), this) << "'self' cannot be the name of a formal parameter.\n";
    }

    if (p.type == SELF_TYPE) {
      semantic_error(c->get_filename(), this) << "Formal parameter " << p.name->get_string() <<
          " cannot have type SELF_TYPE.\n";
    }

    
    if (!class_defined(p.type)) {
      semantic_error(c->get_filename(), this) << "Class " << p.type->get_string() <<
          " of formal parameter " << p.name->get_string() <<
          " is undefined.\n";
    }

    method->add_parameter(p);
  }

  add_method(get_method_name(),
             method,
             c);
}





void method_class::check_inheritance_features(Class_ c) {
  
  // if defined in nearest parent, check for validity
  Method prevdef = lookup_method(get_method_name(), c, false, true);
  Method thisclass = lookup_method(get_method_name(), c, true);

  // check for inconsistencies
  if (prevdef) {

    // inconsistent return types
    if (prevdef->return_type != get_return_type()) {
      semantic_error(c->get_filename(), this) << "In redefined method " <<
          get_method_name()->get_string() << ", the return type " <<
          get_return_type()->get_string() << " is inconsistent with the " <<
          " original type " << prevdef->return_type->get_string() << ".\n";
    }

    // inconsistent number of arguments
    if (prevdef->number_of_params() != thisclass->number_of_params()) {
      semantic_error(c->get_filename(), this) << "In redefined method " <<
          get_method_name()->get_string() << ", the number of arguments is " <<
          " inconsistent with the the number of args of the original method.\n";
    } else {
      
      // Compare types of respective parameters
      for (int i = 0; i < thisclass->number_of_params(); i++) {
        if (thisclass->parameters[i].type != prevdef->parameters[i].type)
          semantic_error(c->get_filename(), this) << "In redefined method " <<
              get_method_name()->get_string() << " parameter type " <<
              thisclass->parameters[i].type->get_string() <<
              " is different from the original type " <<
              prevdef->parameters[i].type->get_string() << ".\n";
      }
    }
  }
}





void method_class::set_expression(Class_ c) {
  Method method = lookup_method(get_method_name(), c);

  // create scope for parameters (object scope)
  enterscope(c->get_name());

  // add parameters to the current scope
  for (int i = 0; i < method->number_of_params(); i++) {
    add_object(method->parameters[i].name,
               method->parameters[i].type,
               c);
  }

  Symbol type = get_expression()->get_expr_type(c);
  
  if (!types_compatible(type, method->return_type, c)) {
    
    semantic_error(c->get_filename(), this) << "Inferred return type " <<
        type->get_string() << " of method " <<
        get_method_name()->get_string() << " does not conform to the " <<
        "declared return type " << method->return_type->get_string() << ".\n";
  }

  exitscope(c->get_name());
}





void attr_class::add_feature(Class_ c) {
  // search for in current scope only
  Symbol prevdef = lookup_object(get_attr_name(), c, true);

  if (get_attr_name() == self) {
    semantic_error(c->get_filename(), this) << "'self' cannot be the name of an attribute.\n";
  }

  // if previous definition found, raise error
  if (prevdef) {
    semantic_error(c->get_filename(), this) << "Redefinition of an attribute " <<
        get_attr_name()->get_string() << ".\n";
  }

  // undefined type of an attribute
  if (!class_defined(get_type_decl()) &&
      !type_reserved(get_type_decl())) {
    
    semantic_error(c->get_filename(), this) << "Type " << get_type_decl()->get_string() <<
        " of an attribute " << get_attr_name()->get_string() <<
        " is undefined.\n";
  }
  
  add_object(get_attr_name(), get_type_decl(), c);
}





void attr_class::check_inheritance_features(Class_ c) {
  // NOTE: exclude current scope from search
  Symbol prevdef = lookup_object(get_attr_name(), c, false, true);

  // if previous definition found, raise error
  if (prevdef) {
    semantic_error(c->get_filename(), this) << "Redefinition of an inherited attribute " <<
        get_attr_name()->get_string() << " from ancestor class.\n";
  }
}





void attr_class::set_expression(Class_ c) {
  Symbol type = get_init()->get_expr_type(c);

  // types do not conform
  if (!types_compatible(type, get_type_decl(), c)) {
    semantic_error(c->get_filename(), this) << "Inferred type " << type->get_string() <<
        " of initialization of attribute " << get_attr_name()->get_string() <<
        " does not conform to declared type " <<
        get_type_decl()->get_string() << ".\n";
  }
}





/*   This is the entry point to the semantic checker.

     Your checker should do the following two things:

     1) Check that the program is semantically correct
     2) Decorate the abstract syntax tree with type information
        by setting the `type' field in each Expression node.
        (see `tree.h')

     You are free to first do 1), make sure you catch all semantic
     errors. Part 2) can be done in a second stage, when you want
     to build mycoolc.
 */
void program_class::semant() {
    initialize_constants();

    /* ClassTable constructor may do some semantic analysis */
    classtable = new ClassTable(classes);

    /* Check presence of Main class */
    if (!class_defined(Main)) {
      classtable->semant_error() << "Class Main is not defined.\n";
    }

    if (classtable->errors()) {
      cerr << "Compilation halted due to static semantic errors." << endl;
      exit(1);
    }


    int i, j; Features ftrs; Feature ftr; Class_ curr;

    /* Add attributes and methods to the symbol tables */
    std::map<Symbol, Class_>::iterator it;
    for (it = class_map.begin(); it != class_map.end(); it++) {
      // create symtable and start a new scope
      ENVIRONMENT[it->first] = SymTable();
      enterscope(it->first);
      enter_method_scope(it->first);

      // add implicit 'self'
      add_object(self, SELF_TYPE, it->second);
      
      curr = it->second;
      ftrs = curr->get_features();

      
      for (i = ftrs->first(); ftrs->more(i); i = ftrs->next(i)) {
        ftr = ftrs->nth(i);
        ftr->add_feature(curr);
      }
    }


    /* Check presence of main method in Main class */
    if (class_defined(Main)) {
      Method main_method = lookup_method(main_meth,
                                         class_map[Main],
                                         true);
      
      if (!main_method) {
        semantic_error(class_map[Main]) << "No 'main' method in class Main.\n";
      } else if (main_method->number_of_params() != 0) {
        semantic_error(class_map[Main]) << "'main' method in class Main " <<
            "must have no arguments.\n";
      }
   
    }


    /* Check for inheritance inconsistencies (ie, redefined attributes,
       invalidly inherited methods */
    for (it = class_map.begin(); it != class_map.end(); it++) {
      curr = it->second;
      ftrs = curr->get_features();

      
      for (i = ftrs->first(); ftrs->more(i); i = ftrs->next(i)) {
        ftr = ftrs->nth(i);
        ftr->check_inheritance_features(curr);
      }
    }

    
    /* Do main typechecking */
    for (it = class_map.begin(); it != class_map.end(); it++) {
      curr = it->second;
      ftrs = curr->get_features();

      
      for (i = ftrs->first(); ftrs->more(i); i = ftrs->next(i)) {
        ftr = ftrs->nth(i);
        ftr->set_expression(curr);
      }
    }



    if (classtable->errors()) {
      cerr << "Compilation halted due to static semantic errors." << endl;
      exit(1);
    }
}
