#include <assert.h>
#include <stdio.h>
#include <vector>
#include "emit.h"
#include "cool-tree.h"
#include "symtab.h"

enum Basicness     {Basic, NotBasic};
#define TRUE 1
#define FALSE 0

class CgenClassTable;
typedef CgenClassTable *CgenClassTableP;

class CgenNode;
typedef CgenNode *CgenNodeP;


struct AttrPair {
  Symbol class_name;
  attr_class *attribute;

  AttrPair(Symbol name, attr_class *attr) {
    class_name = name;
    attribute = attr;
  }
};


struct MethodPair {
  Symbol class_name;
  method_class *method;

  MethodPair(Symbol name, method_class *m) {
    class_name = name;
    method = m;
  }

  int num_args() { return method->num_args(); }
};


class CgenClassTable : public SymbolTable<Symbol,CgenNode> {
 private:
  List<CgenNode> *nds;
  ostream& str;
  int stringclasstag;
  int intclasstag;
  int boolclasstag;
  
  
  // The following methods emit code for
  // constants and global declarations.
  
  void code_global_data();
  void code_global_text();
  void code_bools(int);
  void code_select_gc();
  void code_constants();
  
  // The following creates an inheritance graph from
  // a list of classes.  The graph is implemented as
  // a tree of `CgenNode', and class names are placed
  // in the base class symbol table.
  
  void install_basic_classes();
  void install_class(CgenNodeP nd);
  void install_classes(Classes cs);
  void build_inheritance_tree();
  void set_relations(CgenNodeP nd);
 public:
  CgenClassTable(Classes, ostream& str);
  void code();
  CgenNodeP root();

  std::vector<CgenNodeP> collect_all_classes();
  void write_disptabs();
  void write_nametabs();
  void write_objtabs();
  void write_protobjs();

  std::vector<MethodPair> get_user_defined_methods();

  void code_init();
  void methods_code();
};


class CgenNode : public class__class {
 private:
  CgenNodeP parentnd;                        // Parent of class
  List<CgenNode> *children;                  // Children of class
  Basicness basic_status;                    // `Basic' if class is basic
  // `NotBasic' otherwise
  
 public:
  CgenNode(Class_ c,
           Basicness bstatus,
           CgenClassTableP class_table);
  
  void add_child(CgenNodeP child);
  
  List<CgenNode> *get_children() { return children; }

  std::vector<CgenNodeP>get_children_vect();

  std::vector<CgenNodeP>get_inheritance_path();

  
  void set_parentnd(CgenNodeP p);
  
  CgenNodeP get_parentnd() { return parentnd; }
  
  int basic() { return (basic_status == Basic); }

  std::vector<AttrPair> get_all_attributes();
  std::vector<MethodPair> get_all_methods();
  std::vector<MethodPair> get_class_methods();

  int num_of_attrs() { return get_all_attributes().size(); }

  void write_disptab(ostream &s);
  void write_nametab(ostream &s);
  void write_objtab(ostream &s);
  void write_protobj(ostream &s);

  void code_init(ostream &s);

  std::vector<AttrPair> get_attributes();
};

class BoolConst {
 private:
  int val;
 public:
  BoolConst(int);
  void code_def(ostream&, int boolclasstag);
  void code_ref(ostream&) const;
};



class Context {
 private:
  CgenNodeP current_class;
  std::vector<Symbol> params_table;
  std::vector<Symbol> let_vars_table;

 public:
  Context(CgenNodeP c) {
    current_class = c;
  }
  Context() {}
  ~Context() {}

  Symbol current_class_name() {
    return current_class->get_name();
  }

  CgenNodeP get_current_class() {
    return current_class;
  }
  
  /* Return an attribute offset in object layout */
  int lookup_attribute(Symbol attr_name) {
    std::vector<AttrPair>all_attributes = current_class->get_all_attributes();
    int i;
    for (i = 0; i < (int)all_attributes.size(); i++) {
      if (all_attributes[i].attribute->get_name() == attr_name)
        return i;
    }

    return -1;  // not found
  }


  /* Return param offset on stack */
  int lookup_parameter(Symbol param_name) {
    int i;
    for (i = 0; i < (int)params_table.size(); i++) {
      if (params_table[i] == param_name)
        return (int)params_table.size() - 1 - i;
    }

    return -1;
  }


  /* Return let_var offset on stack.
     NOTE: we must search in reverse order. */
  int lookup_let_variable(Symbol let_var_name) {
    int i;
    for (i = (int)let_vars_table.size() - 1; i >= 0; --i) {
      if (let_vars_table[i] == let_var_name) {
	return (int)let_vars_table.size() - 1 - i;
      }
    }
    return -1;
  }


  int add_parameter(Symbol param_name) {
    params_table.push_back(param_name);
    return (int)params_table.size() - 1;  // correct offset
  }


  int add_let_var(Symbol let_var_name) {
    let_vars_table.push_back(let_var_name);
    return (int)let_vars_table.size() - 1;
  }

  int pop_let_var() {
    let_vars_table.pop_back();
    return (int)let_vars_table.size() - 1;
  }


  /* IMPORTANT: keep the right offset for the let vars */
  int add_no_type();
};
