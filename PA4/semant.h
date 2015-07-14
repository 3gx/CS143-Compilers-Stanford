#ifndef SEMANT_H_
#define SEMANT_H_

#include <assert.h>
#include <iostream>
#include <map>
#include <vector>
#include <set>
#include "cool-tree.h"
#include "stringtab.h"
#include "symtab.h"
#include "list.h"

#define TRUE 1
#define FALSE 0

class ClassTable;
typedef ClassTable *ClassTableP;

// This is a structure that may be used to contain the semantic
// information such as the inheritance graph.  You may use it or not as
// you like: it is only here to provide a container for the supplied
// methods.

class ClassTable {
 private:
  int semant_errors;
  void install_basic_classes();
  ostream& error_stream;

  std::map<Symbol, Symbol>child_to_parent;
  std::map<Symbol, std::vector<Symbol> >parent_to_children;
  

 public:
  explicit ClassTable(Classes);
  int errors() { return semant_errors; }
  ostream& semant_error();
  ostream& semant_error(Class_ c);
  ostream& semant_error(Symbol filename, tree_node *t);

  void add_pair(Symbol child, Symbol parent);
  bool has_parent(Symbol child);
  Symbol get_parent(Symbol child);
  bool is_cyclic(Symbol start_class);
  std::vector<Symbol> get_path(Symbol class_name);
  bool class_defined(Symbol class_name);
};



struct FormalPair {
  Symbol name, type;
  
  FormalPair(Symbol n, Symbol t) {
    name = n, type = t;
  }
};


struct MethodInfo {
  Symbol return_type;
  Symbol name;
  std::vector<FormalPair>parameters;

  MethodInfo() {}

  explicit MethodInfo(Symbol type) {
    return_type = type;
  }

  MethodInfo(const MethodInfo &m) {
    return_type = m.return_type;
    parameters = m.parameters;
  }

  void add_parameter(const FormalPair &p) {
    parameters.push_back(p);
  }

  int number_of_params() {
    return parameters.size();
  }
};



struct SymTable {
  SymbolTable<Symbol, Symbol> *object_table;
  SymbolTable<Symbol, MethodInfo> *method_table;

  SymTable() {
    object_table = new SymbolTable<Symbol, Symbol>();
    method_table = new SymbolTable<Symbol, MethodInfo>();
  }
};


#endif

