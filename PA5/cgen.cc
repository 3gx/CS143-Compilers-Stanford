
//**************************************************************
//
// Code generator SKELETON
//
// Read the comments carefully. Make sure to
//    initialize the base class tags in
//       `CgenClassTable::CgenClassTable'
//
//    Add the label for the dispatch tables to
//       `IntEntry::code_def'
//       `StringEntry::code_def'
//       `BoolConst::code_def'
//
//    Add code to emit everyting else that is needed
//       in `CgenClassTable::code'
//
//
// The files as provided will produce code to begin the code
// segments, declare globals, and emit constants.  You must
// fill in the rest.
//
//**************************************************************

#include <set>
#include <map>
#include <string>
#include <queue>
#include <utility>
#include "cgen.h"
#include "cgen_gc.h"

extern void emit_string_constant(ostream& str, char *s);
extern int cgen_debug;


static int labelnum;


static int INTCLASSTAG = 2;
static int BOOLCLASSTAG = 3;
static int STRINGCLASSTAG = 4;


static std::map<Symbol, int> class_name_to_tag;
static std::map<int, Symbol> tag_to_class_name;
static std::map<Symbol, CgenNodeP> symbol_to_class;


static void write_default_value(Symbol type_name, ostream &s);


static bool class_predefined(Symbol class_name);

template<typename T>
static void reverse(std::vector<T>&v) {
  int i, j;
  for (i = 0, j = (int)v.size() - 1; i < j; i++, j--) {
    T temp = v[i];
    v[i] = v[j];
    v[j] = temp;
  }
}

std::string to_string(char *str) {
  return std::string(str);
}


std::string symbol_to_string(Symbol sym) {
  return to_string(sym->get_string());
}



static void binary_op_code(Expression e1,
			   Expression e2,
			   ostream &s,
			   Context c,
			   bool is_comp_op=false);


//
// Three symbols from the semantic analyzer (semant.cc) are used.
// If e : No_type, then no code is generated for e.
// Special code is generated for new SELF_TYPE.
// The name "self" also generates code different from other references.
//
//////////////////////////////////////////////////////////////////////
//
// Symbols
//
// For convenience, a large number of symbols are predefined here.
// These symbols include the primitive type and method names, as well
// as fixed names used by the runtime system.
//
//////////////////////////////////////////////////////////////////////
Symbol
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

static char *gc_init_names[] =
{ "_NoGC_Init", "_GenGC_Init", "_ScnGC_Init" };
static char *gc_collect_names[] =
{ "_NoGC_Collect", "_GenGC_Collect", "_ScnGC_Collect" };



bool class_predefined(Symbol class_name) {
  return (class_name == Object ||
	  class_name == Int ||
	  class_name == Str ||
	  class_name == Bool ||
	  class_name == IO);
}



int Context::add_no_type() {
  let_vars_table.push_back(No_type);
  return (int)let_vars_table.size() - 1;
}




//  BoolConst is a class that implements code generation for operations
//  on the two booleans, which are given global names here.
BoolConst falsebool(FALSE);
BoolConst truebool(TRUE);

//*********************************************************
//
// Define method for code generation
//
// This is the method called by the compiler driver
// `cgtest.cc'. cgen takes an `ostream' to which the assembly will be
// emmitted, and it passes this and the class list of the
// code generator tree to the constructor for `CgenClassTable'.
// That constructor performs all of the work of the code
// generator.
//
//*********************************************************

void program_class::cgen(ostream &os)
{
  // spim wants comments to start with '#'
  os << "# start of generated code\n";
  
  initialize_constants();
  CgenClassTable *codegen_classtable = new CgenClassTable(classes,os);
  
  codegen_classtable->code();
  codegen_classtable->exitscope();
  
  os << "\n# end of generated code\n";
}


//////////////////////////////////////////////////////////////////////////////
//
//  emit_* procedures
//
//  emit_X  writes code for operation "X" to the output stream.
//  There is an emit_X for each opcode X, as well as emit_ functions
//  for generating names according to the naming conventions (see emit.h)
//  and calls to support functions defined in the trap handler.
//
//  Register names and addresses are passed as strings.  See `emit.h'
//  for symbolic names you can use to refer to the strings.
//
//////////////////////////////////////////////////////////////////////////////

static void emit_load(char *dest_reg, int offset, char *source_reg, ostream& s)
{
  s << LW << dest_reg << " " << offset * WORD_SIZE << "(" << source_reg << ")"
  << endl;
}

static void emit_store(char *source_reg, int offset, char *dest_reg, ostream& s)
{
  s << SW << source_reg << " " << offset * WORD_SIZE << "(" << dest_reg << ")"
  << endl;
}

static void emit_load_imm(char *dest_reg, int val, ostream& s)
{ s << LI << dest_reg << " " << val << endl; }

static void emit_load_address(char *dest_reg, char *address, ostream& s)
{ s << LA << dest_reg << " " << address << endl; }

static void emit_partial_load_address(char *dest_reg, ostream& s)
{ s << LA << dest_reg << " "; }

static void emit_load_bool(char *dest, const BoolConst& b, ostream& s)
{
  emit_partial_load_address(dest,s);
  b.code_ref(s);
  s << endl;
}

static void emit_load_string(char *dest, StringEntry *str, ostream& s)
{
  emit_partial_load_address(dest,s);
  str->code_ref(s);
  s << endl;
}

static void emit_load_int(char *dest, IntEntry *i, ostream& s)
{
  emit_partial_load_address(dest,s);
  i->code_ref(s);
  s << endl;
}

static void emit_move(char *dest_reg, char *source_reg, ostream& s)
{ s << MOVE << dest_reg << " " << source_reg << endl; }

static void emit_neg(char *dest, char *src1, ostream& s)
{ s << NEG << dest << " " << src1 << endl; }

static void emit_add(char *dest, char *src1, char *src2, ostream& s)
{ s << ADD << dest << " " << src1 << " " << src2 << endl; }

static void emit_addu(char *dest, char *src1, char *src2, ostream& s)
{ s << ADDU << dest << " " << src1 << " " << src2 << endl; }

static void emit_addiu(char *dest, char *src1, int imm, ostream& s)
{ s << ADDIU << dest << " " << src1 << " " << imm << endl; }

static void emit_div(char *dest, char *src1, char *src2, ostream& s)
{ s << DIV << dest << " " << src1 << " " << src2 << endl; }

static void emit_mul(char *dest, char *src1, char *src2, ostream& s)
{ s << MUL << dest << " " << src1 << " " << src2 << endl; }

static void emit_sub(char *dest, char *src1, char *src2, ostream& s)
{ s << SUB << dest << " " << src1 << " " << src2 << endl; }

static void emit_sll(char *dest, char *src1, int num, ostream& s)
{ s << SLL << dest << " " << src1 << " " << num << endl; }

static void emit_jalr(char *dest, ostream& s)
{ s << JALR << "\t" << dest << endl; }

static void emit_jal(std::string address,ostream &s)
{ s << JAL << address << endl; }

static void emit_return(ostream& s)
{ s << RET << endl; }

static void emit_gc_assign(ostream& s)
{ s << JAL << "_GenGC_Assign" << endl; }

static void emit_disptable_ref(Symbol sym, ostream& s)
{  s << sym << DISPTAB_SUFFIX; }

static void emit_init_ref(Symbol sym, ostream& s)
{ s << sym << CLASSINIT_SUFFIX; }

static void emit_label_ref(int l, ostream &s)
{ s << "label" << l; }

static void emit_protobj_ref(Symbol sym, ostream& s)
{ s << sym << PROTOBJ_SUFFIX; }

static void emit_method_ref(Symbol classname, Symbol methodname, ostream& s)
{ s << classname << METHOD_SEP << methodname; }

static void emit_label_def(int l, ostream &s)
{
  emit_label_ref(l,s);
  s << ":" << endl;
}

static void emit_beqz(char *source, int label, ostream &s)
{
  s << BEQZ << source << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_beq(char *src1, char *src2, int label, ostream &s)
{
  s << BEQ << src1 << " " << src2 << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_bne(char *src1, char *src2, int label, ostream &s)
{
  s << BNE << src1 << " " << src2 << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_bleq(char *src1, char *src2, int label, ostream &s)
{
  s << BLEQ << src1 << " " << src2 << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_blt(char *src1, char *src2, int label, ostream &s)
{
  s << BLT << src1 << " " << src2 << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_blti(char *src1, int imm, int label, ostream &s)
{
  s << BLT << src1 << " " << imm << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_bgti(char *src1, int imm, int label, ostream &s)
{
  s << BGT << src1 << " " << imm << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_branch(int l, ostream& s)
{
  s << BRANCH;
  emit_label_ref(l,s);
  s << endl;
}



//
// Push a register on the stack. The stack grows towards smaller addresses.
//
static void emit_push(char *reg, ostream& str)
{
  emit_store(reg,0,SP,str);
  emit_addiu(SP,SP,-4,str);
}

//
// Fetch the integer value in an Int object.
// Emits code to fetch the integer value of the Integer object pointed
// to by register source into the register dest
//
static void emit_fetch_int(char *dest, char *source, ostream& s)
{ emit_load(dest, DEFAULT_OBJFIELDS, source, s); }

//
// Emits code to store the integer value contained in register source
// into the Integer object pointed to by dest.
//
static void emit_store_int(char *source, char *dest, ostream& s)
{ emit_store(source, DEFAULT_OBJFIELDS, dest, s); }


static void emit_test_collector(ostream &s)
{
  emit_push(ACC, s);
  emit_move(ACC, SP, s); // stack end
  emit_move(A1, ZERO, s); // allocate nothing
  s << JAL << gc_collect_names[cgen_Memmgr] << endl;
  emit_addiu(SP,SP,4,s);
  emit_load(ACC,0,SP,s);
}

static void emit_gc_check(char *source, ostream &s)
{
  if (source != (char*)A1) emit_move(A1, source, s);
  s << JAL << "_gc_check" << endl;
}



void write_default_value(Symbol type_name, ostream &s) {
  if (type_name == Int) {
    inttable.lookup_string("0")->code_ref(s);
  } else if (type_name == Str) {
    stringtable.lookup_string("")->code_ref(s);
  } else if (type_name == Bool) {
    s << "bool_const0";
  } else {
    s << "0";
  }
}


///////////////////////////////////////////////////////////////////////////////
//
// coding strings, ints, and booleans
//
// Cool has three kinds of constants: strings, ints, and booleans.
// This section defines code generation for each type.
//
// All string constants are listed in the global "stringtable" and have
// type StringEntry.  StringEntry methods are defined both for String
// constant definitions and references.
//
// All integer constants are listed in the global "inttable" and have
// type IntEntry.  IntEntry methods are defined for Int
// constant definitions and references.
//
// Since there are only two Bool values, there is no need for a table.
// The two booleans are represented by instances of the class BoolConst,
// which defines the definition and reference methods for Bools.
//
///////////////////////////////////////////////////////////////////////////////

//
// Strings
//
void StringEntry::code_ref(ostream& s)
{
  s << STRCONST_PREFIX << index;
}

//
// Emit code for a constant String.
// You should fill in the code naming the dispatch table.
//

void StringEntry::code_def(ostream& s, int stringclasstag)
{
  IntEntryP lensym = inttable.add_int(len);
  
  // Add -1 eye catcher
  s << WORD << "-1" << endl;
  
  code_ref(s);  s  << LABEL                                             // label
  << WORD << stringclasstag << endl                                 // tag
  << WORD << (DEFAULT_OBJFIELDS + STRING_SLOTS + (len+4)/4) << endl // size
  << WORD;
  
  
  /***** Add dispatch information for class String ******/
  s << Str << DISPTAB_SUFFIX;
  
  s << endl;                                              // dispatch table
  s << WORD;  lensym->code_ref(s);  s << endl;            // string length
  emit_string_constant(s,str);                                // ascii string
  s << ALIGN;                                                 // align to word
}

//
// StrTable::code_string
// Generate a string object definition for every string constant in the
// stringtable.
//
void StrTable::code_string_table(ostream& s, int stringclasstag)
{
  for (List<StringEntry> *l = tbl; l; l = l->tl())
    l->hd()->code_def(s,stringclasstag);
}

//
// Ints
//
void IntEntry::code_ref(ostream &s)
{
  s << INTCONST_PREFIX << index;
}

//
// Emit code for a constant Integer.
// You should fill in the code naming the dispatch table.
//

void IntEntry::code_def(ostream &s, int intclasstag)
{
  // Add -1 eye catcher
  s << WORD << "-1" << endl;
  
  code_ref(s);  s << LABEL                                // label
  << WORD << intclasstag << endl                      // class tag
  << WORD << (DEFAULT_OBJFIELDS + INT_SLOTS) << endl  // object size
  << WORD;
  
  /***** Add dispatch information for class Int ******/
  s << Int << DISPTAB_SUFFIX;
  
  s << endl;                                          // dispatch table
  s << WORD << str << endl;                           // integer value
}


//
// IntTable::code_string_table
// Generate an Int object definition for every Int constant in the
// inttable.
//
void IntTable::code_string_table(ostream &s, int intclasstag)
{
  for (List<IntEntry> *l = tbl; l; l = l->tl())
    l->hd()->code_def(s,intclasstag);
}


//
// Bools
//
BoolConst::BoolConst(int i) : val(i) { assert(i == 0 || i == 1); }

void BoolConst::code_ref(ostream& s) const
{
  s << BOOLCONST_PREFIX << val;
}

//
// Emit code for a constant Bool.
// You should fill in the code naming the dispatch table.
//

void BoolConst::code_def(ostream& s, int boolclasstag)
{
  // Add -1 eye catcher
  s << WORD << "-1" << endl;
  
  code_ref(s);  s << LABEL                                  // label
  << WORD << boolclasstag << endl                       // class tag
  << WORD << (DEFAULT_OBJFIELDS + BOOL_SLOTS) << endl   // object size
  << WORD;
  
  /***** Add dispatch information for class Bool ******/
  s << Bool << DISPTAB_SUFFIX;
  
  s << endl;                                            // dispatch table
  s << WORD << val << endl;                             // value (0 or 1)
}

//////////////////////////////////////////////////////////////////////////////
//
//  CgenClassTable methods
//
//////////////////////////////////////////////////////////////////////////////

//***************************************************
//
//  Emit code to start the .data segment and to
//  declare the global names.
//
//***************************************************

void CgenClassTable::code_global_data()
{
  Symbol main    = idtable.lookup_string(MAINNAME);
  Symbol string  = idtable.lookup_string(STRINGNAME);
  Symbol integer = idtable.lookup_string(INTNAME);
  Symbol boolc   = idtable.lookup_string(BOOLNAME);
  
  str << "\t.data\n" << ALIGN;
  //
  // The following global names must be defined first.
  //
  str << GLOBAL << CLASSNAMETAB << endl;
  str << GLOBAL; emit_protobj_ref(main,str);    str << endl;
  str << GLOBAL; emit_protobj_ref(integer,str); str << endl;
  str << GLOBAL; emit_protobj_ref(string,str);  str << endl;
  str << GLOBAL; falsebool.code_ref(str);  str << endl;
  str << GLOBAL; truebool.code_ref(str);   str << endl;
  str << GLOBAL << INTTAG << endl;
  str << GLOBAL << BOOLTAG << endl;
  str << GLOBAL << STRINGTAG << endl;
  
  //
  // We also need to know the tag of the Int, String, and Bool classes
  // during code generation.
  //
  str << INTTAG << LABEL
  << WORD << intclasstag << endl;
  str << BOOLTAG << LABEL
  << WORD << boolclasstag << endl;
  str << STRINGTAG << LABEL
  << WORD << stringclasstag << endl;
}


//***************************************************
//
//  Emit code to start the .text segment and to
//  declare the global names.
//
//***************************************************

void CgenClassTable::code_global_text()
{
  str << GLOBAL << HEAP_START << endl
  << HEAP_START << LABEL
  << WORD << 0 << endl
  << "\t.text" << endl
  << GLOBAL;
  emit_init_ref(idtable.add_string("Main"), str);
  str << endl << GLOBAL;
  emit_init_ref(idtable.add_string("Int"),str);
  str << endl << GLOBAL;
  emit_init_ref(idtable.add_string("String"),str);
  str << endl << GLOBAL;
  emit_init_ref(idtable.add_string("Bool"),str);
  str << endl << GLOBAL;
  emit_method_ref(idtable.add_string("Main"), idtable.add_string("main"), str);
  str << endl;
}

void CgenClassTable::code_bools(int boolclasstag)
{
  falsebool.code_def(str,boolclasstag);
  truebool.code_def(str,boolclasstag);
}

void CgenClassTable::code_select_gc()
{
  //
  // Generate GC choice constants (pointers to GC functions)
  //
  str << GLOBAL << "_MemMgr_INITIALIZER" << endl;
  str << "_MemMgr_INITIALIZER:" << endl;
  str << WORD << gc_init_names[cgen_Memmgr] << endl;
  str << GLOBAL << "_MemMgr_COLLECTOR" << endl;
  str << "_MemMgr_COLLECTOR:" << endl;
  str << WORD << gc_collect_names[cgen_Memmgr] << endl;
  str << GLOBAL << "_MemMgr_TEST" << endl;
  str << "_MemMgr_TEST:" << endl;
  str << WORD << (cgen_Memmgr_Test == GC_TEST) << endl;
}


//********************************************************
//
// Emit code to reserve space for and initialize all of
// the constants.  Class names should have been added to
// the string table (in the supplied code, is is done
// during the construction of the inheritance graph), and
// code for emitting string constants as a side effect adds
// the string's length to the integer table.  The constants
// are emmitted by running through the stringtable and inttable
// and producing code for each entry.
//
//********************************************************

void CgenClassTable::code_constants()
{
  //
  // Add constants that are required by the code generator.
  //
  stringtable.add_string("");
  inttable.add_string("0");
  
  stringtable.code_string_table(str,stringclasstag);
  inttable.code_string_table(str,intclasstag);
  code_bools(boolclasstag);
}




CgenClassTable::CgenClassTable(Classes classes, ostream& s) : nds(NULL) , str(s) {
  stringclasstag = 4 /* Change to your String class tag here */;
  intclasstag =    2 /* Change to your Int class tag here */;
  boolclasstag =   3 /* Change to your Bool class tag here */;
  
  enterscope();
  if (cgen_debug) cout << "Building CgenClassTable" << endl;
  install_basic_classes();
  install_classes(classes);
  build_inheritance_tree();

  
  // build tag-class, class-tag maps
  std::vector<CgenNodeP>all_classes = collect_all_classes();
  int tag;
  for (tag = 0; tag < (int)all_classes.size(); tag++) {
    symbol_to_class[all_classes[tag]->get_name()] = all_classes[tag];
    tag_to_class_name[tag] = all_classes[tag]->get_name();
    class_name_to_tag[all_classes[tag]->get_name()] = tag;
  }
}



void CgenClassTable::install_basic_classes() {
  
  // The tree package uses these globals to annotate the classes built below.
  //curr_lineno  = 0;
  Symbol filename = stringtable.add_string("<basic class>");
  
  //
  // A few special class names are installed in the lookup table but not
  // the class list.  Thus, these classes exist, but are not part of the
  // inheritance hierarchy.
  // No_class serves as the parent of Object and the other special classes.
  // SELF_TYPE is the self class; it cannot be redefined or inherited.
  // prim_slot is a class known to the code generator.
  //
  addid(No_class,
        new CgenNode(class_(No_class,No_class,nil_Features(),filename),
                     Basic,this));
  addid(SELF_TYPE,
        new CgenNode(class_(SELF_TYPE,No_class,nil_Features(),filename),
                     Basic,this));
  addid(prim_slot,
        new CgenNode(class_(prim_slot,No_class,nil_Features(),filename),
                     Basic,this));
  
  //
  // The Object class has no parent class. Its methods are
  //        cool_abort() : Object    aborts the program
  //        type_name() : Str        returns a string representation of class name
  //        copy() : SELF_TYPE       returns a copy of the object
  //
  // There is no need for method bodies in the basic classes---these
  // are already built in to the runtime system.
  //
  install_class(
                new CgenNode(
                             class_(Object,
                                    No_class,
                                    append_Features(
                                                    append_Features(
                                                                    single_Features(method(cool_abort, nil_Formals(), Object, no_expr())),
                                                                    single_Features(method(type_name, nil_Formals(), Str, no_expr()))),
                                                    single_Features(method(copy, nil_Formals(), SELF_TYPE, no_expr()))),
                                    filename),
                             Basic,this));
  
  //
  // The IO class inherits from Object. Its methods are
  //        out_string(Str) : SELF_TYPE          writes a string to the output
  //        out_int(Int) : SELF_TYPE               "    an int    "  "     "
  //        in_string() : Str                    reads a string from the input
  //        in_int() : Int                         "   an int     "  "     "
  //
  install_class(
                new CgenNode(
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
                                    filename),
                             Basic,this));
  
  //
  // The Int class has no methods and only a single attribute, the
  // "val" for the integer.
  //
  install_class(
                new CgenNode(
                             class_(Int,
                                    Object,
                                    single_Features(attr(val, prim_slot, no_expr())),
                                    filename),
                             Basic,this));
  
  //
  // Bool also has only the "val" slot.
  //
  install_class(
                new CgenNode(
                             class_(Bool, Object, single_Features(attr(val, prim_slot, no_expr())),filename),
                             Basic,this));
  
  //
  // The class Str has a number of slots and operations:
  //       val                                  ???
  //       str_field                            the string itself
  //       length() : Int                       length of the string
  //       concat(arg: Str) : Str               string concatenation
  //       substr(arg: Int, arg2: Int): Str     substring
  //
  install_class(
                new CgenNode(
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
                                    filename),
                             Basic,this));
  
}

// CgenClassTable::install_class
// CgenClassTable::install_classes
//
// install_classes enters a list of classes in the symbol table.
//
void CgenClassTable::install_class(CgenNodeP nd)
{
  Symbol name = nd->get_name();
  
  if (probe(name))
  {
    return;
  }
  
  // The class name is legal, so add it to the list of classes
  // and the symbol table.
  nds = new List<CgenNode>(nd,nds);
  addid(name,nd);
}

void CgenClassTable::install_classes(Classes cs)
{
  for(int i = cs->first(); cs->more(i); i = cs->next(i))
    install_class(new CgenNode(cs->nth(i),NotBasic,this));
}

//
// CgenClassTable::build_inheritance_tree
//
void CgenClassTable::build_inheritance_tree()
{
  for(List<CgenNode> *l = nds; l; l = l->tl())
    set_relations(l->hd());
}

//
// CgenClassTable::set_relations
//
// Takes a CgenNode and locates its, and its parent's, inheritance nodes
// via the class table.  Parent and child pointers are added as appropriate.
//
void CgenClassTable::set_relations(CgenNodeP nd)
{
  CgenNode *parent_node = probe(nd->get_parent());
  nd->set_parentnd(parent_node);
  parent_node->add_child(nd);
}

void CgenNode::add_child(CgenNodeP n)
{
  children = new List<CgenNode>(n,children);
}

void CgenNode::set_parentnd(CgenNodeP p) {
  assert(parentnd == NULL);
  assert(p != NULL);
  parentnd = p;
}



std::vector<CgenNodeP> CgenNode::get_children_vect() {
  std::vector<CgenNodeP>res;
  List<CgenNode> *ch = get_children();
  while (ch) {
    res.push_back(ch->hd());
    ch = ch->tl();
  }
  return res;
}



std::vector<CgenNodeP> CgenNode::get_inheritance_path() {
  CgenNodeP curr = this;
  std::vector<CgenNodeP> path;
  path.push_back(curr);

  while ((curr = curr->get_parentnd())) {
    path.push_back(curr);
  }

  path.pop_back();  // pop "no_class" class
  
  return path;
}


std::vector<AttrPair> CgenNode::get_all_attributes() {
  std::vector<AttrPair> attrs;
  std::vector<CgenNodeP> path = get_inheritance_path();
  std::vector<CgenNodeP>::reverse_iterator it;
  Features features;
  Feature feature;
  int i;

  for (it = path.rbegin(); it != path.rend(); ++it) {
    features = (*it)->features;
    for (i = features->first(); features->more(i); i = features->next(i)) {
      feature = features->nth(i);
      if (!feature->is_method())
        attrs.push_back(AttrPair((*it)->get_name(),
                                 (attr_class *)feature));
    }
  }
  
  return attrs;
}



std::vector<AttrPair> CgenNode::get_attributes() {
  std::vector<AttrPair>attrs;
  Feature feature;
  int i;

  for (i = features->first(); features->more(i); i = features->next(i)) {
    feature = features->nth(i);
    if (!feature->is_method()) {
      attrs.push_back(AttrPair(get_name(),
                               (attr_class *)feature));
    }
  }

  return attrs;
}



std::vector<MethodPair> CgenNode::get_all_methods() {
  std::vector<MethodPair> methods;
  std::vector<CgenNodeP> path = get_inheritance_path();
  reverse(path);
  Features features; Feature feature;
  int i, j, k;
  bool replaced;

  for (k = 0; k < (int)path.size(); k++) {
    CgenNodeP class_node = path[k];
    features = class_node->features;

    for (i = features->first(); features->more(i); i = features->next(i)) {
      feature = features->nth(i);
      if (!feature->is_method()) continue;
      MethodPair m(class_node->get_name(),
                   (method_class *)feature);

      for (replaced = false, j = 0; j < (int)methods.size(); j++) {
        if (methods[j].method->get_name() == feature->get_name()) {
          methods[j] = m;
          replaced = true;
          break;
        }
      }

      if (!replaced) methods.push_back(m);
    }
  }

  return methods;
}



std::vector<MethodPair> CgenNode::get_class_methods() {
  std::vector<MethodPair> result;
  std::vector<MethodPair> all_methods = get_all_methods();
  int i;
  for (i = 0; i < (int)all_methods.size(); i++) {
    MethodPair p = all_methods[i];
    if (p.class_name == get_name())
      result.push_back(p);
  }

  return result;
}


void CgenNode::write_disptab(ostream& s) {
  std::vector<MethodPair> all_methods = get_all_methods();
  int i;
  s << get_name()->get_string() << DISPTAB_SUFFIX << LABEL;

  for (i = 0; i < (int)all_methods.size(); i++) {
    MethodPair p = all_methods[i];
    s << WORD << p.class_name->get_string() << METHOD_SEP <<
        p.method->get_name()->get_string() << "\n";
  }
}


void CgenNode::write_nametab(ostream& s) {
  s << WORD;
  stringtable.lookup_string(get_name()->get_string())->code_ref(s);
}


void CgenNode::write_objtab(ostream& s) {
  s << WORD << get_name()->get_string() << PROTOBJ_SUFFIX << "\n" <<
      WORD << get_name()->get_string() << CLASSINIT_SUFFIX;
}


void CgenNode::write_protobj(ostream& s) {
  s << WORD << -1 << "\n" << get_name()->get_string() << PROTOBJ_SUFFIX <<
      LABEL << WORD << class_name_to_tag[get_name()] << "\n" << WORD <<
      (DEFAULT_OBJFIELDS + num_of_attrs()) << "\n" << WORD <<
      get_name()->get_string() << DISPTAB_SUFFIX << "\n";

  std::vector<AttrPair>all_attributes = get_all_attributes();
  int i;

  for (i = 0; i < (int)all_attributes.size(); i++) {
    AttrPair p = all_attributes[i];
    s << WORD;
    write_default_value(p.attribute->type_decl, s);
    s << "\n";
  }
}



void CgenNode::code_init(ostream& s) {
  std::vector<AttrPair> all_attributes = get_all_attributes();
  int all_attr_num = (int)all_attributes.size();
  int attr_num = (int)get_attributes().size();
  int offset = all_attr_num - attr_num;

  Expression init;

  s << get_name()->get_string() << CLASSINIT_SUFFIX << LABEL;

  emit_addiu(SP, SP, -12, s);
  emit_store(FP, 3, SP, s);
  emit_store(SELF, 2, SP, s);
  emit_store(RA, 1, SP, s);
  emit_addiu(FP, SP, 4, s);
  emit_move(SELF, ACC, s);

  if (get_name() != Object) {
    std::string parent_init(get_parentnd()->get_name()->get_string());
    parent_init += CLASSINIT_SUFFIX;
    emit_jal(parent_init, s);
  }

  for (; offset < all_attr_num; offset++) {
    attr_class *attribute = all_attributes[offset].attribute;
    init = attribute->init;

    if (init->is_empty()) {
      Symbol type_decl = attribute->type_decl;
      if (type_decl == Int || type_decl == Str || type_decl == Bool) {
	emit_partial_load_address(ACC, s);
	write_default_value(attribute->type_decl, s);
	s << std::endl;
	emit_store(ACC, DEFAULT_OBJFIELDS + offset, SELF, s);
      }
    } else {
      Context c(this);
      init->code(s, c);
      emit_store(ACC, DEFAULT_OBJFIELDS + offset, SELF, s);

      if (cgen_Memmgr == 1) {
	emit_addiu(A1, SELF, 4 * (offset + 3), s);
	emit_gc_assign(s);
      }
    }
  }

  emit_move(ACC, SELF, s);

  emit_load(FP, 3, SP, s);
  emit_load(SELF, 2, SP, s);
  emit_load(RA, 1, SP, s);
  emit_addiu(SP, SP, 12, s);
  s << RET << "\n";
}



void CgenClassTable::code() {
  if (cgen_debug) cout << "coding global data" << endl;
  code_global_data();
  
  if (cgen_debug) cout << "choosing gc" << endl;
  code_select_gc();
  
  if (cgen_debug) cout << "coding constants" << endl;
  code_constants();
  
  //                 Add your code to emit
  //                   - prototype objects
  //                   - class_nameTab
  //                   - dispatch tables

  
  write_nametabs();
  write_objtabs();
  write_disptabs();
  write_protobjs();
  
  if (cgen_debug) cout << "coding global text" << endl;
  code_global_text();
  
  //                 Add your code to emit
  //                   - object initializer
  //                   - the class methods
  //                   - etc...
  code_init();

  methods_code();
}


CgenNodeP CgenClassTable::root()
{
  return probe(Object);
}


std::vector<CgenNodeP> CgenClassTable::collect_all_classes() {
  std::vector<CgenNodeP> all_classes;
  std::queue<CgenNodeP> q;
  CgenNodeP top_class;
  q.push(root());

  while (!q.empty()) {
    top_class = q.front();
    all_classes.push_back(top_class);
    q.pop();

    std::vector<CgenNodeP> children = top_class->get_children_vect();
    int i;

    for (i = 0; i < (int)children.size(); i++) {
      q.push(children[i]);
    }
  }
  
  return all_classes;
}


void CgenClassTable::write_disptabs() {
  std::vector<CgenNodeP>all_classes = collect_all_classes();
  int i;
  for (i = 0; i < (int)all_classes.size(); i++) {
    CgenNodeP class_node = all_classes[i];
    class_node->write_disptab(str);
    str << "\n";
  }
}


void CgenClassTable::write_nametabs() {
  str << CLASSNAMETAB << LABEL;
  std::vector<CgenNodeP>all_classes = collect_all_classes();
  int i;
  for (i = 0; i < (int)all_classes.size(); i++) {
    CgenNodeP class_node = all_classes[i];
    class_node->write_nametab(str);
    str << "\n";
  }
}


void CgenClassTable::write_objtabs() {
  str << CLASSOBJTAB << LABEL;
  std::vector<CgenNodeP>all_classes = collect_all_classes();
  int i;
  for (i = 0; i < (int)all_classes.size(); i++) {
    CgenNodeP class_node = all_classes[i];
    class_node->write_objtab(str);
    str << "\n";
  }
}


void CgenClassTable::write_protobjs() {
  std::vector<CgenNodeP>all_classes = collect_all_classes();
  int i;
  for (i = 0; i < (int)all_classes.size(); i++) {
    CgenNodeP class_node = all_classes[i];
    class_node->write_protobj(str);
  }
}


void CgenClassTable::methods_code() {
  std::vector<MethodPair> user_methods = get_user_defined_methods();
  int i;
  for (i = 0; i < (int)user_methods.size(); i++) {
    MethodPair m = user_methods[i];
    m.method->code(str, m.class_name);
  }
}


void method_class::code(ostream &s, Symbol class_name) {
  emit_method_ref(class_name, name, s);
  s << LABEL;

  emit_addiu(SP, SP, -12, s);
  emit_store(FP, 3, SP, s);
  emit_store(SELF, 2, SP, s);
  emit_store(RA, 1, SP, s);

  emit_addiu(FP, SP, 4, s);
  emit_move(SELF, ACC, s);

  Context c(symbol_to_class[class_name]);
  int i;

  for (i = formals->first(); formals->more(i); i = formals->next(i)) {
    c.add_parameter(formals->nth(i)->get_name());
  }

  expr->code(s, c);
  
  emit_load(FP, 3, SP, s);
  emit_load(SELF, 2, SP, s);
  emit_load(RA, 1, SP, s);
  
  emit_addiu(SP, SP, (DEFAULT_OBJFIELDS + num_args()) * 4, s);
  s << RET << "\n";
}



std::vector<MethodPair> CgenClassTable::get_user_defined_methods() {
  std::vector<MethodPair> methods;
  std::set< std::pair<Symbol, Symbol> >visited;
  std::vector<CgenNodeP> all_classes = collect_all_classes();
  int i, j;

  for (i = 0; i < (int)all_classes.size(); i++) {
    CgenNodeP class_node = all_classes[i];
    if (class_predefined(class_node->get_name())) continue;
    std::vector<MethodPair> class_methods = class_node->get_class_methods();
    for (j = 0; j < (int)class_methods.size(); j++) {
      MethodPair p = class_methods[j];
      std::pair<Symbol, Symbol> pa = std::make_pair(p.class_name,
                                                    p.method->get_name());
      if (visited.find(pa) == visited.end()) {
        methods.push_back(p);
      }

      visited.insert(pa);
    }
  }

  return methods;
}


void CgenClassTable::code_init() {
  std::vector<CgenNodeP> all_classes = collect_all_classes();
  int i;
  for (i = 0; i < (int)all_classes.size(); i++) {
    CgenNodeP class_node = all_classes[i];
    class_node->code_init(str);
  }
}


///////////////////////////////////////////////////////////////////////
//
// CgenNode methods
//
///////////////////////////////////////////////////////////////////////

CgenNode::CgenNode(Class_ nd, Basicness bstatus, CgenClassTableP ct) :
class__class((const class__class &) *nd),
parentnd(NULL),
children(NULL),
basic_status(bstatus)
{ 
  stringtable.add_string(name->get_string());          // Add class name to string table
}


//******************************************************************
//
//   Fill in the following methods to produce code for the
//   appropriate expression.  You may add or remove parameters
//   as you wish, but if you do, remember to change the parameters
//   of the declarations in `cool-tree.h'  Sample code for
//   constant integers, strings, and booleans are provided.
//
//*****************************************************************



static void binary_op_code(Expression e1,
			   Expression e2,
			   ostream &s,
			   Context c,
			   bool is_comp_op) {
  e1->code(s, c);

  emit_push(ACC, s);
  c.add_no_type();

  e2->code(s, c);

  if (!is_comp_op)
    emit_jal(OBJCOPY, s);

  emit_addiu(SP, SP, 4, s);
  emit_load(T1, 0, SP, s);
  emit_move(T2, ACC, s);

  emit_fetch_int(T1, T1, s);
  emit_fetch_int(T2, T2, s);
}




void assign_class::code(ostream &s, Context c) {
  expr->code(s, c);

  int offset;

  if ((offset = c.lookup_let_variable(name)) != -1) {
    emit_store(ACC, offset + 1, SP, s);
    
    if (cgen_Memmgr == 1) {
      emit_addiu(A1, SP, 4 * (offset + 1), s);
      emit_gc_assign(s);
    }

  } else if ((offset = c.lookup_parameter(name)) != -1) {
    emit_store(ACC, offset + 3, FP, s);

    if (cgen_Memmgr == 1) {
      emit_addiu(A1, FP, 4 * (offset + 3), s);
      emit_gc_assign(s);
    }

  } else {
    // assign to attribute
    offset = c.lookup_attribute(name);
    emit_store(ACC, offset + DEFAULT_OBJFIELDS, SELF, s);

    if (cgen_Memmgr == 1) {
      emit_addiu(A1, SELF, 4 * (offset + 3), s);
      emit_gc_assign(s);
    }
  }
}



void static_dispatch_class::code(ostream &s, Context c) {
  int i;

  for (i = actual->first(); actual->more(i); i = actual->next(i)) {
    actual->nth(i)->code(s, c);
    emit_push(ACC, s);

    c.add_no_type();
  }

  expr->code(s, c);

  emit_bne(ACC, ZERO, labelnum, s);
  
  emit_load_address(ACC, "str_const0", s);
  emit_load_imm(T1, 1, s);
  emit_jal("_dispatch_abort", s);

  emit_label_def(labelnum++, s);

  CgenNodeP return_class = symbol_to_class[type_name];
  
  std::string disptab = symbol_to_string(type_name) + DISPTAB_SUFFIX;
  emit_load_address(T1, (char *)disptab.c_str(), s);
  
  int offset;
  std::vector<MethodPair>all_methods = return_class->get_all_methods();
  for (offset = 0; offset < (int)all_methods.size(); offset++) {
    MethodPair m = all_methods[offset];
    if (m.method->get_name() == name) {
      break;
    }
  }
  
  emit_load(T1, offset, T1, s);
  emit_jalr(T1, s);
}



void dispatch_class::code(ostream &s, Context c) {
  int i;

  for (i = actual->first(); actual->more(i); i = actual->next(i)) {
    actual->nth(i)->code(s, c);
    emit_push(ACC, s);

    c.add_no_type();
  }

  expr->code(s, c);

  emit_bne(ACC, ZERO, labelnum, s);
  
  emit_load_address(ACC, "str_const0", s);
  emit_load_imm(T1, 1, s);
  emit_jal("_dispatch_abort", s);

  emit_label_def(labelnum++, s);


  CgenNodeP return_class = c.get_current_class();
  if (expr->get_type() != SELF_TYPE) {
    return_class = symbol_to_class[expr->get_type()];
  }

  emit_load(T1, 2, ACC, s);
  
  int offset;
  std::vector<MethodPair>all_methods = return_class->get_all_methods();
  for (offset = 0; offset < (int)all_methods.size(); offset++) {
    MethodPair m = all_methods[offset];
    if (m.method->get_name() == name) {
      break;
    }
  }
  
  emit_load(T1, offset, T1, s);
  emit_jalr(T1, s);
}






void cond_class::code(ostream &s, Context c) {
  pred->code(s, c);
  emit_fetch_int(T1, ACC, s);

  int label_false, label_end;
  label_false = labelnum++;
  label_end = labelnum++;

  emit_beq(T1, ZERO, label_false, s);
  then_exp->code(s, c);
  emit_branch(label_end, s);

  emit_label_def(label_false, s);
  else_exp->code(s, c);
  
  emit_label_def(label_end, s);
}




void loop_class::code(ostream &s, Context c) {
  int loop_start, loop_end;
  loop_start = labelnum++;
  loop_end = labelnum++;

  emit_label_def(loop_start, s);
  pred->code(s, c);
  emit_fetch_int(T1, ACC, s);
  emit_beq(T1, ZERO, loop_end, s);
  body->code(s, c);
  emit_branch(loop_start, s);

  emit_label_def(loop_end, s);
  emit_move(ACC, ZERO, s);
}


void typcase_class::code(ostream &s, Context c) {
  expr->code(s, c);
  int success_label, case_label;
  success_label = labelnum;
  case_label = success_label + 1;
  labelnum = case_label + cases->len() + 1;
  

  emit_bne(ACC, ZERO, case_label, s);
  emit_load_address(ACC, "str_const0", s);
  emit_load_imm(T1, 1, s);
  emit_jal("_case_abort2", s);

  int i, class_tag;
  branch_class *ca;

  for (i = cases->first(); cases->more(i); i = cases->next(i)) {
    ca = (branch_class *)cases->nth(i);

    class_tag = class_name_to_tag[ca->type_decl];

    emit_label_def(case_label++, s);

    if (i == 0) {
      emit_load(T2, 0, ACC, s);
    }

    class_tag = class_name_to_tag[ca->type_decl];

    emit_blti(T2, class_tag, case_label, s);
    emit_bgti(T2, class_tag, case_label, s);

    c.add_let_var(ca->name);
    emit_push(ACC, s);

    ca->expr->code(s, c);

    emit_addiu(SP, SP, 4, s);
    c.pop_let_var();

    emit_branch(success_label, s);
  }

  emit_label_def(case_label, s);
  emit_jal("_case_abort", s);

  emit_label_def(success_label, s);
}





void block_class::code(ostream &s, Context c) {
  int i;
  for (i = body->first(); body->more(i); i = body->next(i)) {
    body->nth(i)->code(s, c);
  }
}




void let_class::code(ostream &s, Context c) {
  init->code(s, c);

  if (init->is_empty()) {
    if (type_decl == Int ||
	type_decl == Str ||
	type_decl == Bool) {
      emit_partial_load_address(ACC, s);
      write_default_value(type_decl, s);
      s << std::endl;
    }
  }

  emit_push(ACC, s);
  c.add_let_var(identifier);

  body->code(s, c);

  emit_addiu(SP, SP, 4, s);
}




void plus_class::code(ostream &s, Context c) {
  binary_op_code(e1, e2, s, c);
  
  emit_add(T3, T1, T2, s);
  
  emit_store(T3, 3, ACC, s);
}


void sub_class::code(ostream &s, Context c) {
  binary_op_code(e1, e2, s, c);
  
  emit_sub(T3, T1, T2, s);
  
  emit_store(T3, 3, ACC, s);
}



void mul_class::code(ostream &s, Context c) {
  binary_op_code(e1, e2, s, c);

  emit_mul(T3, T1, T2, s);

  emit_store(T3, 3, ACC, s);
}



void divide_class::code(ostream &s, Context c) {
  binary_op_code(e1, e2, s, c);

  emit_div(T3, T1, T2, s);

  emit_store(T3, 3, ACC, s);
}



void neg_class::code(ostream &s, Context c) {
  e1->code(s, c);

  emit_jal(OBJCOPY, s);
  emit_fetch_int(T1, ACC, s);
  emit_neg(T1, T1, s);

  emit_store(T1, 3, ACC, s);
}



void lt_class::code(ostream &s, Context c) {
  binary_op_code(e1, e2, s, c, true);

  emit_load_address(ACC, "bool_const1", s);

  emit_blt(T1, T2, labelnum, s);
  emit_load_address(ACC, "bool_const0", s);

  emit_label_def(labelnum++, s);
}




void eq_class::code(ostream &s, Context c) {
  e1->code(s, c);

  emit_push(ACC, s);
  c.add_no_type();

  e2->code(s, c);

  emit_addiu(SP, SP, 4, s);
  emit_load(T1, 0, SP, s);
  emit_move(T2, ACC, s);

  if (e1->type == Int ||
      e1->type == Str ||
      e1->type == Bool) {
    emit_load_bool(ACC, BoolConst(1), s);
    emit_load_bool(A1, BoolConst(0), s);
    emit_jal("equality_test", s);
    return;
  }

  emit_load_bool(ACC, BoolConst(1), s);
  emit_beq(T1, T2, labelnum, s);
  emit_load_bool(ACC, BoolConst(0), s);
  emit_label_def(labelnum++, s);
}





void leq_class::code(ostream &s, Context c) {
  binary_op_code(e1, e2, s, c, true);

  emit_load_address(ACC, "bool_const1", s);

  emit_bleq(T1, T2, labelnum, s);
  emit_load_address(ACC, "bool_const0", s);

  emit_label_def(labelnum++, s);
}



void comp_class::code(ostream &s, Context c) {
  e1->code(s, c);

  emit_fetch_int(T1, ACC, s);
  emit_load_bool(ACC, BoolConst(1), s);
  emit_beq(T1, ZERO, labelnum, s);
  emit_load_bool(ACC, BoolConst(0), s);
  
  emit_label_def(labelnum++, s);
}




void int_const_class::code(ostream& s, Context c) {
  //
  // Need to be sure we have an IntEntry *, not an arbitrary Symbol
  //
  emit_load_int(ACC,inttable.lookup_string(token->get_string()),s);
}

void string_const_class::code(ostream& s, Context c)
{
  emit_load_string(ACC,stringtable.lookup_string(token->get_string()),s);
}

void bool_const_class::code(ostream& s, Context c)
{
  emit_load_bool(ACC, BoolConst(val), s);
}


void new__class::code(ostream &s, Context c) {
  std::string protobj, objinit;
  protobj = symbol_to_string(type_name) + PROTOBJ_SUFFIX;
  objinit = symbol_to_string(type_name) + CLASSINIT_SUFFIX;

  if (type_name != SELF_TYPE) {
    emit_partial_load_address(ACC, s);
    s << protobj << std::endl;
    emit_jal(OBJCOPY, s);
    emit_jal(objinit, s);
  } else {
    emit_load_address(T1, CLASSOBJTAB, s);
    emit_load(T2, 0, SELF, s);
    emit_sll(T2, T2, 3, s);
    emit_addu(T1, T1, T2, s);
    emit_push(T1, s);
    emit_load(ACC, 0, T1, s);
    emit_jal(OBJCOPY, s);
    emit_load(T1, 1, SP, s);
    emit_addiu(SP, SP, 4, s);
    emit_load(T1, 1, T1, s);
    emit_jalr(T1, s);
  }
}



void isvoid_class::code(ostream &s, Context c) {
  e1->code(s, c);

  emit_move(T1, ACC, s);
  emit_load_bool(ACC, BoolConst(1), s);
  emit_beq(T1, ZERO, labelnum, s);
  emit_load_bool(ACC, BoolConst(0), s);

  emit_label_def(labelnum++, s);
}

void no_expr_class::code(ostream &s, Context c) {
  emit_move(ACC, ZERO, s);
}



void object_class::code(ostream &s, Context c) {
  int offset;

  if ((offset = c.lookup_let_variable(name)) != -1) {
    emit_load(ACC, offset + 1, SP, s);

    if (cgen_Memmgr == 1) {
      emit_addiu(A1, SP, 4 * (offset + 1), s);
      emit_gc_assign(s);
    }

  } else if ((offset = c.lookup_parameter(name)) != -1) {
    emit_load(ACC, offset + 3, FP, s);

    if (cgen_Memmgr == 1) {
      emit_addiu(A1, FP, 4 * (offset + 3), s);
      emit_gc_assign(s);
    }

  } else if ((offset = c.lookup_attribute(name)) != -1) {
    emit_load(ACC, offset + DEFAULT_OBJFIELDS, SELF, s);

    if (cgen_Memmgr == 1) {
      emit_addiu(A1, SELF, 4 * (offset + 3), s);
      emit_gc_assign(s);
    }

  } else {
    // name == self
    emit_move(ACC, SELF, s);
  }
}
