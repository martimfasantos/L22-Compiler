#ifndef __L22_TARGETS_TYPE_CHECKER_H__
#define __L22_TARGETS_TYPE_CHECKER_H__

#include "targets/basic_ast_visitor.h"
#include <cdk/types/reference_type.h>
#include <cdk/types/functional_type.h>
#include <stack>

namespace l22 {

  /**
   * Print nodes as XML elements to the output stream.
   */
  class type_checker: public basic_ast_visitor {
    cdk::symbol_table<l22::symbol> &_symtab;

    basic_ast_visitor *_parent;

    std::stack<std::shared_ptr<l22::symbol>> &_funStack;

    bool _inBlock = false;


  public:
    type_checker(std::shared_ptr<cdk::compiler> compiler, cdk::symbol_table<l22::symbol> &symtab,
      basic_ast_visitor *parent, std::stack<std::shared_ptr<l22::symbol>> &funStack) :
        basic_ast_visitor(compiler), _symtab(symtab), _parent(parent), _funStack(funStack) {
    }


  public:
    ~type_checker() {
      os().flush();
    }

  protected:
    void processUnaryExpression(cdk::unary_operation_node *const node, int lvl);
    void processScalarLogicalExpression(cdk::binary_operation_node *const node, int lvl);
    void processIDExpression(cdk::binary_operation_node *const node, int lvl);
    void processIntOnlyExpression(cdk::binary_operation_node *const node, int lvl);
    void processBooleanLogicExpression(cdk::binary_operation_node *const node, int lvl);
    void processGeneralLogicalExpression(cdk::binary_operation_node *const node, int lvl);
    std::shared_ptr<cdk::basic_type> typeOfPointer(std::shared_ptr<cdk::reference_type> leftPtr, std::shared_ptr<cdk::reference_type> rightPtr);
    template<typename T>
    void process_literal(cdk::literal_node<T> *const node, int lvl) {
    }

  public:
    // do not edit these lines
#define __IN_VISITOR_HEADER__
#include ".auto/visitor_decls.h"       // automatically generated
#undef __IN_VISITOR_HEADER__
    // do not edit these lines: end

  };

} // l22

//---------------------------------------------------------------------------
//     HELPER MACRO FOR TYPE CHECKING
//---------------------------------------------------------------------------

#define CHECK_TYPES(compiler, symtab, node, funStack) { \
  try { \
    l22::type_checker checker(compiler, symtab, this, funStack); \
    (node)->accept(&checker, 0); \
  } \
  catch (const std::string &problem) { \
    std::cerr << (node)->lineno() << ": " << problem << std::endl; \
    return; \
  } \
}

#define ASSERT_SAFE_EXPRESSIONS CHECK_TYPES(_compiler, _symtab, node, _funStack)

#endif
