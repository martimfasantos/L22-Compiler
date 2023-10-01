#ifndef __L22_AST_FUNCTION_CALL_H__
#define __L22_AST_FUNCTION_CALL_H__

#include <string>
#include <cdk/ast/expression_node.h>
#include <cdk/ast/sequence_node.h>

namespace l22 {

  class function_call_node: public cdk::expression_node {
    cdk::expression_node *_fpointer;
    cdk::sequence_node *_arguments;

  public:
    inline function_call_node(int lineno, cdk::expression_node *fpointer) :
      cdk::expression_node(lineno), _fpointer(fpointer), _arguments(new cdk::sequence_node(lineno)) {
    }

    inline function_call_node(int lineno, cdk::expression_node *fpointer, cdk::sequence_node *arguments) :
      cdk::expression_node(lineno), _fpointer(fpointer), _arguments(arguments) {
    }

  public:
    inline cdk::expression_node *fpointer() {
      return _fpointer;
    }
    inline cdk::sequence_node* arguments() {
      return _arguments;
    }
    inline cdk::expression_node* argument(size_t ix) {
      return dynamic_cast<cdk::expression_node*>(_arguments->node(ix));
    }

    void accept(basic_ast_visitor *sp, int level) {
      sp->do_function_call_node(this, level);
    }

  };

} // l22

#endif
