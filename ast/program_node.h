#ifndef __L22_AST_PROGRAM_NODE_H__
#define __L22_AST_PROGRAM_NODE_H__

#include <cdk/ast/basic_node.h>

namespace l22 {

  /**
   * Class for describing program nodes.
   */
  class program_node: public cdk::basic_node {
    l22::block_node *_statements;

  public:
    inline program_node(int lineno, l22::block_node *statements) :
        cdk::basic_node(lineno), _statements(statements) {
    }

  public:
    inline l22::block_node *statements() {
      return _statements;
    }

    void accept(basic_ast_visitor *sp, int level) {
      sp->do_program_node(this, level);
    }

  };

} // l22

#endif
