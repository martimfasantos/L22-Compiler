#ifndef __L22_AST_FUNCTION_DEFINITION_H__
#define __L22_AST_FUNCTION_DEFINITION_H__

#include <string>
#include <cdk/ast/typed_node.h>
#include <cdk/ast/sequence_node.h>
#include "ast/block_node.h"

namespace l22 {

  //!
  //! Class for describing function definitions.
  //! <pre>
  //! (cdk::sequence_node) -> type :
  //!               $$ = new l22::block_node
  //! </pre>
  //!
  class function_definition_node: public cdk::expression_node {
    cdk::sequence_node *_arguments;
    l22::block_node *_block;
    std::shared_ptr<cdk::basic_type> _output_type;

  public:
    // podemos inicializar o type apenas no type checker e aqui guardamos só o output type?
    function_definition_node(int lineno, cdk::sequence_node *arguments, std::shared_ptr<cdk::basic_type> funType , l22::block_node *block) :
        cdk::expression_node(lineno), _arguments(arguments), _block(block), _output_type(funType) {
      // type(funType);
    }

  public:
    cdk::sequence_node* arguments() {
      return _arguments;
    }
    cdk::typed_node* argument(size_t ax) {
      return dynamic_cast<cdk::typed_node*>(_arguments->node(ax));
    }
    l22::block_node* block() {
      return _block;
    }

    std::shared_ptr<cdk::basic_type> output_type() {
      return _output_type;
    }

    void accept(basic_ast_visitor *sp, int level) {
      sp->do_function_definition_node(this, level);
    }

  };

} // l22

#endif
