#ifndef __L22_TARGETS_POSTFIX_WRITER_H__
#define __L22_TARGETS_POSTFIX_WRITER_H__

#include "targets/basic_ast_visitor.h"

#include <stack>
#include <set>
#include <string>
#include <sstream>
#include <cdk/emitters/basic_postfix_emitter.h>
#include <stack>
#include <cdk/types/functional_type.h>

namespace l22 {

  //!
  //! Traverse syntax tree and generate the corresponding assembly code.
  //!
  class postfix_writer: public basic_ast_visitor {
    cdk::symbol_table<l22::symbol> &_symtab;

    
    std::stack<int> _whileInit, _whileEnd; // while again/stop labels
    std::set<std::string> _functions_to_declare;
    bool _inFunctionBody = false;
    bool _inFunctionArgs = false;
    bool _inFunctionDecl = false;
    bool _inFunctionCall = false;
    int _offset = 0;

    std::stack<std::shared_ptr<l22::symbol>> _funStack; // for keeping track of the current function and its arguments

    // remember function name for resolving '@'
    std::stack<std::string> _currentBodyRetLabel; // where to jump when a return occurs of an exclusive section ends
    int _currentBodyExitLabel = 0; // where to jump when a return occurs in the end section


    cdk::basic_postfix_emitter &_pf;
    int _lbl;


  public:
    postfix_writer(std::shared_ptr<cdk::compiler> compiler, cdk::symbol_table<l22::symbol> &symtab, cdk::basic_postfix_emitter &pf) :
        basic_ast_visitor(compiler), _symtab(symtab),  _pf(pf), _lbl(0) {
    }

  public:
    ~postfix_writer() {
      os().flush();
    }

  private:
    /** Method used to generate sequential labels. */
    inline std::string mklbl(int lbl) {
      std::ostringstream oss;
      if (lbl < 0)
        oss << ".L" << -lbl;
      else
        oss << "_L" << lbl;
      return oss.str();
    }

  public:
  // do not edit these lines
#define __IN_VISITOR_HEADER__
#include ".auto/visitor_decls.h"       // automatically generated
#undef __IN_VISITOR_HEADER__
  // do not edit these lines: end

  };

} // l22

#endif
