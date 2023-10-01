#include <string>
#include <sstream>
#include "targets/type_checker.h"
#include "targets/postfix_writer.h"
#include "targets/frame_size_calculator.h"
#include "targets/symbol.h"
#include ".auto/all_nodes.h"  // all_nodes.h is automatically generated
#include "l22_parser.tab.h"

//---------------------------------------------------------------------------


void l22::postfix_writer::do_nil_node(cdk::nil_node * const node, int lvl) {
  // EMPTY
}


void l22::postfix_writer::do_data_node(cdk::data_node * const node, int lvl) {
  // EMPTY
}

//---------------------------------------------------------------------------


void l22::postfix_writer::do_index_node(l22::index_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;

  node->base()->accept(this, lvl);
  node->index()->accept(this, lvl);

  _pf.INT(node->type()->size());
  _pf.SHTL();
  _pf.ADD();
}


void l22::postfix_writer::do_stack_alloc_node(l22::stack_alloc_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  node->argument()->accept(this, lvl);
  if(cdk::reference_type::cast(node->type())->referenced()->name() == cdk::TYPE_DOUBLE){
    _pf.INT(3);
  } else {
    _pf.INT(2);
  }

  _pf.SHTL();
  _pf.ALLOC();    
  _pf.SP();
}


void l22::postfix_writer::do_nullptr_node(l22::nullptr_node * const node, int lvl) {
	ASSERT_SAFE_EXPRESSIONS;
  if (_inFunctionBody) {
    _pf.INT(0);
  } else {
    _pf.SINT(0);
  }
}


void l22::postfix_writer::do_sizeof_node(l22::sizeof_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  _pf.INT(node->expression()->type()->size());
}

//---------------------------------------------------------------------------

void l22::postfix_writer::do_declaration_node(l22::declaration_node * const node , int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  
  std::string id = node->identifier();
  
  int offset = 0;
  int size = node->type()->size();


  // offset (Local variables)
  if(_inFunctionBody) {
    _offset -= size;
    offset = _offset;
  } else if(_inFunctionArgs) {
    offset = _offset;
    _offset += size;
  } else {
    offset = 0;
  }

  std::shared_ptr<l22::symbol> symbol = new_symbol();

  if (symbol) {
    symbol->set_offset(offset);
    reset_new_symbol();
  }

  if (node->initializer()) {

    if(_inFunctionBody) { 
      node->initializer()->accept(this, lvl);

      if (dynamic_cast<l22::function_definition_node *>(node->initializer())) {
        _pf.ADDR(mklbl(_lbl));
      }

      if(node->is_typed(cdk::TYPE_DOUBLE)) {

        if(node->initializer()->is_typed(cdk::TYPE_INT)){
          _pf.I2D();
        }

        _pf.LOCAL(offset);
        _pf.STDOUBLE();
      } else if (node->is_typed(cdk::TYPE_FUNCTIONAL)){
        // EMPTY
      } else {

        _pf.LOCAL(offset);
        _pf.STINT();
      }

    } else if (!_inFunctionBody && !_inFunctionArgs) {
      if(node->is_typed(cdk::TYPE_INT) || node->is_typed(cdk::TYPE_POINTER) 
      || node->is_typed(cdk::TYPE_DOUBLE)) {
        _pf.DATA();
        _pf.ALIGN();
          // Variaveis globais
        if (node->qualifier() == tPUBLIC || node->qualifier() == tUSE || _funStack.size() == 0) {
          _pf.GLOBAL(node->identifier(), _pf.OBJ());
          
        }
        _pf.LABEL(node->identifier());

        if (node->is_typed(cdk::TYPE_INT) || node->is_typed(cdk::TYPE_POINTER) || node->is_typed(cdk::TYPE_FUNCTIONAL)) {
      
          node->initializer()->accept(this, lvl);

        } else if (node->is_typed(cdk::TYPE_DOUBLE)){

          if(node->initializer()->is_typed(cdk::TYPE_DOUBLE)){
            node->initializer()->accept(this, lvl);

          } else if (node->initializer()->is_typed(cdk::TYPE_INT)) {
            cdk::integer_node *dclini = dynamic_cast<cdk::integer_node *>(node->initializer());
            cdk::double_node ddi(dclini->lineno(), dclini->value());
            ddi.accept(this, lvl);
          } else {
            std::cerr << node->lineno() << ": '" << id << "' wrong initializer for real variable.\n";
            exit(2);
          }
        }

      } else if (node->is_typed(cdk::TYPE_STRING)) {
        _pf.DATA(); 
        _pf.ALIGN();
        _pf.LABEL(node->identifier());
        node->initializer()->accept(this, lvl); 

      } else if (node->is_typed(cdk::TYPE_FUNCTIONAL)){

        _inFunctionDecl = true;
        node->initializer()->accept(this, lvl);
        _inFunctionDecl = false;

        _pf.DATA();
        _pf.ALIGN();
          // Variaveis globais
        if (node->qualifier() == tPUBLIC || node->qualifier() == tUSE || _funStack.size() == 0) {
          _pf.GLOBAL(node->identifier(), _pf.OBJ());
          
        }
        _pf.LABEL(node->identifier());

        if (!_inFunctionBody){
          _pf.SADDR(_funStack.top()->label());
        } else {
          _pf.ADDR(_funStack.top()->label());
        }

        // pop function from stack
        _funStack.pop();
      }
    }
    else {
      std::cerr << node->lineno() << ": '" << id << "' has an unexpected initializer.\n";
      exit(2);
    }

    // Declaracoes globais: ex. int a
  } else {
    // functions
    if(node->is_typed(cdk::TYPE_FUNCTIONAL) && node->qualifier() == tFOREIGN) {
      _functions_to_declare.insert(node->identifier());
    }

    // not functions
    if (!_inFunctionBody && !_inFunctionArgs && (node->is_typed(cdk::TYPE_INT) || node->is_typed(cdk::TYPE_POINTER)
      || node->is_typed(cdk::TYPE_DOUBLE) || node->is_typed(cdk::TYPE_STRING)) ) {
      auto symbol = l22::make_symbol(node->qualifier(), node->type(), id, (bool)node->initializer());
      _symtab.insert(id, symbol);
      _pf.BSS();
      _pf.ALIGN();
      _pf.LABEL(id);
      _pf.SALLOC(size);
    }
  }

}

void l22::postfix_writer::do_function_call_node(l22::function_call_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;

  cdk::rvalue_node *aux = dynamic_cast<cdk::rvalue_node *>(node->fpointer());
  cdk::variable_node *identifier = nullptr;

  if (aux != nullptr) {
    identifier = dynamic_cast<cdk::variable_node *>(aux->lvalue());
  }


  std::shared_ptr<l22::symbol> symbol;
  // function call with id or @
  if (identifier != nullptr) {
    std::string id;

    // Chamada recursiva
    if (identifier->name() == "@") {  
      symbol = _funStack.top();
    } else {
      symbol = _symtab.find(identifier->name());
    }
  
  } else {
    symbol = _funStack.top();
    // pop from stack after call
    _funStack.pop();
  }


  size_t arguments_size = 0;
  if (node->arguments()->size() > 0) {
    for (int ax = node->arguments()->size() - 1; ax >= 0; ax--) {
      cdk::expression_node *arg = node->argument(ax);
      arg->accept(this, lvl + 2);
      std::shared_ptr<cdk::basic_type> i_type = cdk::functional_type::cast(symbol->type())->input(ax);
  
      if (i_type->name() == cdk::TYPE_DOUBLE && arg->is_typed(cdk::TYPE_INT)) {
        _pf.I2D();
      }
      else if (i_type->name() == cdk::TYPE_FUNCTIONAL) {
        _pf.INT(_lbl);
        _pf.LDINT();
      }
      arguments_size += arg->type()->size();
    }
  }

  node->fpointer()->accept(this, lvl + 2);


  // foreign
  if (_functions_to_declare.find(symbol->name()) != _functions_to_declare.end()) {
    _pf.CALL(symbol->name());
  }
  else {
    _pf.BRANCH();
  }


  if(arguments_size != 0){
    _pf.TRASH(arguments_size);
  }    

  // Load return function value after call
  if (symbol != nullptr){

    if(symbol->ftype()->output(0)->name() == cdk::TYPE_INT || symbol->ftype()->output(0)->name() == cdk::TYPE_POINTER || 
      symbol->ftype()->output(0)->name() == cdk::TYPE_STRING || symbol->ftype()->output(0)->name() == cdk::TYPE_FUNCTIONAL ) {
  
      _pf.LDFVAL32();
    } else if(symbol->ftype()->output(0)->name() == cdk::TYPE_DOUBLE) {
      _pf.LDFVAL64();
    }

  } else if (symbol->ftype()->output(0)->name() != cdk::TYPE_VOID) {
    std::cerr << "Must not happen." << std::endl;
  } 
}

void l22::postfix_writer::do_function_definition_node(l22::function_definition_node * const node , int lvl) {
  ASSERT_SAFE_EXPRESSIONS;

  std::string label;
  if (node->block()) {

    if(_funStack.top()->name() == "_main") {
      label = "._main";
    
    } else {
      label = mklbl(++_lbl);
      _funStack.top()->set_label(label);
    }

    _currentBodyRetLabel.push(mklbl(++_lbl));

    _offset = 8;
    _symtab.push();

    if (node->arguments()){
      _inFunctionArgs = true;
      node->arguments()->accept(this, lvl+2);
      _inFunctionArgs = false;
    }

    _pf.TEXT();
    _pf.ALIGN();
    
    _pf.LABEL(label);

    frame_size_calculator lsc(_compiler, _symtab);
    node->accept(&lsc, lvl);
    _pf.ENTER(lsc.localsize());


    _offset = 0;
    _inFunctionBody = true;
    node->block()->accept(this, lvl + 4);
    _inFunctionBody = false;
    _symtab.pop();

    _pf.LABEL(_currentBodyRetLabel.top());
    _pf.LEAVE();
    _pf.RET();

    _currentBodyRetLabel.pop();
  
  } else {
    if(_funStack.size() == 0) {
      return;
    }

  }

  if (!_inFunctionDecl && !_inFunctionCall){
    // pop function from stack
    _funStack.pop();
  }
}


void l22::postfix_writer::do_block_node(l22::block_node * const node, int lvl) {
	_symtab.push(); 
  if (node->declarations()) node->declarations()->accept(this, lvl + 2);
  if (node->instructions()) node->instructions()->accept(this, lvl + 2);
  _symtab.pop();
}

void l22::postfix_writer::do_return_node(l22::return_node * const node, int lvl) {
  

  if (_funStack.top()->ftype()->output(0)->name() != cdk::TYPE_VOID) {

    node->retval()->accept(this, lvl + 2);


    if (_funStack.top()->ftype()->output(0)->name() == cdk::TYPE_INT ||
     _funStack.top()->ftype()->output(0)->name() == cdk::TYPE_STRING ||
     _funStack.top()->ftype()->output(0)->name() == cdk::TYPE_POINTER||
     _funStack.top()->ftype()->output(0)->name() == cdk::TYPE_FUNCTIONAL ){
      _pf.STFVAL32();
  
    
    } else if (_funStack.top()->ftype()->output(0)->name() == cdk::TYPE_DOUBLE) {

  
      if (node->retval()->is_typed(cdk::TYPE_INT)) {
        _pf.I2D();
      }
      _pf.STFVAL64();

    } else {
      std::cerr << node->lineno() << ": should not happen: unknown return type" << std::endl;
    }
  }

  if (_funStack.size() != 0){
    if (_funStack.top()->name() == "_main"){
      _funStack.pop();
    } else {
  
    }
  } else {

  }

  _pf.JMP(_currentBodyRetLabel.top());

}

//---------------------------------------------------------------------------


void l22::postfix_writer::do_sequence_node(cdk::sequence_node * const node, int lvl) {
  for (size_t i = 0; i < node->size(); i++) {
    node->node(i)->accept(this, lvl);
  }
}

//---------------------------------------------------------------------------


void l22::postfix_writer::do_integer_node(cdk::integer_node * const node, int lvl) {
  if (_inFunctionBody) {
    _pf.INT(node->value());
  } else {
    _pf.SINT(node->value());
  }
}


void l22::postfix_writer::do_double_node(cdk::double_node * const node, int lvl) {

  if (_inFunctionBody) {
    _pf.DOUBLE(node->value()); // load number to the stack
  } else {
    _pf.SDOUBLE(node->value());  // double is on the DATA segment (static)
  }
}


void l22::postfix_writer::do_string_node(cdk::string_node * const node, int lvl) {
  int lbl1;

  _pf.RODATA();
  _pf.ALIGN();
  _pf.LABEL(mklbl(lbl1 = ++_lbl));
  _pf.SSTRING(node->value());

  _pf.ALIGN();
  // Variavel local dentro do corpo de uma funcao
  if (_inFunctionBody) {
    _pf.TEXT();
    _pf.ADDR(mklbl(lbl1));
  } else { // Variavel static 
    _pf.DATA();
    _pf.SADDR(mklbl(lbl1));
  }
}


/* --------------------------------------------------------------------------- */
/* ----|                      UNARY EXPRESSIONS                          |---- */
/* --------------------------------------------------------------------------- */


void l22::postfix_writer::do_identity_node(l22::identity_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  node->argument()->accept(this, lvl);
}


void l22::postfix_writer::do_neg_node(cdk::neg_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  node->argument()->accept(this, lvl);
  if(node->is_typed(cdk::TYPE_INT)) {
    _pf.NEG();
  } else if(node->is_typed(cdk::TYPE_DOUBLE)) {
    _pf.DNEG();
  }
}


void l22::postfix_writer::do_address_of_node(l22::address_of_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS
  node->lvalue()->accept(this, lvl);
}


void l22::postfix_writer::do_not_node(cdk::not_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  node->argument()->accept(this, lvl);
  _pf.INT(0);
  _pf.EQ();
}


/* --------------------------------------------------------------------------- */
/* ----|                      BINARY EXPRESSIONS                         |---- */
/* --------------------------------------------------------------------------- */


void l22::postfix_writer::do_mul_node(cdk::mul_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  node->left()->accept(this, lvl);
  if(node->is_typed(cdk::TYPE_DOUBLE) && node->left()->is_typed(cdk::TYPE_INT)) {
    _pf.I2D();
  }
  node->right()->accept(this, lvl);
  if(node->is_typed(cdk::TYPE_DOUBLE) && node->right()->is_typed(cdk::TYPE_INT)) {
    _pf.I2D();
  }

  if(node->is_typed(cdk::TYPE_DOUBLE)) {
    _pf.DMUL();
  } else {
    _pf.MUL();
  }
}


void l22::postfix_writer::do_div_node(cdk::div_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  node->left()->accept(this, lvl);
  if(node->is_typed(cdk::TYPE_DOUBLE) && node->left()->is_typed(cdk::TYPE_INT)) {
    _pf.I2D();
  }
  node->right()->accept(this, lvl);
  if(node->is_typed(cdk::TYPE_DOUBLE) && node->right()->is_typed(cdk::TYPE_INT)) {
    _pf.I2D();
  }

  if(node->is_typed(cdk::TYPE_DOUBLE)) {
    _pf.DDIV();
  } else {
    _pf.DIV();
  }
}

void l22::postfix_writer::do_mod_node(cdk::mod_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  node->left()->accept(this, lvl);
  node->right()->accept(this, lvl);
  _pf.MOD();
}

//---------------------------------------------------------------------------


void l22::postfix_writer::do_add_node(cdk::add_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  node->left()->accept(this, lvl);
  // converter esquerda para double pois direita é double
  if(node->is_typed(cdk::TYPE_DOUBLE) && node->left()->is_typed(cdk::TYPE_INT)) {
    _pf.I2D();
  }
  else if(node->is_typed(cdk::TYPE_POINTER) && node->left()->is_typed(cdk::TYPE_INT)) {
    _pf.INT(3);
    _pf.SHTL();
  }

  node->right()->accept(this, lvl);
  if(node->is_typed(cdk::TYPE_DOUBLE) && node->right()->is_typed(cdk::TYPE_INT)) {
    _pf.I2D();
  }
  else if (node->is_typed(cdk::TYPE_POINTER) && node->right()->is_typed(cdk::TYPE_INT)){

    _pf.INT(3);
    _pf.SHTL();
  }

  if(node->is_typed(cdk::TYPE_DOUBLE)) {
    _pf.DADD();
  } else {
    _pf.ADD();
  }
}

void l22::postfix_writer::do_sub_node(cdk::sub_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  node->left()->accept(this, lvl);

  // converter esquerda para double pois direita é double
  if(node->is_typed(cdk::TYPE_DOUBLE) && node->left()->is_typed(cdk::TYPE_INT)) {
    _pf.I2D();
  }

  node->right()->accept(this, lvl);
  if(node->is_typed(cdk::TYPE_DOUBLE) && node->right()->is_typed(cdk::TYPE_INT)) {
    _pf.I2D();
  }
  else if (node->type()->name() == cdk::TYPE_POINTER && node->right()->type()->name() == cdk::TYPE_INT) {
    _pf.INT(3);
    _pf.SHTL();
  }

  if (node->left()->is_typed(cdk::TYPE_POINTER) && node->right()->is_typed(cdk::TYPE_POINTER)){
    int lbl1;

    _pf.SUB();
    _pf.INT(cdk::reference_type::cast(node->left()->type())->referenced()->size());
    _pf.DIV();
    _pf.DUP32();
    _pf.INT(0);
    _pf.LT(); 
    _pf.JZ(mklbl(lbl1 = ++_lbl));
    _pf.NEG();
    _pf.ALIGN();
    _pf.LABEL(mklbl(lbl1));

  } else {
    if(node->is_typed(cdk::TYPE_DOUBLE)) {
        _pf.DSUB();
      } else {
        _pf.SUB();
      }
  }
}

//---------------------------------------------------------------------------


void l22::postfix_writer::do_lt_node(cdk::lt_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  node->left()->accept(this, lvl);
  if(node->right()->is_typed(cdk::TYPE_DOUBLE) && node->left()->is_typed(cdk::TYPE_INT)) {
    _pf.I2D();
  }

  node->right()->accept(this, lvl);
  if(node->left()->is_typed(cdk::TYPE_DOUBLE) && node->right()->is_typed(cdk::TYPE_INT)) {
    _pf.I2D();
  }

  // Se um ja foi apanhado como double, ja printou um I2D e vao ser os 2 double
  if(node->left()->is_typed(cdk::TYPE_DOUBLE) || node->right()->is_typed(cdk::TYPE_DOUBLE)){
    _pf.DCMP(); // o compare entre 2 doubles retorna um inteiro: >0 se 1o > 2o, <0 se 1o < 2o e 0 caso sejam iguais
    _pf.INT(0); // por o 0 na pilha na stack para comparar
  }

  _pf.LT();
}


void l22::postfix_writer::do_le_node(cdk::le_node * const node, int lvl) {
   ASSERT_SAFE_EXPRESSIONS;
  node->left()->accept(this, lvl);
  if(node->right()->is_typed(cdk::TYPE_DOUBLE) && node->left()->is_typed(cdk::TYPE_INT)) {
    _pf.I2D();
  }

  node->right()->accept(this, lvl);
  if(node->left()->is_typed(cdk::TYPE_DOUBLE) && node->right()->is_typed(cdk::TYPE_INT)) {
    _pf.I2D();
  }

  // Se um ja foi apanhado como double, ja printou um I2D e vao ser os 2 double
  if(node->left()->is_typed(cdk::TYPE_DOUBLE) || node->right()->is_typed(cdk::TYPE_DOUBLE)){
    _pf.DCMP(); // o compare entre 2 doubles retorna um inteiro: >0 se 1o > 2o, <0 se 1o < 2o e 0 caso sejam iguais
    _pf.INT(0); // por o 0 na pilha na stack para comparar
  }

  _pf.LE();
}


void l22::postfix_writer::do_ge_node(cdk::ge_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  node->left()->accept(this, lvl);
  if(node->right()->is_typed(cdk::TYPE_DOUBLE) && node->left()->is_typed(cdk::TYPE_INT)) {
    _pf.I2D();
  }

  node->right()->accept(this, lvl);
  if(node->left()->is_typed(cdk::TYPE_DOUBLE) && node->right()->is_typed(cdk::TYPE_INT)) {
    _pf.I2D();
  }

  // Se um ja foi apanhado como double, ja printou um I2D e vao ser os 2 double
  if(node->left()->is_typed(cdk::TYPE_DOUBLE) || node->right()->is_typed(cdk::TYPE_DOUBLE)){
    _pf.DCMP(); // o compare entre 2 doubles retorna um inteiro: >0 se 1o > 2o, <0 se 1o < 2o e 0 caso sejam iguais
    _pf.INT(0); // por o 0 na pilha na stack para comparar
  }

  _pf.GE();
}


void l22::postfix_writer::do_gt_node(cdk::gt_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  node->left()->accept(this, lvl);
  if(node->right()->is_typed(cdk::TYPE_DOUBLE) && node->left()->is_typed(cdk::TYPE_INT)) {
    _pf.I2D();
  }

  node->right()->accept(this, lvl);
  if(node->left()->is_typed(cdk::TYPE_DOUBLE) && node->right()->is_typed(cdk::TYPE_INT)) {
    _pf.I2D();
  }

  // Se um ja foi apanhado como double, ja printou um I2D e vao ser os 2 double
  if(node->left()->is_typed(cdk::TYPE_DOUBLE) || node->right()->is_typed(cdk::TYPE_DOUBLE)){
    _pf.DCMP(); // o compare entre 2 doubles retorna um inteiro: >0 se 1o > 2o, <0 se 1o < 2o e 0 caso sejam iguais
    _pf.INT(0); // por o 0 na pilha na stack para comparar
  }

  _pf.GT();
}

//---------------------------------------------------------------------------


void l22::postfix_writer::do_ne_node(cdk::ne_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  node->left()->accept(this, lvl);
  if(node->right()->is_typed(cdk::TYPE_DOUBLE) && node->left()->is_typed(cdk::TYPE_INT)) {
    _pf.I2D();
  }

  node->right()->accept(this, lvl);
  if(node->left()->is_typed(cdk::TYPE_DOUBLE) && node->right()->is_typed(cdk::TYPE_INT)) {
    _pf.I2D();
  }

  // Se um ja foi apanhado como double, ja printou um I2D e vao ser os 2 double
  if(node->left()->is_typed(cdk::TYPE_DOUBLE) || node->right()->is_typed(cdk::TYPE_DOUBLE)){
    _pf.DCMP(); // o compare entre 2 doubles retorna um inteiro: >0 se 1o > 2o, <0 se 1o < 2o e 0 caso sejam iguais
    _pf.INT(0); // por o 0 na pilha na stack para comparar
  }

  _pf.NE();
}


void l22::postfix_writer::do_eq_node(cdk::eq_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  node->left()->accept(this, lvl);
  if(node->right()->is_typed(cdk::TYPE_DOUBLE) && node->left()->is_typed(cdk::TYPE_INT)) {
    _pf.I2D();
  }

  node->right()->accept(this, lvl);
  if(node->left()->is_typed(cdk::TYPE_DOUBLE) && node->right()->is_typed(cdk::TYPE_INT)) {
    _pf.I2D();
  }

  // Se um ja foi apanhado como double, ja printou um I2D e vao ser os 2 double
  if(node->left()->is_typed(cdk::TYPE_DOUBLE) || node->right()->is_typed(cdk::TYPE_DOUBLE)){
    _pf.DCMP(); // o compare entre 2 doubles retorna um inteiro: >0 se 1o > 2o, <0 se 1o < 2o e 0 caso sejam iguais
    _pf.INT(0); // por o 0 na pilha na stack para comparar
  }

  _pf.EQ();
}

//---------------------------------------------------------------------------

void l22::postfix_writer::do_and_node(cdk::and_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  int lbl = ++_lbl;
  node->left()->accept(this, lvl);
  _pf.DUP32();  // só da ints
  _pf.JZ(mklbl(lbl));  // Se o primeiro argumento for 0/False, nem avalia o segundo
  node->right()->accept(this, lvl);
  _pf.AND();
  _pf.ALIGN();
  _pf.LABEL(mklbl(lbl));
}


void l22::postfix_writer::do_or_node(cdk::or_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  int lbl = ++_lbl;
  node->left()->accept(this, lvl);
  _pf.DUP32();  // só da ints
  _pf.JNZ(mklbl(lbl));  // Se o primeiro argumento for 1/True, nem avalia o segundo
  node->right()->accept(this, lvl);
  _pf.OR();
  _pf.ALIGN();
  _pf.LABEL(mklbl(lbl));
}


//---------------------------------------------------------------------------

void l22::postfix_writer::do_assignment_node(cdk::assignment_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  // Casos: static, globais e locais
  node->rvalue()->accept(this, lvl); // determine the new value
  if(node->is_typed(cdk::TYPE_DOUBLE)){
    if(node->rvalue()->is_typed(cdk::TYPE_INT)){
      _pf.I2D();
    }
    _pf.DUP64();
  } else {
    _pf.DUP32();
  }

  node->lvalue()->accept(this, lvl);
  if (node->type()->name() == cdk::TYPE_DOUBLE) {


    _pf.STDOUBLE();
  } else {
    _pf.STINT();
  }
}

//---------------------------------------------------------------------------


void l22::postfix_writer::do_variable_node(cdk::variable_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;

  std::shared_ptr<l22::symbol> var;
  const std::string &id = node->name();
  if (id == "@") {
      if (_funStack.size() == 0 || _funStack.top()->name() == "_main") {
        throw std::string("Recursive call outside function");
      }
      var = _funStack.top();
      _pf.ADDR(var->label());
  }
  else {
    var = _symtab.find(node->name());
    if (var->offset() == 0){ // static
      _pf.ADDR(var->name());
    } else { // local
      _pf.LOCAL(var->offset());
    }
  }
}


void l22::postfix_writer::do_rvalue_node(cdk::rvalue_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  node->lvalue()->accept(this, lvl);


  cdk::variable_node *vnode = dynamic_cast<cdk::variable_node *>(node->lvalue());
  if (vnode != nullptr && vnode->name() == "@") {
    return;
  }
  else {
    if(node->is_typed(cdk::TYPE_DOUBLE)){
      _pf.LDDOUBLE();
    } 
    else if (node->is_typed(cdk::TYPE_POINTER)) {

    }
    else {
      _pf.LDINT();
    }
  }
}


//---------------------------------------------------------------------------


void l22::postfix_writer::do_program_node(l22::program_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  // Note that L22 doesn't have a main function. Thus, it doesn't need
  // a function node. However, it must start in the a "main" function.
  // The ProgramNode (representing the whole program) doubles as a
  // main function node.

  // generate the main function (RTS mandates that its name be "_main")
  _pf.TEXT();
  _pf.ALIGN();
  _pf.GLOBAL("_main", _pf.FUNC());
  _pf.LABEL("_main");

  _currentBodyRetLabel.push(mklbl(++_lbl));

  frame_size_calculator lsc(_compiler, _symtab);
  node->accept(&lsc, lvl);
  _pf.ENTER(lsc.localsize());

  _inFunctionBody = true;
  node->statements()->accept(this, lvl);
  _inFunctionBody = false;

  // end the main function
  _pf.LABEL(_currentBodyRetLabel.top());
  _pf.INT(0);
  _pf.STFVAL32();
  _pf.LEAVE();
  _pf.RET();

  _currentBodyRetLabel.pop();

  for (std::string s : _functions_to_declare){
    _pf.EXTERN(s);
  }
}

//---------------------------------------------------------------------------


void l22::postfix_writer::do_evaluation_node(l22::evaluation_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;

  node->argument()->accept(this, lvl);
  _pf.TRASH(node->argument()->type()->size());
}


void l22::postfix_writer::do_read_node(l22::read_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  if(node->is_typed(cdk::TYPE_INT)) {
    _functions_to_declare.insert("readi");
    _pf.CALL("readi");
    _pf.LDFVAL32();
  }
  else if (node->is_typed(cdk::TYPE_DOUBLE)) {
    _functions_to_declare.insert("readd");
    _pf.CALL("readd");
    _pf.LDFVAL64();
  }
}

void l22::postfix_writer::do_print_node(l22::print_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;  
  
  for (size_t ix = 0; ix < node->arguments()->size(); ix++) {
    node->arguments()->node(ix)->accept(this, lvl);

    cdk::typed_node *etype = dynamic_cast<cdk::typed_node *>(node->arguments()->node(ix));

    if (etype != nullptr){
      if (etype->type()->name() == cdk::TYPE_INT) {
        _functions_to_declare.insert("printi");
        _pf.CALL("printi");
        _pf.TRASH(4); // trash int
      } else if (etype->type()->name() == cdk::TYPE_DOUBLE) {
        _functions_to_declare.insert("printd");
        _pf.CALL("printd");
        _pf.TRASH(8); // trash double
      } else if (etype->type()->name() == cdk::TYPE_STRING) {
        _functions_to_declare.insert("prints");
        _pf.CALL("prints");
        _pf.TRASH(4); // trash char pointer
      }
    } else {
      std::cerr << "cannot print expression of unknown type" << std::endl;
      return;
    }
  }
  

  if(node->newLine()) {
    _functions_to_declare.insert("println");
    _pf.CALL("println");
  }

}


//---------------------------------------------------------------------------


void l22::postfix_writer::do_while_node(l22::while_node * const node, int lvl) {

  int whileInit = ++_lbl; _whileInit.push(whileInit); // after init, bewhilee body
  int whileEnd = ++_lbl; _whileEnd.push(whileEnd); // after while (end)

  _symtab.push();

  os() << "        ;; WHILE test" << std::endl;
  _pf.ALIGN();
  _pf.LABEL(mklbl(whileInit));
  node->condition()->accept(this, lvl);
  _pf.JZ(mklbl(whileEnd));

  os() << "        ;; WHILE body " << std::endl;
  node->block()->accept(this, lvl + 2); 

  os() << "        ;; WHILE jump to test" << std::endl;
  _pf.JMP(mklbl(whileInit));   // Jump to start

  os() << "        ;; WHILE end" << std::endl;
  _pf.ALIGN();
  _pf.LABEL(mklbl(whileEnd)); // End while

  _symtab.pop();
  _whileInit.pop();
  _whileEnd.pop();
}


void l22::postfix_writer::do_again_node(l22::again_node * const node, int lvl) {
	if(_whileInit.size() > 0){
    _pf.JMP(mklbl(_whileInit.top()));
  } else {
    std::cerr << "Again instruction can only be used inside a while loop." << std::endl;
  }
}


void l22::postfix_writer::do_stop_node(l22::stop_node * const node, int lvl) {
	if(_whileEnd.size() > 0){
    _pf.JMP(mklbl(_whileEnd.top()));
  } else {
    std::cerr << "Stop instruction can only be used inside a while loop." << std::endl;
  }
}

//---------------------------------------------------------------------------


void l22::postfix_writer::do_if_node(l22::if_node * const node, int lvl) {
  int lblEnd;
  node->condition()->accept(this, lvl);
  _pf.JZ(mklbl(lblEnd = ++_lbl));
  node->block()->accept(this, lvl + 2);
  _pf.LABEL(mklbl(lblEnd));
}

//---------------------------------------------------------------------------


void l22::postfix_writer::do_if_else_node(l22::if_else_node * const node, int lvl) {
  int lblElse, lblEnd;
  node->condition()->accept(this, lvl);
  _pf.JZ(mklbl(lblElse = lblEnd = ++_lbl));
  node->thenblock()->accept(this, lvl + 2);
  if (node->elseblock()) {
    _pf.JMP(mklbl(lblEnd = ++_lbl));
    _pf.LABEL(mklbl(lblElse));

    node->elseblock()->accept(this, lvl + 2);
  }
  _pf.LABEL(mklbl(lblEnd));
}
