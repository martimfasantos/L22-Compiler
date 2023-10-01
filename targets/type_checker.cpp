#include <string>
#include "targets/type_checker.h"
#include ".auto/all_nodes.h"  // automatically generated
#include <cdk/types/primitive_type.h>
#include "l22_parser.tab.h"

#define ASSERT_UNSPEC { if (node->type() != nullptr && !node->is_typed(cdk::TYPE_UNSPEC)) return; }

//---------------------------------------------------------------------------

void l22::type_checker::do_sequence_node(cdk::sequence_node *const node, int lvl) {
  for(size_t i = 0; i < node->size(); i++) {
    node->node(i)->accept(this, lvl + 2);
  }
}

//---------------------------------------------------------------------------

void l22::type_checker::do_program_node(l22::program_node *const node, int lvl) {
  // push type of function to context
  std::vector<std::shared_ptr<cdk::basic_type>> *argsTypes = new std::vector<std::shared_ptr<cdk::basic_type>>();
  auto symbol = l22::make_symbol(0, cdk::functional_type::create(*argsTypes, cdk::primitive_type::create(4, cdk::TYPE_INT)), "_main", false, false);
  _funStack.push(symbol);

  
  node->statements()->accept(this, lvl);
}

void l22::type_checker::do_declaration_node(l22::declaration_node * const node , int lvl) {
  std::vector<std::shared_ptr<cdk::basic_type>> argsTypes;

  // var
  if (node->type() == nullptr) {
    if(node->initializer() == nullptr) {
      throw std::string("missing required initializer in var declaration");
    }
    else {
      node->initializer()->accept(this, lvl + 2);

      // if initializer is unspec, it can only be input
      if (node->initializer()->is_typed(cdk::TYPE_UNSPEC)) {
        node->initializer()->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
      }

      node->type(node->initializer()->type());
      

      if (node->type()->name() == cdk::TYPE_FUNCTIONAL) {
        std::shared_ptr<cdk::functional_type> fun_types = cdk::functional_type::cast(node->type());

        for (size_t ax = 0; ax < fun_types->input_length() ; ax++) {
          argsTypes.push_back(fun_types->input(ax));      
        }
      }

    }
  }
  else if (node->initializer() != nullptr) {
    // se tiver initializer
    node->initializer()->accept(this, lvl);

    if (node->initializer()->is_typed(cdk::TYPE_UNSPEC)){
      
      if(node->is_typed(cdk::TYPE_INT) || node->is_typed(cdk::TYPE_DOUBLE))
        node->initializer()->type(node->type());
      else
        throw std::string("Unable to read input.");
      
     
    }
    // int x = int
    else if (node->is_typed(cdk::TYPE_INT)) {
      if (!node->initializer()->is_typed(cdk::TYPE_INT))
        throw std::string("Wrong type for initializer (integer expected).");
    }
    // double x = double
    else if (node->is_typed(cdk::TYPE_DOUBLE)) {
      if (!node->initializer()->is_typed(cdk::TYPE_DOUBLE) && !node->initializer()->is_typed(cdk::TYPE_INT))
        throw std::string("Wrong type for initializer (integer or double expected).");
    }
    // text x = text
    else if (node->is_typed(cdk::TYPE_STRING)) {
      if (!node->initializer()->is_typed(cdk::TYPE_STRING))
        throw std::string("Wrong type for initializer (string expected).");
    }
    // pointer x = pointer
    else if (node->is_typed(cdk::TYPE_POINTER)) {
      if (!node->initializer()->is_typed(cdk::TYPE_POINTER))
        throw std::string("Wrong type for initializer (pointer expected).");
      l22::nullptr_node *n = dynamic_cast<l22::nullptr_node *>(node->initializer());
      if(n == nullptr) {
        typeOfPointer(cdk::reference_type::cast(node->type()), cdk::reference_type::cast(node->initializer()->type()));
      }
    }
    else if (node->is_typed(cdk::TYPE_FUNCTIONAL)) {
      if(!node->initializer()->is_typed(cdk::TYPE_FUNCTIONAL))
        throw std::string("Wrong type for initializer (function expected).");

      std::shared_ptr<cdk::functional_type> data_types = cdk::functional_type::cast(node->type());
      std::shared_ptr<cdk::functional_type> fun_types = cdk::functional_type::cast(node->initializer()->type());

      if (data_types->output(0) != fun_types->output(0)) {
        if (!(data_types->output(0)->name() == cdk::TYPE_DOUBLE && fun_types->output(0)->name() == cdk::TYPE_INT))
          throw std::string("Function declaration output does not match");
      }

      if (data_types->input_length() == fun_types->input_length()) {

        for (size_t ax = 0; ax < data_types->input_length() ; ax++) {
          argsTypes.push_back(fun_types->input(ax));
          // argument of exactly same type
          if (data_types->input(ax)->name() == fun_types->input(ax)->name()) continue;
          // int and double
          if (data_types->input(ax)->name() == cdk::TYPE_INT && fun_types->input(ax)->name() == cdk::TYPE_DOUBLE) continue;
          
          throw std::string("type mismatch for argument " + std::to_string(ax + 1) + ".");
        }
      }
      else {
        throw std::string(
            "number of arguments in data type (" + std::to_string(data_types->input_length()) + ") must match declaration ("
                + std::to_string(_funStack.top()->ftype()->input_length()) + ").");
      }

    }
    else {
      throw std::string("Unknown type for variable initializer.");
    }  
  }

  const std::string &id = node->identifier();
  std::shared_ptr<l22::symbol> symbol;

  if (node->is_typed(cdk::TYPE_FUNCTIONAL) && !node->initializer()) {
    symbol = l22::make_symbol(node->qualifier(), node->type(), id, (bool)node->initializer(), true);
  }
  else {
    symbol = l22::make_symbol(node->qualifier(), node->type(), id, (bool)node->initializer(), false);
    
  }

  if(node->is_typed(cdk::TYPE_FUNCTIONAL) && node->initializer()) {

    symbol->set_argument_types(argsTypes);
    std::shared_ptr<l22::symbol> previous = _symtab.find(id);
    if (previous) {

      if (previous->forward() && (((previous->qualifier() == tPUBLIC && node->qualifier() == tPUBLIC)
        || (previous->qualifier() == tPRIVATE && node->qualifier() == tPRIVATE)) || previous->qualifier() == tUSE)) {
        _symtab.replace(id, symbol);
        _parent->set_new_symbol(symbol);
      } else {
        throw std::string("conflicting definition for '" + id + "'");
      }
      return;
    }
  }

  if  (_symtab.insert(id, symbol)) {
    _parent->set_new_symbol(symbol);  // advise parent that a symbol has been inserted
  } else {
    throw std::string("variable '" + id + "' redeclared");
  }

}

void l22::type_checker::do_function_call_node(l22::function_call_node * const node, int lvl) {
  ASSERT_UNSPEC;
  
  node->fpointer()->accept(this, lvl);

  // pointer to function or @
  if (node->fpointer()->is_typed(cdk::TYPE_FUNCTIONAL)) {
    std::shared_ptr<cdk::functional_type> ptype = cdk::functional_type::cast(node->fpointer()->type());
    node->arguments()->accept(this, lvl);

    if (node->arguments()->size() == ptype->input_length()) {
      for (size_t ax = 0; ax < node->arguments()->size(); ax++) {
        // argument of exactly same type
        if (node->argument(ax)->is_typed(ptype->input(ax)->name())) continue;
        // int and double
        if (node->argument(ax)->is_typed(cdk::TYPE_INT) && (ptype->input(ax)->name() == cdk::TYPE_DOUBLE)) continue;
        
        throw std::string("type mismatch for argument " + std::to_string(ax + 1) + " of function call.");
      }
    } else {
      throw std::string(
          "number of arguments in call (" + std::to_string(node->arguments()->size()) + ") must match declaration ("
              + std::to_string(_funStack.top()->ftype()->input_length()) + ").");
    }

    node->type(ptype->output(0));

  }
  else {
    throw std::string("function call with invalid function pointer");
  }
}

void l22::type_checker::do_function_definition_node(l22::function_definition_node * const node , int lvl) {
  ASSERT_UNSPEC;
  _symtab.push();
  

  
  std::vector<std::shared_ptr<cdk::basic_type>> *argsTypes = new std::vector<std::shared_ptr<cdk::basic_type>>();
  if (node->arguments()) {
      for(size_t i = 0; i < node->arguments()->size(); i++) {
        l22::declaration_node *arg = dynamic_cast<l22::declaration_node *>(node->arguments()->node(i));
        if(arg != nullptr){
          node->argument(i)->accept(this, lvl);
          argsTypes->push_back(arg->type());
        }
      }
  }

  node->type(cdk::functional_type::create(*argsTypes, node->output_type()));

  auto symbol = l22::make_symbol(0, node->type(), "temp", false, false);

  _funStack.push(symbol);

  node->block()->accept(this, lvl);

  _symtab.pop();

}

//---------------------------------------------------------------------------
void l22::type_checker::do_variable_node(cdk::variable_node *const node, int lvl) {
  ASSERT_UNSPEC;
  const std::string &id = node->name();


  if (id == "@") {
      if (_funStack.size() == 0 || _funStack.top()->name() == "_main") {
        throw std::string("Recursive call outside function");
      }
      node->type(_funStack.top()->ftype());
  }
  else {
    std::shared_ptr<l22::symbol> symbol = _symtab.find(id);

    if (symbol != nullptr) {
      node->type(symbol->type());
    } else {
      throw std::string("variable " + id + " not in symbol table");
    }
  }
  
}

void l22::type_checker::do_rvalue_node(cdk::rvalue_node *const node, int lvl) {
  ASSERT_UNSPEC;
  node->lvalue()->accept(this, lvl);
  node->type(node->lvalue()->type());
  

}

//---------------------------------------------------------------------------

void l22::type_checker::do_evaluation_node(l22::evaluation_node *const node, int lvl) {
  node->argument()->accept(this, lvl + 2);

  if(node->argument()->is_typed(cdk::TYPE_UNSPEC)) {
      node->argument()->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
  }

}

void l22::type_checker::do_print_node(l22::print_node *const node, int lvl) {
  node->arguments()->accept(this, lvl + 2);
}

//---------------------------------------------------------------------------
void l22::type_checker::do_read_node(l22::read_node *const node, int lvl) {
  ASSERT_UNSPEC;
  node->type(cdk::primitive_type::create(0, cdk::TYPE_UNSPEC));
}

//---------------------------------------------------------------------------

void l22::type_checker::do_while_node(l22::while_node *const node, int lvl) {
  node->condition()->accept(this, lvl + 4);
  if (!node->condition()->is_typed(cdk::TYPE_INT)) throw std::string("expected integer condition");
  
  node->block()->accept(this, lvl + 2);
}

void l22::type_checker::do_again_node(l22::again_node * const node, int lvl) {
	// EMPTY
}

void l22::type_checker::do_stop_node(l22::stop_node * const node, int lvl) {
	// EMPTY
}

//---------------------------------------------------------------------------

void l22::type_checker::do_if_node(l22::if_node *const node, int lvl) {
  node->condition()->accept(this, lvl + 4);
  if (!node->condition()->is_typed(cdk::TYPE_INT)) throw std::string("expected integer condition");
  
  node->block()->accept(this, lvl);

}

void l22::type_checker::do_if_else_node(l22::if_else_node *const node, int lvl) {
  node->condition()->accept(this, lvl + 4);
  if (!node->condition()->is_typed(cdk::TYPE_INT)) throw std::string("expected integer condition");
  
  node->elseblock()->accept(this, lvl);

}

//---------------------------------------------------------------------------

void l22::type_checker::do_nil_node(cdk::nil_node *const node, int lvl) {  
  // EMPTY OK
}

void l22::type_checker::do_data_node(cdk::data_node *const node, int lvl) {
  // EMPTY
}

void l22::type_checker::do_double_node(cdk::double_node *const node, int lvl) {
  ASSERT_UNSPEC;
  node->type(cdk::primitive_type::create(8, cdk::TYPE_DOUBLE));
}

void l22::type_checker::do_not_node(cdk::not_node *const node, int lvl) {
  ASSERT_UNSPEC;
  node->argument()->accept(this, lvl);
  if(node->argument()->is_typed(cdk::TYPE_UNSPEC)) {
    node->argument()->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
  }
  else if (!node->argument()->is_typed(cdk::TYPE_INT)) {
    throw std::string("integer expression expected in binary operator (left)");
  }

  node->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
}

void l22::type_checker::do_and_node(cdk::and_node *const node, int lvl) {
  processBooleanLogicExpression(node, lvl);
}

void l22::type_checker::do_or_node(cdk::or_node *const node, int lvl) {
  processBooleanLogicExpression(node, lvl);
}

//---------------------------------------------------------------------------

void l22::type_checker::do_index_node(l22::index_node * const node, int lvl) {
  ASSERT_UNSPEC;

  node->base()->accept(this, lvl + 2);
  if (!node->base()->is_typed(cdk::TYPE_POINTER)) throw std::string("pointer expression expected in index left-value");
  
 
  std::shared_ptr<cdk::basic_type> reference_type = cdk::reference_type::cast(node->base()->type())->referenced();
  // se o que está a ser referenciado for do tipo functional dá erro
  if (reference_type->name() == cdk::TYPE_FUNCTIONAL) throw std::string("pointer to funcion can not be indexed");

  node->index()->accept(this, lvl + 2);
  if(node->index()->is_typed(cdk::TYPE_UNSPEC)) {
    node->index()->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
  }
  else if (!node->index()->is_typed(cdk::TYPE_INT))
    throw std::string("Integer expression expected in left-value index.");

  


  node->type(reference_type);
  

}

void l22::type_checker::do_stack_alloc_node(l22::stack_alloc_node * const node, int lvl) {
  ASSERT_UNSPEC;
  node->argument()->accept(this, lvl + 2);
  if(node->argument()->is_typed(cdk::TYPE_UNSPEC)) {
    node->argument()->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
  }
  else if (!node->argument()->is_typed(cdk::TYPE_INT))
    throw std::string("Integer expression expected in allocation expression.");

  node->type(cdk::reference_type::create(4, cdk::primitive_type::create(8, cdk::TYPE_DOUBLE)));

}

void l22::type_checker::do_address_of_node(l22::address_of_node * const node, int lvl) {
  ASSERT_UNSPEC;
  node->lvalue()->accept(this, lvl + 2);
  if (!node->lvalue()->is_typed(cdk::TYPE_UNSPEC) && !node->lvalue()->is_typed(cdk::TYPE_VOID))
    node->type(cdk::reference_type::create(4, node->lvalue()->type()));
  else
    throw std::string("Wrong type in unary logical expression.");
}

void l22::type_checker::do_nullptr_node(l22::nullptr_node * const node, int lvl) {
	ASSERT_UNSPEC;
  node->type(cdk::reference_type::create(4, nullptr));
}

void l22::type_checker::do_sizeof_node(l22::sizeof_node * const node, int lvl) {
  ASSERT_UNSPEC;
  node->expression()->accept(this, lvl + 2);
  node->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
}

//---------------------------------------------------------------------------

void l22::type_checker::do_block_node(l22::block_node * const node, int lvl) {
  if (node->declarations()) {
    node->declarations()->accept(this, lvl);
  }
  if (node->instructions()) {
    node->instructions()->accept(this, lvl);
  }
}

void l22::type_checker::do_return_node(l22::return_node * const node, int lvl) {
	if (node->retval()) {
    
    if (_funStack.size() == 0) {
      throw std::string("return outside function.");
    }
    if (_funStack.top()->ftype()->output(0)->name() == cdk::TYPE_VOID)
      throw std::string("Void function cannot return values.");

    node->retval()->accept(this, lvl + 2);

    std::shared_ptr<cdk::functional_type> out_type = cdk::functional_type::cast(node->retval()->type());
    if (_funStack.top()->ftype()->output(0)->name() == cdk::TYPE_INT) {
      if (!node->retval()->is_typed(cdk::TYPE_INT) && (out_type != nullptr && out_type->output(0)->name() != cdk::TYPE_INT))
        throw std::string("Wrong type for return (integer expected).");
    } else if (_funStack.top()->ftype()->output(0)->name() == cdk::TYPE_DOUBLE) {
      if (!node->retval()->is_typed(cdk::TYPE_INT) && !node->retval()->is_typed(cdk::TYPE_DOUBLE) 
        && (out_type != nullptr && out_type->output(0)->name() != cdk::TYPE_INT && out_type->output(0)->name() != cdk::TYPE_DOUBLE))
        throw std::string("Wrong type for return (integer or double expected).");
    } else if (_funStack.top()->ftype()->output(0)->name() == cdk::TYPE_STRING) {
      if (!node->retval()->is_typed(cdk::TYPE_STRING) && (out_type != nullptr && out_type->output(0)->name() != cdk::TYPE_STRING))
        throw std::string("Wrong type for return (string expected).");
    } else if (_funStack.top()->ftype()->output(0)->name() == cdk::TYPE_POINTER) {
        typeOfPointer(cdk::reference_type::cast(node->retval()->type()), cdk::reference_type::cast(_funStack.top()->ftype()->output(0)));
    } else if (_funStack.top()->ftype()->output(0)->name() == cdk::TYPE_FUNCTIONAL) {
      if(!node->retval()->is_typed(cdk::TYPE_FUNCTIONAL))
        throw std::string("Wrong type for return (function expected).");

      std::shared_ptr<cdk::functional_type> return_type = cdk::functional_type::cast(node->retval()->type());

      if (_funStack.top()->ftype()->output(0) != return_type->output(0)) {
        throw std::string("Function declaration output does not match");
      }

      if (_funStack.top()->ftype()->input_length() == return_type->input_length()) {
        for (size_t ax = 0; ax < _funStack.top()->ftype()->input_length() ; ax++) {
          // argument of exactly same type
          if (_funStack.top()->ftype()->input(ax)->name() == return_type->input(ax)->name()) continue;
          // int and double
          if (_funStack.top()->ftype()->input(ax)->name() == cdk::TYPE_DOUBLE && return_type->input(ax)->name() == cdk::TYPE_INT) continue;
          throw std::string("type mismatch for argument " + std::to_string(ax + 1) + ".");
        }
      } else {
        throw std::string(
            "number of arguments in return (" + std::to_string(return_type->input_length()) + ") must match the definition ("
                + std::to_string(_funStack.top()->ftype()->input_length()) + ").");
      }
    }
    else {
      throw std::string("Unknown type for return expression.");
    }
  }
}

//---------------------------------------------------------------------------

void l22::type_checker::do_integer_node(cdk::integer_node *const node, int lvl) {
  ASSERT_UNSPEC;
  node->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
}

void l22::type_checker::do_string_node(cdk::string_node *const node, int lvl) {
  ASSERT_UNSPEC;
  node->type(cdk::primitive_type::create(4, cdk::TYPE_STRING));
}


/* --------------------------------------------------------------------------- */
/* ----|                      UNARY EXPRESSIONS                          |---- */
/* --------------------------------------------------------------------------- */

void l22::type_checker::do_identity_node(l22::identity_node * const node, int lvl) {
  processUnaryExpression(node, lvl);
}

void l22::type_checker::do_neg_node(cdk::neg_node *const node, int lvl)  {
  processUnaryExpression(node, lvl);
}

/* --------------------------------------------------------------------------- */
/* ----|                      BINARY EXPRESSIONS                         |---- */
/* --------------------------------------------------------------------------- */

void l22::type_checker::do_mul_node(cdk::mul_node *const node, int lvl) {
  processIDExpression(node, lvl);
}

void l22::type_checker::do_div_node(cdk::div_node *const node, int lvl) {
  processIDExpression(node, lvl);
}

void l22::type_checker::do_mod_node(cdk::mod_node *const node, int lvl) {
  processIntOnlyExpression(node, lvl);
}

//---------------------------------------------------------------------------
void l22::type_checker::do_add_node(cdk::add_node *const node, int lvl) {
  ASSERT_UNSPEC;
  std::shared_ptr<cdk::basic_type> leftptr;
  std::shared_ptr<cdk::basic_type> rightptr;
  node->left()->accept(this, lvl + 2);
  node->right()->accept(this, lvl + 2);

  if (node->left()->is_typed(cdk::TYPE_DOUBLE) && node->right()->is_typed(cdk::TYPE_DOUBLE))
    node->type(cdk::primitive_type::create(8, cdk::TYPE_DOUBLE));
  else if (node->left()->is_typed(cdk::TYPE_DOUBLE) && node->right()->is_typed(cdk::TYPE_INT))
    node->type(cdk::primitive_type::create(8, cdk::TYPE_DOUBLE));
  else if (node->left()->is_typed(cdk::TYPE_INT) && node->right()->is_typed(cdk::TYPE_DOUBLE))
    node->type(cdk::primitive_type::create(8, cdk::TYPE_DOUBLE));
  else if (node->left()->is_typed(cdk::TYPE_INT) && node->right()->is_typed(cdk::TYPE_INT))
    node->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
  else if (node->left()->is_typed(cdk::TYPE_POINTER) && node->right()->is_typed(cdk::TYPE_INT)) {
    
      leftptr = cdk::reference_type::cast(node->left()->type())->referenced();
      if(leftptr->name() == cdk::TYPE_FUNCTIONAL) {
        throw std::string("Pointer to function does not support pointer arithmetics");
      }
    node->type(node->left()->type());
  }
  else if (node->left()->is_typed(cdk::TYPE_INT) && node->right()->is_typed(cdk::TYPE_POINTER)) {
      rightptr = cdk::reference_type::cast(node->right()->type())->referenced();
      if(rightptr->name() == cdk::TYPE_FUNCTIONAL) {
        throw std::string("Pointer to function does not support pointer arithmetics");
      }
    node->type(node->right()->type());
  }
  else if(node->left()->is_typed(cdk::TYPE_UNSPEC) && node->right()->is_typed(cdk::TYPE_UNSPEC)) {
    node->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
    node->left()->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
    node->right()->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
  }
  else if(node->left()->is_typed(cdk::TYPE_UNSPEC)) {
    if(node->right()->is_typed(cdk::TYPE_DOUBLE) || node->right()->is_typed(cdk::TYPE_INT))
        node->left()->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
    else
      throw std::string("Invalid expression in right argument of binary expression.");
    
    node->type(node->right()->type());

  }
  else if(node->right()->is_typed(cdk::TYPE_UNSPEC)) {
    if(node->left()->is_typed(cdk::TYPE_DOUBLE) || node->left()->is_typed(cdk::TYPE_INT))
        node->right()->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
    else
      throw std::string("Invalid expression in right argument of binary expression.");
    
    node->type(node->left()->type());
  }
  else {
    throw std::string("wrong types in binary expression");
  }
  

}

void l22::type_checker::do_sub_node(cdk::sub_node *const node, int lvl) {

  ASSERT_UNSPEC;
  std::shared_ptr<cdk::basic_type> leftptr;
  std::shared_ptr<cdk::basic_type> rightptr;
  node->left()->accept(this, lvl + 2);
  node->right()->accept(this, lvl + 2);

  if (node->left()->is_typed(cdk::TYPE_DOUBLE) && node->right()->is_typed(cdk::TYPE_DOUBLE))
    node->type(cdk::primitive_type::create(8, cdk::TYPE_DOUBLE));
  else if (node->left()->is_typed(cdk::TYPE_DOUBLE) && node->right()->is_typed(cdk::TYPE_INT))
    node->type(cdk::primitive_type::create(8, cdk::TYPE_DOUBLE));
  else if (node->left()->is_typed(cdk::TYPE_INT) && node->right()->is_typed(cdk::TYPE_DOUBLE))
    node->type(cdk::primitive_type::create(8, cdk::TYPE_DOUBLE));
  else if (node->left()->is_typed(cdk::TYPE_INT) && node->right()->is_typed(cdk::TYPE_INT))
    node->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
  else if (node->left()->is_typed(cdk::TYPE_POINTER) && node->right()->is_typed(cdk::TYPE_INT)) {
      leftptr = cdk::reference_type::cast(node->left()->type())->referenced();
      if(leftptr->name() == cdk::TYPE_FUNCTIONAL) {
        throw std::string("Pointer to function does not support pointer arithmetics");
      }
    node->type(node->left()->type());
  }
  else if (node->left()->is_typed(cdk::TYPE_POINTER) && node->right()->is_typed(cdk::TYPE_POINTER)) {
    leftptr = cdk::reference_type::cast(node->left()->type())->referenced();
    if(leftptr->name() == cdk::TYPE_FUNCTIONAL) {
      throw std::string("Pointer to function does not support pointer arithmetics");
    }
    rightptr = cdk::reference_type::cast(node->right()->type())->referenced();
    if(rightptr->name() == cdk::TYPE_FUNCTIONAL) {
      throw std::string("Pointer to function does not support pointer arithmetics");
    }
    typeOfPointer(cdk::reference_type::cast(node->left()->type()),cdk::reference_type::cast(node->right()->type()));
    node->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
  }
  else if(node->left()->is_typed(cdk::TYPE_UNSPEC) && node->right()->is_typed(cdk::TYPE_UNSPEC)) {
    node->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
    node->left()->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
    node->right()->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
  }
  else if(node->left()->is_typed(cdk::TYPE_UNSPEC)) {
    if(node->right()->is_typed(cdk::TYPE_DOUBLE) || node->right()->is_typed(cdk::TYPE_INT))
        node->left()->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
    else
      throw std::string("Invalid expression in right argument of binary expression.");
    
    node->type(node->right()->type());

  }
  else if(node->right()->is_typed(cdk::TYPE_UNSPEC)) {
    if(node->left()->is_typed(cdk::TYPE_DOUBLE) || node->left()->is_typed(cdk::TYPE_INT))
        node->right()->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
    else
      throw std::string("Invalid expression in right argument of binary expression.");
    
    node->type(node->left()->type());
  }
  else {
    throw std::string("wrong types in binary expression");
  }
  

}

//---------------------------------------------------------------------------

void l22::type_checker::do_lt_node(cdk::lt_node *const node, int lvl) {
  processScalarLogicalExpression(node, lvl);
}

void l22::type_checker::do_gt_node(cdk::gt_node *const node, int lvl) {
  processScalarLogicalExpression(node, lvl);
}

void l22::type_checker::do_le_node(cdk::le_node *const node, int lvl) {
  processScalarLogicalExpression(node, lvl);
}

void l22::type_checker::do_ge_node(cdk::ge_node *const node, int lvl) {
  processScalarLogicalExpression(node, lvl);
}

//---------------------------------------------------------------------------

void l22::type_checker::do_ne_node(cdk::ne_node *const node, int lvl) {
  processGeneralLogicalExpression(node, lvl);
}

void l22::type_checker::do_eq_node(cdk::eq_node *const node, int lvl) {
  processGeneralLogicalExpression(node, lvl);
}

//---------------------------------------------------------------------------

void l22::type_checker::do_assignment_node(cdk::assignment_node *const node, int lvl) {
  ASSERT_UNSPEC;

  node->lvalue()->accept(this, lvl + 4);
  node->rvalue()->accept(this, lvl + 4);

  if(node->lvalue()->is_typed(cdk::TYPE_UNSPEC))
    throw std::string("Left value must have a type.");

  if(node->rvalue()->is_typed(cdk::TYPE_UNSPEC)) {

    if(node->lvalue()->is_typed(cdk::TYPE_INT) || node->lvalue()->is_typed(cdk::TYPE_DOUBLE) || node->lvalue()->is_typed(cdk::TYPE_STRING)){
      node->rvalue()->type(node->lvalue()->type());
    }
    else
      throw std::string("Unknown node with unspecified type");
  }

  if(node->lvalue()->is_typed(cdk::TYPE_INT) && node->rvalue()->is_typed(cdk::TYPE_INT)) {
    node->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
  }
  else if(node->lvalue()->is_typed(cdk::TYPE_DOUBLE) && (node->rvalue()->is_typed(cdk::TYPE_DOUBLE) || node->rvalue()->is_typed(cdk::TYPE_INT))) {
    node->type(cdk::primitive_type::create(8, cdk::TYPE_DOUBLE));
  }
  else if(node->lvalue()->is_typed(cdk::TYPE_STRING) && node->rvalue()->is_typed(cdk::TYPE_STRING)) {
    node->type(cdk::primitive_type::create(4, cdk::TYPE_STRING));
  }
  else if(node->lvalue()->is_typed(cdk::TYPE_POINTER) && node->rvalue()->is_typed(cdk::TYPE_POINTER)) {
    l22::nullptr_node *n = dynamic_cast<l22::nullptr_node *>(node->rvalue());
    if(n == nullptr)
      typeOfPointer(cdk::reference_type::cast(node->lvalue()->type()), cdk::reference_type::cast(node->rvalue()->type()));
    node->type(node->lvalue()->type());
  }
  else if (node->lvalue()->is_typed(cdk::TYPE_FUNCTIONAL) && node->rvalue()->is_typed(cdk::TYPE_FUNCTIONAL)){
    std::shared_ptr<cdk::functional_type> left = cdk::functional_type::cast(node->lvalue()->type());
    std::shared_ptr<cdk::functional_type> right = cdk::functional_type::cast(node->rvalue()->type());
    
    if (left->output(0) != right->output(0)) {
      if (!(left->output(0)->name() == cdk::TYPE_DOUBLE && right->output(0)->name() == cdk::TYPE_INT))
        throw std::string("Function declaration output does not match");
    }

    if (left->input_length() == right->input_length()) {
      for (size_t ax = 0; ax < left->input_length() ; ax++) {
        // argument of exactly same type
        if (left->input(ax)->name() == right->input(ax)->name()) continue;
        // int and double
        if (left->input(ax)->name() == cdk::TYPE_INT && right->input(ax)->name() == cdk::TYPE_DOUBLE) continue;
        
        throw std::string("type mismatch for argument " + std::to_string(ax + 1) + ".");
      }
    } else {
      throw std::string(
          "number of arguments in data type (" + std::to_string(left->input_length()) + ") must match declaration ("
              + std::to_string(_funStack.top()->ftype()->input_length()) + ").");
    }
  }
  else {
    throw std::string("wrong types in assignment");
  }
  
}

//---------------------------------------------------------------------------


/* --------------------------------------------------------------------------- */
/* ----|                          PROTECTED                              |---- */
/* --------------------------------------------------------------------------- */

// lt, le, gt, ge
void l22::type_checker::processScalarLogicalExpression(cdk::binary_operation_node *const node, int lvl) {
  ASSERT_UNSPEC;
  node->left()->accept(this, lvl + 2);
  if(node->left()->is_typed(cdk::TYPE_UNSPEC)) {
    node->left()->type(cdk::primitive_type::create(4, cdk::TYPE_DOUBLE));
  }
  else if (!node->left()->is_typed(cdk::TYPE_INT) && !node->left()->is_typed(cdk::TYPE_DOUBLE)) {
    throw std::string("integer expression expected in binary logical expression (left)");
  }
  
  node->right()->accept(this, lvl + 2);
  if(node->right()->is_typed(cdk::TYPE_UNSPEC)) {
    node->right()->type(cdk::primitive_type::create(4, cdk::TYPE_DOUBLE));
  }
  else if (!node->right()->is_typed(cdk::TYPE_INT) && !node->right()->is_typed(cdk::TYPE_DOUBLE)) {
    throw std::string("integer expression expected in binary logical expression (left)");
  }

  node->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
}

// and or
void l22::type_checker::processBooleanLogicExpression(cdk::binary_operation_node *const node, int lvl) {
  ASSERT_UNSPEC;

  node->left()->accept(this, lvl + 2);
  if (node->left()->is_typed(cdk::TYPE_UNSPEC)) {
    node->left()->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
  }
  else if (!node->left()->is_typed(cdk::TYPE_INT)) {
    throw std::string("wrong type in left argument of binary expression");
  }

  node->right()->accept(this, lvl + 2);
  if (node->right()->is_typed(cdk::TYPE_UNSPEC)) {
    node->right()->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
  }
  else if (!node->right()->is_typed(cdk::TYPE_INT)) {
    throw std::string("wrong type in right argument of binary expression");
  }

  node->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
}

// identity e neg
void l22::type_checker::processUnaryExpression(cdk::unary_operation_node *const node, int lvl) {
  ASSERT_UNSPEC;
  node->argument()->accept(this, lvl);

  if (node->argument()->is_typed(cdk::TYPE_INT))
    node->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
  else if (node->argument()->is_typed(cdk::TYPE_DOUBLE))
    node->type(cdk::primitive_type::create(8, cdk::TYPE_DOUBLE));
  else if (node->argument()->is_typed(cdk::TYPE_UNSPEC)) {
    node->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
    node->argument()->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
  }
  else
    throw std::string("Wrong type in argument of unary expression (Integer or double expected).");
}

// ne, eq
void l22::type_checker::processGeneralLogicalExpression(cdk::binary_operation_node *const node, int lvl) {
  ASSERT_UNSPEC;
  // type unspec
  node->left()->accept(this, lvl + 2);
  if(node->left()->is_typed(cdk::TYPE_UNSPEC)) {
    node->left()->type(cdk::primitive_type::create(4, cdk::TYPE_DOUBLE));
  }

  node->right()->accept(this, lvl + 2);
  if(node->right()->is_typed(cdk::TYPE_UNSPEC)) {
    node->right()->type(cdk::primitive_type::create(4  , cdk::TYPE_DOUBLE));
  }

  // pointers -> têm de apontar para o mesmo tipo
  if(node->left()->is_typed(cdk::TYPE_POINTER) && node->right()->is_typed(cdk::TYPE_POINTER)) {
    typeOfPointer(cdk::reference_type::cast(node->left()->type()),cdk::reference_type::cast(node->right()->type()));
  }
  // diferentes, pode ser double == int ou int == double
  else if(node->left()->type()->name() != node->right()->type()->name()) {
    if(!((node->left()->is_typed(cdk::TYPE_DOUBLE) && node->right()->is_typed(cdk::TYPE_INT))|| (node->left()->is_typed(cdk::TYPE_INT)  &&  node->right()->is_typed(cdk::TYPE_DOUBLE))))
        throw std::string("Operator has incompatible types.");
  }
  node->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
}

// * /
void l22::type_checker::processIDExpression(cdk::binary_operation_node *const node, int lvl) {
  ASSERT_UNSPEC;
  node->left()->accept(this, lvl + 2);
  node->right()->accept(this, lvl + 2);

  if (node->left()->is_typed(cdk::TYPE_DOUBLE) && node->right()->is_typed(cdk::TYPE_DOUBLE))
    node->type(cdk::primitive_type::create(8, cdk::TYPE_DOUBLE));
  else if (node->left()->is_typed(cdk::TYPE_DOUBLE) && node->right()->is_typed(cdk::TYPE_INT))
    node->type(cdk::primitive_type::create(8, cdk::TYPE_DOUBLE));
  else if (node->left()->is_typed(cdk::TYPE_INT) && node->right()->is_typed(cdk::TYPE_DOUBLE))
    node->type(cdk::primitive_type::create(8, cdk::TYPE_DOUBLE));
  else if (node->left()->is_typed(cdk::TYPE_INT) && node->right()->is_typed(cdk::TYPE_INT))
    node->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
  else if (node->left()->is_typed(cdk::TYPE_UNSPEC) && node->right()->is_typed(cdk::TYPE_UNSPEC)) {
    node->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
    node->left()->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
    node->right()->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
  }
  else if(node->left()->is_typed(cdk::TYPE_UNSPEC)) {
    node->type(node->right()->type());
    node->left()->type(cdk::primitive_type::create(4, cdk::TYPE_DOUBLE));
  }
  else if(node->right()->is_typed(cdk::TYPE_UNSPEC)) {
    node->type(node->left()->type());
    node->right()->type(cdk::primitive_type::create(4, cdk::TYPE_DOUBLE));
  }
  else
    throw std::string("Wrong types in binary expression.");
}

// para o %
void l22::type_checker::processIntOnlyExpression(cdk::binary_operation_node *const node, int lvl) {
  ASSERT_UNSPEC;
  node->left()->accept(this, lvl);
  if(node->left()->is_typed(cdk::TYPE_UNSPEC)) {
    node->left()->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
  }
  else if (!node->left()->is_typed(cdk::TYPE_INT)) {
    throw std::string("integer expression expected in binary operator (left)");
  }

  node->right()->accept(this, lvl);
  if(node->right()->is_typed(cdk::TYPE_UNSPEC)) {
    node->right()->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
  }
  else if (!node->right()->is_typed(cdk::TYPE_INT)) {
    throw std::string("integer expression expected in binary operator (right)");
  }

  node->type(cdk::primitive_type::create(4, cdk::TYPE_INT));

}

std::shared_ptr<cdk::basic_type> l22::type_checker::typeOfPointer(std::shared_ptr<cdk::reference_type> leftptr, std::shared_ptr<cdk::reference_type> rightptr){
    std::shared_ptr<cdk::basic_type> left = leftptr, right = rightptr;
    // corre até que o que está a ser referenciado seja algo de um tipo != TYPE_POINTER
    while (left->name() == cdk::TYPE_POINTER && right->name() == cdk::TYPE_POINTER) {
      left = cdk::reference_type::cast(left)->referenced();
      right = cdk::reference_type::cast(right)->referenced();
    }
    // se há um que já não é pointer e um que ainda é, então não apontam ambos para o mesmo
    if (left->name() == cdk::TYPE_POINTER || right->name() == cdk::TYPE_POINTER)
      throw std::string("Wrong pointer type.");
    // ambos apontam para inteiro
    if (left->name() == cdk::TYPE_INT && right->name() == cdk::TYPE_INT)
      return cdk::primitive_type::create(4, cdk::TYPE_INT);
    // ambos apontam para double
    else if (left->name() == cdk::TYPE_DOUBLE && right->name() == cdk::TYPE_DOUBLE)
      return cdk::primitive_type::create(8, cdk::TYPE_DOUBLE);
    // ambos apontam para string
    else if (left->name() == cdk::TYPE_STRING && right->name() == cdk::TYPE_STRING)
      return cdk::primitive_type::create(4, cdk::TYPE_STRING);
    // ambos apontam para função
    else if (left->name() == cdk::TYPE_FUNCTIONAL && right->name() == cdk::TYPE_FUNCTIONAL)
      return cdk::primitive_type::create(4, cdk::TYPE_FUNCTIONAL);
    else // FIXME AUTO
      throw std::string("Wrong pointer type.");
}