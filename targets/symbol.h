#ifndef __L22_TARGETS_SYMBOL_H__
#define __L22_TARGETS_SYMBOL_H__

#include <string>
#include <memory>
#include <cdk/types/basic_type.h>
#include <cdk/types/functional_type.h>


namespace l22 {

  class symbol {

    std::string _name; // identifier
    long _value; // hack!
    int _qualifier;
    std::shared_ptr<cdk::basic_type> _type;
    std::vector<std::shared_ptr<cdk::basic_type>> _argument_types;
    bool _initialized; // initialized?
    int _offset = 0;
    std::string _label = "";
    bool _forward = false;


  public:
    symbol(int qualifier, std::shared_ptr<cdk::basic_type> type, const std::string &name, bool initialized, bool forward = false) :
        _name(name), _value(0), _qualifier(qualifier), _type(type), _initialized(initialized), _forward(forward) {
    }

    virtual ~symbol() {
      // EMPTY
    }

    std::shared_ptr<cdk::basic_type> type() const {
      return _type;
    }

    std::shared_ptr<cdk::functional_type> ftype() const {
      std::shared_ptr<cdk::functional_type> _ftype = cdk::functional_type::cast(_type);
      return _ftype;
    }
    
    void set_type(std::shared_ptr<cdk::basic_type> t) {
      _type = t;
    }

    bool is_typed(cdk::typename_type name) const {
      return _type->name() == name;
    }

    const std::string &name() const {
      return _name;
    }

    const std::string &label() const {
      return _label;
    }

    void set_label(const std::string label) {
      _label = label;
    }

    bool initialized() const {
      return _initialized;
    }

    bool global() const {
      return _offset == 0;
    }

    bool forward() const {
      return _forward;
    }

    long value() const {
      return _value;
    }
    long value(long v) {
      return _value = v;
    }

    int qualifier() const {
      return _qualifier;
    }    

    int offset() const {
      return _offset;
    }
    void set_offset(int offset) {
      _offset = offset;
    }

    void set_argument_types(const std::vector<std::shared_ptr<cdk::basic_type>> &types) {
      _argument_types = types;
    }

    bool argument_is_typed(size_t ax, cdk::typename_type name) const {
      return _argument_types[ax]->name() == name;
    }

    std::shared_ptr<cdk::basic_type> argument_type(size_t ax) const {
      return _argument_types[ax];
    }

    size_t argument_size(size_t ax) const {
      return _argument_types[ax]->size();
    }

    size_t number_of_arguments() const {
      return _argument_types.size();
    }

  };

  inline auto make_symbol(int qualifier, std::shared_ptr<cdk::basic_type> type, const std::string &name, bool initialized, bool forward = false) {
    return std::make_shared<symbol>(qualifier, type, name, initialized, forward);
  }

} // l22

#endif
