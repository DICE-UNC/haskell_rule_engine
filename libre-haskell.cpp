#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <fstream>
#include <iostream>
#include <list>
#include <vector>

#include <boost/any.hpp>
#include <boost/range/irange.hpp>

#include <irods/irods_error.hpp>
#include <irods/irods_re_plugin.hpp>

#include "RE_stub.h"

void *re_state;

irods::error start(irods::default_re_ctx&) {

    int argc = 1;
    char *argv0[1] = {const_cast<char*>("dummy")};
    char **argv = argv0;
    hs_init(&argc, &argv);
	
    std::string rulebase("/etc/irods/Core");
    re_state = c_start(const_cast<char *>(rulebase.c_str()));
    
    return SUCCESS();
}

irods::error stop(irods::default_re_ctx&) {
    c_stop(re_state);
    hs_exit();
    return SUCCESS();
}

irods::error rule_exists(irods::default_re_ctx&, std::string fn, bool& b) {

    c_ruleExists(const_cast<char *>(fn.c_str()), &b);  
    return SUCCESS();
}

char *any_to_haskell(boost::any &a) {
  if(a.type() == typeid(std::string)) {
    return strdup(boost::any_cast<std::string>(a).c_str());
  } else if(a.type() == typeid(std::string *)) {
    return strdup(boost::any_cast<std::string*>(a)->c_str());
  } else {
     return NULL;
  }
}

void haskell_to_any_update(char *b, boost::any &a) {
  if(a.type() == typeid(std::string *)) {
	*(boost::any_cast<std::string *>(a)) = std::string(b );
  }
  free(b);
}

int c_callback(irods::callback *cb, char *c_rn, int c_nargs, char **c_ps) {
  std::list<boost::any> ps;
  std::vector<std::string> ps0;
  for(int i=0;i<c_nargs;i++) {
      ps0.push_back(std::string(c_ps[i]));
      ps.push_back(boost::any(&ps0[i]));
  }
  irods::error err = (*cb)(std::string(c_rn), ps);
  for(int i=0;i<c_nargs;i++) {
      free(c_ps[i]);
      c_ps[i] = strdup(ps0[i].c_str());
  }
  
  return err.code();
}
irods::error exec_rule(irods::default_re_ctx&, std::string fn, std::list<boost::any>& ps, irods::callback cb) {

    std::vector<char *> args;

    irods::foreach2 (ps, boost::irange(0, static_cast<int>(ps.size())), [&](boost::any& a, int b) {
        if((args[b] = any_to_haskell(a)) == NULL) {
	    return ERROR(-1, "cannot convert parameter");
	}
	
    });


    int errcode = c_execRule(re_state, const_cast<char *>(fn.c_str()), ps.size(), &args, &cb, reinterpret_cast<HsFunPtr>(&c_callback));
    
    if(errcode < 0) {
        return ERROR(errcode, "haskell re error");
    } else {
        irods::foreach2 (ps, boost::irange(0, static_cast<int>(ps.size())), [&](boost::any& a, int b) {
            haskell_to_any_update(args[b], a);
        });
	return SUCCESS();

    }

}

extern "C"

    irods::pluggable_rule_engine<irods::default_re_ctx>*
    plugin_factory(const std::string& _inst_name, const std::string& _context) {
        irods::pluggable_rule_engine<irods::default_re_ctx>* re=new irods::pluggable_rule_engine<irods::default_re_ctx>( _inst_name , _context);
        re->add_operation<irods::default_re_ctx&>(
            "start",
            std::function<irods::error(irods::default_re_ctx&)>(start));
        re->add_operation<irods::default_re_ctx&>(
            "stop",
            std::function<irods::error(irods::default_re_ctx&)>(stop));
        re->add_operation<irods::default_re_ctx&, std::string, bool&>(
            "rule_exists",
            std::function<irods::error(irods::default_re_ctx&, std::string, bool&)>(rule_exists));
        re->add_operation<irods::default_re_ctx&, std::string, std::list<boost::any>&, irods::callback>(
            "exec_rule",
            std::function<irods::error(irods::default_re_ctx&, std::string, std::list<boost::any>&, irods::callback)>(exec_rule));
        return re;
    }
