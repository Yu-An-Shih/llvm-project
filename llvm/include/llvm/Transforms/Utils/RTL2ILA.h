#ifndef LLVM_TRANSFORMS_RTL2ILA_H
#define LLVM_TRANSFORMS_RTL2ILA_H

#include "llvm/IR/PassManager.h"

#include <vector>
#include <unordered_map>

namespace llvm {

struct BitStr {
  std::string tri_str;

  // Operations
  BitStr trunc_to(int);
  BitStr zext_to(int);
  BitStr operator&(const BitStr &);
  BitStr operator|(const BitStr &);
  BitStr lshr(int);

  bool isConstVal() { return tri_str.find('x') == std::string::npos && tri_str.find('z') == std::string::npos; }
  int getConstVal() { 
    assert( isConstVal() );
    return std::stoi( std::string(tri_str.rbegin(), tri_str.rend()), 0, 2 );
  }

  friend raw_ostream& operator<<(raw_ostream& os, const BitStr& bs) {
    for (auto it = bs.tri_str.rbegin(); it !=bs.tri_str.rend(); ++it)
      os << *it;
    //os << bs.tri_str;
    return os;
  }
};

struct DependencyContainer {
  int pos_l, pos_h;
  
  Value *var_ptr;
  int var_lsb, var_hsb;

  DependencyContainer() {}
  DependencyContainer(int pl, int ph, Value *v, int vl, int vh)
    :pos_l(pl), pos_h(ph), var_ptr(v), var_lsb(vl), var_hsb(vh) {}
  DependencyContainer(Value *val) {
    var_ptr = val;
    var_lsb = pos_l = 0;
    var_hsb = pos_h = val->getType()->getIntegerBitWidth() - 1;
  }

  // overload equality operator for cmp_to_0_map
  bool operator==(const DependencyContainer &rhs) const {
    return (var_ptr == rhs.var_ptr) && (var_lsb == rhs.var_lsb) && (var_hsb == rhs.var_hsb);
  }

  friend raw_ostream& operator<<(raw_ostream& os, const DependencyContainer& dc) {
    os << dc.var_ptr->getName() << "[" << dc.var_hsb << ":" << dc.var_lsb 
                           << "] -- [" << dc.pos_h << ":" << dc.pos_l << "]";
    return os;
  }
};

class DependencyHash {
public:
  
  size_t operator()(const DependencyContainer& dc) const {
      return (size_t)dc.var_ptr + (size_t)dc.var_lsb + (size_t)dc.var_hsb;
  }
};

class BitwiseContainer {
public:
  BitwiseContainer() {}
  BitwiseContainer(Value *val) {
    dependencies.push_back(DependencyContainer(val));
    //setBitstr(val->getType()->getIntegerBitWidth(), 'x');
    setBitstr(std::string(val->getType()->getIntegerBitWidth(), 'x'));
  }
  BitwiseContainer(std::string conststr) {
    setBitstr(conststr);
  }

  // Operations
  BitwiseContainer trunc_to(int);
  BitwiseContainer zext_to(int);
  BitwiseContainer operator&(const BitwiseContainer &);
  BitwiseContainer operator|(const BitwiseContainer &);
  BitwiseContainer lshr(int);

  // NOTE: tri_str is reverted for manipulation purposes
  void setBitstr(std::string conststr) { bitstr.tri_str = std::string(conststr.rbegin(), conststr.rend()); }
  void setBitstr(BitStr bs) { bitstr = bs; }
  BitStr& getBitstr() { return bitstr; }
  
  bool isConstVal() { return bitstr.isConstVal(); }
  int getConstVal() { return bitstr.getConstVal(); }

  void pushDependency(DependencyContainer dc) { dependencies.push_back(dc); }
  void insertDependency(DependencyContainer dc);
  std::vector<DependencyContainer> getDependencies() const { return dependencies; }
  void setDependencies(std::vector<DependencyContainer> dc_vec) { dependencies = dc_vec; }

  void print() {
    errs() << "BitStr: " << bitstr << "\n";
    
    errs() << "Dependencies: {";
    for (auto it = dependencies.begin(); it != dependencies.end(); ++it)
      errs() << *it << ", ";
    errs() << "}\n";
  }

  struct SymbolicRange {
    int start, end;
    bool overlapped;

    SymbolicRange(int s, int e, bool ol = false) : start(s), end(e), overlapped(ol) {}
  };

  // bitwise AND, OR
  std::vector<SymbolicRange> computeSymbolicRanges(BitStr);
  void inheritDependencies(std::vector<SymbolicRange> &, std::vector<DependencyContainer>, std::vector<DependencyContainer>);
  bool isOverlapConsistent(DependencyContainer dc);
  
private:
  std::vector<DependencyContainer> dependencies;
  BitStr bitstr;
};


class RTL2ILAPass : public PassInfoMixin<RTL2ILAPass> {
public:
  PreservedAnalyses run(Function &F, FunctionAnalysisManager &AM);

private:
  BitwiseContainer* getBitwiseContainer(Value *, std::unordered_map<Value *, BitwiseContainer *> &);

  std::unordered_map<Value *, BitwiseContainer *> bitwise_inst_map;
  std::unordered_map<DependencyContainer, Value *, DependencyHash> cmp_to_0_map;
};

} // namespace llvm

#endif // LLVM_TRANSFORMS_RTL2ILA_H