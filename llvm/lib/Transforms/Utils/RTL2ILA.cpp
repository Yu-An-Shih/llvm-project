//===-- RTL2ILA.cpp - <TODO> --------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#include "llvm/Transforms/Utils/RTL2ILA.h"

//#include "llvm/IR/InstrTypes.h"
#include "llvm/IR/Instructions.h"

#include <bitset>
//#include <algorithm>

using namespace llvm;

#define BITSET_WIDTH 64  // ISSUE: Is 64 enough?

PreservedAnalyses RTL2ILAPass::run(Function &F, FunctionAnalysisManager &AM) {
  //errs() << "Function: " << F.getName() << "\n";
  for (auto& B : F) {
    //errs() << "Basic block:\n" << B << "\n";

    for (auto& I : B) {
      if (auto* inst = dyn_cast<CastInst>(&I)) {
        if (std::string(inst->getOpcodeName()) == "trunc" || std::string(inst->getOpcodeName()) == "zext") {
          Value *operand = inst->getOperand(0);

          // get the BitwiseContainer for operand
          BitwiseContainer *operand_ct = getBitwiseContainer(operand, bitwise_inst_map);
          if (operand_ct == nullptr)
            continue;

          // generate BitwiseContainer for the new value corresponding to the instruction
          BitwiseContainer *inst_ct;
          if (std::string(inst->getOpcodeName()) == "trunc") {
            inst_ct = new BitwiseContainer(operand_ct->trunc_to(inst->getType()->getIntegerBitWidth()));
          } else if (std::string(inst->getOpcodeName()) == "zext") {
            inst_ct = new BitwiseContainer(operand_ct->zext_to(inst->getType()->getIntegerBitWidth()));
          }
          
          if (inst_ct->isConstVal()) { // the whole value is known
            // rewrite the instruciton
            int constval = inst_ct->getConstVal();
            inst->setOperand(0, ConstantInt::get(operand->getType(), constval));

            // ISSUE: should this be insert to bitwise_inst_map (aggressive)?
          } else {
            bitwise_inst_map[inst] = inst_ct;
          }

          // DEBUG: print
          //errs() << inst->getName() << ": ";
          //inst_ct->print();

        }
      } else if (auto* inst = dyn_cast<BinaryOperator>(&I)) {
        //errs() << "Instruction: " << I << "\n";

        if (inst->getOpcode() == Instruction::BinaryOps::And || inst->getOpcode() == Instruction::BinaryOps::Or || inst->getOpcode() == Instruction::BinaryOps::LShr) {
          Value *lhs = inst->getOperand(0);
          Value *rhs = inst->getOperand(1);
          
          // get the BitwiseContainer for lhs
          BitwiseContainer *lhs_ct = getBitwiseContainer(lhs, bitwise_inst_map);
          if (lhs_ct == nullptr)
            continue;
          // get the BitwiseContainer for rhs
          BitwiseContainer *rhs_ct = getBitwiseContainer(rhs, bitwise_inst_map);
          if (rhs_ct == nullptr)
            continue;

          // generate BitwiseContainer for the new value corresponding to the instruction
          BitwiseContainer *inst_ct;
          if (inst->getOpcode() == Instruction::BinaryOps::And) {
            inst_ct = new BitwiseContainer(*lhs_ct & *rhs_ct);
          } else if (inst->getOpcode() == Instruction::BinaryOps::Or) {
            inst_ct = new BitwiseContainer(*lhs_ct | *rhs_ct);
          } else if (inst->getOpcode() == Instruction::BinaryOps::LShr) {
            if (!rhs_ct->isConstVal()) // bitwise track can only be done when the right shift amount is a known constant
              continue;

            inst_ct = new BitwiseContainer(lhs_ct->lshr(rhs_ct->getConstVal()));
          }
          
          
          if (inst_ct->isConstVal()) { // the whole value is known
            // rewrite the instruciton
            int constval = inst_ct->getConstVal();
            inst->setOperand(0, ConstantInt::get(lhs->getType(), constval));
            inst->setOperand(1, ConstantInt::get(rhs->getType(), constval));

            // ISSUE: should we be insert to bitwise_inst_map (aggressive)?
          } else if ((inst_ct->getBitstr()).tri_str.find('z') == std::string::npos) {
            // insert inst to bitwise_inst_map if no overlap between symbolic values
            bitwise_inst_map[inst] = inst_ct;
          }
        
          // DEBUG: print
          //errs() << inst->getName() << ": ";
          //inst_ct->print();
        }
      } else if (auto* inst = dyn_cast<ICmpInst>(&I)) {
        //errs() << "Instruction: " << I << "\n";
        
        // cond should be eq or ne
        if (!inst->isEquality())
          continue;

        // compare to zero
        Value *op2 = inst->getOperand(1);
        if (!isa<ConstantInt>(op2) || dyn_cast<ConstantInt>(op2)->getSExtValue() != 0)
          continue;
        
        Value *op1 = inst->getOperand(0);
        BitwiseContainer *op1_ct = getBitwiseContainer(op1, bitwise_inst_map);

        /*if (op1_ct != nullptr) {
          op1_ct->print();
          errs() << op1_ct->getDependencies().size() << " " << op1_ct->getBitstr().tri_str.find('1') << "\n";
        }*/
        
        // the value has only one dependency and all other bits are zero
        if (op1_ct == nullptr || op1_ct->getDependencies().size() != 1 || op1_ct->getBitstr().tri_str.find('1') != std::string::npos)
          continue;
        
        DependencyContainer dc = op1_ct->getDependencies()[0];
        auto map_it = cmp_to_0_map.find(dc);
        
        if (map_it == cmp_to_0_map.end()) {
          cmp_to_0_map[dc] = op1;
        } else {
          inst->setOperand(0, map_it->second);
          inst->setOperand(1, ConstantInt::get(map_it->second->getType(), 0));
        }
      }
    }
  }

  /*for (auto it = bitwise_inst_map.begin(); it != bitwise_inst_map.end(); ++it) {
    errs() << it->first->getName() << ": ";
    it->second->print();
  }*/
  
  //return PreservedAnalyses::all();
  return PreservedAnalyses::none();
}


BitwiseContainer* RTL2ILAPass::getBitwiseContainer(Value *val, std::unordered_map<Value *, BitwiseContainer *> &bitwise_inst_map) {
  BitwiseContainer *bc = nullptr;
  if (isa<Argument>(val)) {                                  // val is a function argument
    // create a new BitwiseContainer for the val
    // and insert it to bitwise_inst_map
    bc = new BitwiseContainer(val);
    bitwise_inst_map[val] = bc;
  } else if (auto* val_const = dyn_cast<ConstantInt>(val)) { // val is a constant
    // create a new BitwiseContainer to hold the constant
    std::string conststr = std::bitset<BITSET_WIDTH>(val_const->getSExtValue()).to_string();
    conststr.erase(0, BITSET_WIDTH - val_const->getBitWidth());
    bc = new BitwiseContainer(conststr);
  } else if (bitwise_inst_map.count(val)) {
    bc = bitwise_inst_map[val];
  }
  // ISSUE: should we be more aggressive? (create new BitwiseContainer even if val is none of the above)
  return bc;
}

/* BitwiseContainer */

BitwiseContainer BitwiseContainer::trunc_to(int bitwidth) {
  BitwiseContainer result;
  // compute bitstr
  result.setBitstr(bitstr.trunc_to(bitwidth));
  // update dependencies
  for (auto it = dependencies.begin(); it != dependencies.end(); ++it) {
    if (it->pos_h < bitwidth) {
      result.pushDependency(*it);
    } else {
      DependencyContainer tmp = *it;
      tmp.pos_h = bitwidth - 1;
      tmp.var_hsb -= (it->pos_h - (bitwidth - 1));

      if (tmp.pos_l <= tmp.pos_h)
        result.pushDependency(tmp);
      
      break;
    }
  }

  return result;
}

BitwiseContainer BitwiseContainer::zext_to(int bitwidth) {
  BitwiseContainer result;
  // compute bitstr
  result.setBitstr(bitstr.zext_to(bitwidth));
  // update dependencies
  result.setDependencies(dependencies);
  
  return result;
}

BitwiseContainer BitwiseContainer::operator&(const BitwiseContainer &rhs_ct) {
  BitwiseContainer result;

  // compute bitstr
  BitStr bs = this->bitstr & rhs_ct.bitstr;
  result.setBitstr(bs);

  // compute symbolic ranges
  std::vector<SymbolicRange> sym_ranges = computeSymbolicRanges(bs);

  // inherit dependencies
  result.inheritDependencies(sym_ranges, this->getDependencies(), rhs_ct.getDependencies());
  
  return result;
}

BitwiseContainer BitwiseContainer::operator|(const BitwiseContainer &rhs_ct) {
  BitwiseContainer result;

  // compute bitstr
  BitStr bs = this->bitstr | rhs_ct.bitstr;
  result.setBitstr(bs);
  
  // compute symbolic ranges
  std::vector<SymbolicRange> sym_ranges = computeSymbolicRanges(bs);

  // inherit dependencies
  result.inheritDependencies(sym_ranges, this->getDependencies(), rhs_ct.getDependencies());
  
  return result;
}

BitwiseContainer BitwiseContainer::lshr(int num) {
  BitwiseContainer result;

  // compute bitstr
  result.setBitstr(bitstr.lshr(num));
  
  // update dependencies
  for (auto it = dependencies.begin(); it != dependencies.end(); ++it) {
    DependencyContainer tmp = *it;
    tmp.pos_l -= num; tmp.pos_h -= num;
    if (tmp.pos_h < 0)
      continue;
    if (tmp.pos_l < 0) {
      tmp.var_lsb -= tmp.pos_l;
      tmp.pos_l = 0;
    }
    
    result.pushDependency(tmp);
  }
  
  return result;
}

std::vector<BitwiseContainer::SymbolicRange> BitwiseContainer::computeSymbolicRanges(BitStr bs) {
  std::vector<SymbolicRange> sym_ranges;

  std::string ts = bs.tri_str;
  int start, end;
  bool started = false, overlapped = false;
  for (size_t i = 0; i < ts.size(); i++) {
    switch (started) {
      case false:
        if (ts[i] == 'x' || ts[i] == 'z') {
          start = i;
          started = true;
          overlapped = (ts[i] == 'z');
        }
        break;
      case true:
        if ((!overlapped && ts[i] != 'x') || (overlapped && ts[i] != 'z')) {
          end = i - 1;
          sym_ranges.push_back(SymbolicRange(start, end, overlapped));

          if (ts[i] == '0' || ts[i] == '1') {
            started = false;
          } else {
            start = i;
            overlapped = (ts[i] == 'z');
          }
        }
        break;
    }
  }

  if (started) {
    end = ts.size() - 1;
    sym_ranges.push_back(SymbolicRange(start, end, overlapped));
  }

  return sym_ranges;
}

void BitwiseContainer::inheritDependencies(std::vector<SymbolicRange> &sym_ranges, std::vector<DependencyContainer> lhs_dps, std::vector<DependencyContainer> rhs_dps) {
  size_t lhs_id = 0, rhs_id = 0;
  
  for (auto sr_it = sym_ranges.begin(); sr_it != sym_ranges.end(); ++sr_it) {
    // inherit dependencies from lhs
    for (size_t i = lhs_id; i < lhs_dps.size(); ++i) {
      if (lhs_dps[i].pos_h >= sr_it->start) {
        if (lhs_dps[i].pos_l > sr_it->end) {
          lhs_id = i;
          break;
        }
        // push back dependency
        DependencyContainer dc(
          std::max(lhs_dps[i].pos_l, sr_it->start), std::min(lhs_dps[i].pos_h, sr_it->end), // pos_l, pos_h
          lhs_dps[i].var_ptr, // var_ptr
          std::max(lhs_dps[i].var_lsb, lhs_dps[i].var_lsb + (sr_it->start - lhs_dps[i].pos_l)), // var_lsb
          std::min(lhs_dps[i].var_hsb, lhs_dps[i].var_hsb - (lhs_dps[i].pos_h - sr_it->end))    // var_hsb
        );
        pushDependency(dc);

        if (lhs_dps[i].pos_h >= sr_it->end) {
          lhs_id = (lhs_dps[i].pos_h == sr_it->end) ? i + 1 : i;
          break;
        }
      }
    }
    // inherit dependencies from rhs
    for (size_t i = rhs_id; i < rhs_dps.size(); ++i) {
      if (rhs_dps[i].pos_h >= sr_it->start) {
        if (rhs_dps[i].pos_l > sr_it->end) {
          rhs_id = i;
          break;
        }
        // insert or check dependency
        DependencyContainer dc(
          std::max(rhs_dps[i].pos_l, sr_it->start), std::min(rhs_dps[i].pos_h, sr_it->end), // pos_l, pos_h
          rhs_dps[i].var_ptr, // var_ptr
          std::max(rhs_dps[i].var_lsb, rhs_dps[i].var_lsb + (sr_it->start - rhs_dps[i].pos_l)), // var_lsb
          std::min(rhs_dps[i].var_hsb, rhs_dps[i].var_hsb - (rhs_dps[i].pos_h - sr_it->end))    // var_hsb
        );
        if (sr_it->overlapped) {
          if (isOverlapConsistent(dc)) {
            int substr_width = dc.pos_h - dc.pos_l + 1;
            bitstr.tri_str.replace(dc.pos_l, substr_width, std::string(substr_width, 'x'));
          } else
            return ;
        }
        else
          insertDependency(dc);

        if (rhs_dps[i].pos_h >= sr_it->end) {
          rhs_id = (rhs_dps[i].pos_h == sr_it->end) ? i + 1 : i;
          break;
        }
      }
    }
  }
}

void BitwiseContainer::insertDependency(DependencyContainer dc) {
  pushDependency(dc);
  for (size_t i = dependencies.size() - 1; i > 0; --i) {
    if (dependencies[i].pos_l < dependencies[i-1].pos_l) {
      DependencyContainer dc = dependencies[i];
      dependencies[i] = dependencies[i-1];
      dependencies[i-1] = dc;
    } else {
      break;
    }
  }
}

bool BitwiseContainer::isOverlapConsistent(DependencyContainer dc) {
  // TODO: improve efficiency
  for (auto dp_it = dependencies.begin(); dp_it != dependencies.end(); ++dp_it) {
    if (!(dp_it->pos_h < dc.pos_l || dp_it->pos_l > dc.pos_h)) { // overlapped
      if (dp_it->var_ptr != dc.var_ptr) return false;
    }
  }
  return true;
}

/* BitStr */

BitStr BitStr::trunc_to(int bitwidth) {
  BitStr result;
  result.tri_str = tri_str;
  result.tri_str.erase(bitwidth, tri_str.size());

  return result;
}

BitStr BitStr::zext_to(int bitwidth) {
  BitStr result;
  result.tri_str = tri_str;
  result.tri_str.append(bitwidth - tri_str.size(), '0');

  return result;
}

BitStr BitStr::operator&(const BitStr &rhs_bs) {
  BitStr result;
  
  result.tri_str = std::string((this->tri_str).size(), 'x');
  for (size_t i = 0; i < (this->tri_str).size(); i++) {
    if (this->tri_str[i] == '0' || rhs_bs.tri_str[i] == '0')
      result.tri_str[i] = '0';
    else if (this->tri_str[i] == '1' && rhs_bs.tri_str[i] == '1')
      result.tri_str[i] = '1';
    else if (this->tri_str[i] == 'x' && rhs_bs.tri_str[i] == 'x')
      result.tri_str[i] = 'z'; // NOTE: overlap of symbolic values
  }
  
  return result;
}

BitStr BitStr::operator|(const BitStr &rhs_bs) {
  BitStr result;
  
  result.tri_str = std::string((this->tri_str).size(), 'x');
  for (size_t i = 0; i < (this->tri_str).size(); i++) {
    if (this->tri_str[i] == '1' || rhs_bs.tri_str[i] == '1')
      result.tri_str[i] = '1';
    else if (this->tri_str[i] == '0' && rhs_bs.tri_str[i] == '0')
      result.tri_str[i] = '0';
    else if (this->tri_str[i] == 'x' && rhs_bs.tri_str[i] == 'x')
      result.tri_str[i] = 'z'; // NOTE: overlap of symbolic values
  }
  
  return result;
}

BitStr BitStr::lshr(int num) {
  assert((size_t)num <= tri_str.size());
  
  BitStr result;
  result.tri_str = tri_str;
  result.tri_str.erase(0, num);

  result = result.zext_to(tri_str.size());

  return result;
}
