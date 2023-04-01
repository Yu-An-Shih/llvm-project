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


PreservedAnalyses RTL2ILAPass::run(Function &F, FunctionAnalysisManager &AM)
{
  // errs() << "Function: " << F.getName() << "\n";
  
  for (auto& B : F)
  {
    // errs() << "Basic block:\n" << B << "\n";
    for (auto& I : B)
    {
      // errs() << "Instruction: " << I << "\n";

      if (auto* inst = dyn_cast<CastInst>(&I))  // Conversion Operations
      {
        Value *operand = inst->getOperand(0);

        if (inst->getType()->isVectorTy() || operand->getType()->isVectorTy())
        {
          errs() << "Warning: Does not handle vector types.\n";
          continue;
        }
        
        // get the BitwiseContainer for operand
        BitwiseContainer *operand_ct = getBitwiseContainer(operand);
        assert(operand_ct != nullptr);
        
        // generate BitwiseContainer for the new value corresponding to the instruction
        BitwiseContainer *inst_ct;
        switch (inst->getOpcode())
        {
        case Instruction::CastOps::Trunc: // trunc .. to
          inst_ct = new BitwiseContainer(operand_ct->trunc_to(inst->getType()->getIntegerBitWidth()));
          break;
        
        case Instruction::CastOps::ZExt:  // zext .. to
          inst_ct = new BitwiseContainer(operand_ct->zext_to(inst->getType()->getIntegerBitWidth()));
          break;
        
        case Instruction::CastOps::SExt:
          inst_ct = new BitwiseContainer(operand_ct->sext_to(inst->getType()->getIntegerBitWidth()));
          break;
        
        default:
          continue;
          // break;
        }

        if (inst_ct->isAllKnown()) // the whole value is known
        {
          // extract constant value and replace the original instruction value
          u_int64_t constval = inst_ct->getKnownVal();
          Constant *inst_const = ConstantInt::get(inst->getType(), constval);
          
          inst->replaceAllUsesWith(inst_const);
        }
        else if (inst_ct->isPartKnown())  // part of the value is known
        {
          // insert inst to bitwise_inst_map
          bitwise_inst_map[inst] = inst_ct;
        }

        // DEBUG: print
        // errs() << inst->getName() << ": "; inst_ct->print();
        
      }
      else if (auto* inst = dyn_cast<BinaryOperator>(&I))   // Binary Bitwise Operations
      {
        if (inst->getType()->isVectorTy())
        {
          errs() << "Warning: Does not handle vector types.\n";
          continue;
        }
        
        Value *op0 = inst->getOperand(0);
        Value *op1 = inst->getOperand(1);

        // get the BitwiseContainer for operands
        BitwiseContainer *op0_ct = getBitwiseContainer(op0);
        BitwiseContainer *op1_ct = getBitwiseContainer(op1);
        assert(op0_ct != nullptr && op0_ct != nullptr);

        // generate BitwiseContainer for the new value corresponding to the instruction
        BitwiseContainer *inst_ct;
        switch (inst->getOpcode())
        {
        case Instruction::BinaryOps::And:
          inst_ct = new BitwiseContainer(*op0_ct & *op1_ct);
          break;
        
        case Instruction::BinaryOps::Or:
          inst_ct = new BitwiseContainer(*op0_ct | *op1_ct);
          break;
        
        case Instruction::BinaryOps::Xor:
          inst_ct = new BitwiseContainer(*op0_ct ^ *op1_ct);
          // TODO: xor as not
          break;
        
        case Instruction::BinaryOps::Shl:
          if (!op1_ct->isAllKnown()) // bitwise track can only be done when the left shift amount is a known constant
            continue;
          
          inst_ct = new BitwiseContainer(op0_ct->shl(op1_ct->getKnownVal(), inst->hasNoUnsignedWrap(), inst->hasNoSignedWrap()));
          break;
        
        case Instruction::BinaryOps::LShr:
          if (!op1_ct->isAllKnown()) // bitwise track can only be done when the right shift amount is a known constant
            continue;
          
          inst_ct = new BitwiseContainer(op0_ct->lshr(op1_ct->getKnownVal(), inst->isExact()));
          break;
        
        case Instruction::BinaryOps::AShr:
          if (!op1_ct->isAllKnown()) // bitwise track can only be done when the right shift amount is a known constant
            continue;
          
          inst_ct = new BitwiseContainer(op0_ct->ashr(op1_ct->getKnownVal(), inst->isExact()));
          break;

        default:
          continue;
          // break;
        }

        if (inst_ct->isAllKnown()) // the whole value is known
        {
          // extract constant value and replace the original instruction value
          u_int64_t constval = inst_ct->getKnownVal();
          Constant *inst_const = ConstantInt::get(inst->getType(), constval);
          
          inst->replaceAllUsesWith(inst_const);
        }
        else if (inst_ct->isPartKnown())  // part of the value is known
        {
          // insert inst to bitwise_inst_map
          bitwise_inst_map[inst] = inst_ct;
        }

        // DEBUG: print
        // errs() << inst->getName() << ": "; inst_ct->print();

      }
      else if (auto* inst = dyn_cast<SelectInst>(&I))   // select
      {
        if (inst->getType()->isVectorTy())
        {
          errs() << "Warning: Does not handle vector types.\n";
          continue;
        }

        Value *cond = inst->getCondition();
        Value *tru_val = inst->getTrueValue();
        Value *fals_val = inst->getFalseValue();

        // get the BitwiseContainer for operands
        BitwiseContainer *cond_ct = getBitwiseContainer(cond);
        BitwiseContainer *tru_ct = getBitwiseContainer(tru_val);
        BitwiseContainer *fals_ct = getBitwiseContainer(fals_val);
        
        // generate BitwiseContainer for the new value corresponding to the instruction
        BitwiseContainer *inst_ct;
        inst_ct = new BitwiseContainer(cond_ct->select(*tru_ct, *fals_ct));

        
        if (inst_ct->isAllKnown()) // the whole value is known
        {
          // extract constant value and replace the original instruction value
          u_int64_t constval = inst_ct->getKnownVal();
          Constant *inst_const = ConstantInt::get(inst->getType(), constval);
          
          inst->replaceAllUsesWith(inst_const);
        }
        else if (inst_ct->isPartKnown())  // part of the value is known
        {
          // insert inst to bitwise_inst_map
          bitwise_inst_map[inst] = inst_ct;
        }

        // DEBUG: print
        // errs() << inst->getName() << ": "; inst_ct->print();

      } 
      else if (auto* inst = dyn_cast<ICmpInst>(&I))   // icmp
      {
        if (inst->getType()->isVectorTy())
        {
          errs() << "Warning: Does not handle vector types.\n";
          continue;
        }

        Value *op0 = inst->getOperand(0);
        Value *op1 = inst->getOperand(1);

        // get the BitwiseContainer for operands
        BitwiseContainer *op0_ct = getBitwiseContainer(op0);
        BitwiseContainer *op1_ct = getBitwiseContainer(op1);
        assert(op0_ct != nullptr && op0_ct != nullptr);

        switch (inst->getPredicate())
        {
        case CmpInst::Predicate::ICMP_EQ:   // icmp eq op1, op2
          if (isEq(*op0_ct, *op1_ct))
          {
            Constant *inst_const = ConstantInt::get(inst->getType(), 1);
            inst->replaceAllUsesWith(inst_const);
          }
          else if (isNe(*op0_ct, *op1_ct))
          {
            Constant *inst_const = ConstantInt::get(inst->getType(), 0);
            inst->replaceAllUsesWith(inst_const);
          }
          break;
        
        case CmpInst::Predicate::ICMP_NE:   // icmp ne op1, op2
          if (isEq(*op0_ct, *op1_ct))
          {
            Constant *inst_const = ConstantInt::get(inst->getType(), 0);
            inst->replaceAllUsesWith(inst_const);
          }
          else if (isNe(*op0_ct, *op1_ct))
          {
            Constant *inst_const = ConstantInt::get(inst->getType(), 1);
            inst->replaceAllUsesWith(inst_const);
          }
          break;
        
        default:
          continue;
          // break;
        }
        
        /*// cond should be eq or ne
        if (!inst->isEquality())
          continue;

        // compare to zero
        Value *op2 = inst->getOperand(1);
        if (!isa<ConstantInt>(op2) || dyn_cast<ConstantInt>(op2)->getSExtValue() != 0)
          continue;
        
        Value *op1 = inst->getOperand(0);
        BitwiseContainer *op1_ct = getBitwiseContainer(op1);

        // if (op1_ct != nullptr) {
        //   op1_ct->print();
        //   errs() << op1_ct->getDependencies().size() << " " << op1_ct->getBitstr().tri_str.find('1') << "\n";
        // }
        
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
        }*/
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


BitwiseContainer* RTL2ILAPass::getBitwiseContainer(Value *val)
{
  BitwiseContainer *bc = nullptr;

  if (bitwise_inst_map.count(val))  // The bitwise values of val was computed and stored in bitwise_inst_map
  {
    bc = bitwise_inst_map[val];
  }
  else if (isa<Constant>(val))      // val is a constant
  {
    if (auto* val_const = dyn_cast<ConstantInt>(val))                   // val is a constant integer
    {
      // create a new BitwiseContainer to hold the constant
      std::string conststr = std::bitset<BITSET_WIDTH>(val_const->getSExtValue()).to_string();
      if (val_const->getBitWidth() <= BITSET_WIDTH)
        conststr.erase(0, BITSET_WIDTH - val_const->getBitWidth());
      else
      {
        errs() << "WARNING: the bitwidth of "; val_const->print(errs()); errs() << " is larger than BITSET_WIDTH (" << BITSET_WIDTH << ")!\n";
        std::string sextstr(val_const->getBitWidth() - BITSET_WIDTH, conststr[0]);
        sextstr.append(conststr);
        conststr = sextstr;
      }
      bc = new BitwiseContainer(conststr);
    }
    else if (auto* vect_const = dyn_cast<ConstantDataSequential>(val))   // val is a constant vector
    {
      // TODO
      errs() << val->getType()->isVectorTy() << "\n";
      errs() << "Warning: Does not handle vector types.\n";
      // vect_const->getElementAsInteger (unsigned i)
    }
    else
    {
      errs() << "Error: " << val->getName() << "contains unexpected type!!!\n";
      exit(0);
    }
  }
  else                              // val is a function argument, or just haven't been stored in bitwise_inst_map
  {
    if (val->getType()->isIntegerTy())      // val is an integer
    {
      bc = new BitwiseContainer(val);
      // bitwise_inst_map[val] = bc;  // ISSUE: Should this container be stored in bitwise_inst_map?
    }
    else if (val->getType()->isVectorTy())  // val is a vector
    {
      errs() << "Warning: Does not handle vector types.\n";
    }
    else
    {
      errs() << "Error: " << val->getName() << "contains unexpected type!!!\n";
      exit(0);
    }
  }

  if (bc == nullptr)
  {
    errs() << "Error: " << val->getName() << "returns null BitwiseContainer pointer!!!\n";
    exit(0);
  }
  
  return bc;
}

/*BitwiseContainer* RTL2ILAPass::getBitwiseContainer(Value *val) {
  BitwiseContainer *bc = nullptr;
  if (isa<Argument>(val)) {                                  // val is a function argument
    // create a new BitwiseContainer for the val
    // and insert it to bitwise_inst_map
    bc = new BitwiseContainer(val);
    bitwise_inst_map[val] = bc;
  } else if (auto* val_const = dyn_cast<ConstantInt>(val)) { // val is a constant
    // create a new BitwiseContainer to hold the constant
    std::string conststr = std::bitset<BITSET_WIDTH>(val_const->getSExtValue()).to_string();
    if (val_const->getBitWidth() <= BITSET_WIDTH)
      conststr.erase(0, BITSET_WIDTH - val_const->getBitWidth());
    else {
      errs() << "WARNING: the bitwidth of "; val_const->print(errs()); errs() << " is larger than BITSET_WIDTH (" << BITSET_WIDTH << ")!\n";
      std::string sextstr(val_const->getBitWidth() - BITSET_WIDTH, conststr[0]);
      sextstr.append(conststr);
      conststr = sextstr;
    }
    bc = new BitwiseContainer(conststr);
  } else if (bitwise_inst_map.count(val)) {
    bc = bitwise_inst_map[val];
  } else { // ISSUE: should we be more aggressive? (create new BitwiseContainer even if val is none of the above)
    bc = new BitwiseContainer(val);
    bitwise_inst_map[val] = bc;
  }
  return bc;
}*/

bool RTL2ILAPass::isEq(const BitwiseContainer &op0_ct, const BitwiseContainer &op1_ct)
{
  std::string op0_str = op0_ct.getBitstr().tri_str;
  std::string op1_str = op1_ct.getBitstr().tri_str;

  for (size_t i = 0; i < op0_str.size(); i++)
  {
    if ( !((op0_str[i] == '0' && op1_str[i] == '0') || (op0_str[i] == '1' && op1_str[i] == '1')) )
      return false;
  }
  
  return true;
}

bool RTL2ILAPass::isNe(const BitwiseContainer &op0_ct, const BitwiseContainer &op1_ct)
{
  std::string op0_str = op0_ct.getBitstr().tri_str;
  std::string op1_str = op1_ct.getBitstr().tri_str;

  for (size_t i = 0; i < op0_str.size(); i++)
  {
    if ((op0_str[i] == '0' && op1_str[i] == '1') || (op0_str[i] == '1' && op1_str[i] == '0'))
      return true;
  }
  
  return false;
}

/* BitwiseContainer */

BitwiseContainer BitwiseContainer::trunc_to(int bitwidth) {
  BitwiseContainer result;
  // compute bitstr
  result.setBitstr(bitstr.trunc_to(bitwidth));
  // update dependencies
  /*for (auto it = dependencies.begin(); it != dependencies.end(); ++it) {
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
  }*/

  return result;
}

BitwiseContainer BitwiseContainer::zext_to(int bitwidth) {
  BitwiseContainer result;
  // compute bitstr
  result.setBitstr(bitstr.zext_to(bitwidth));
  // update dependencies
  // result.setDependencies(dependencies);
  
  return result;
}

BitwiseContainer BitwiseContainer::sext_to(int bitwidth) {
  BitwiseContainer result;
  // compute bitstr
  result.setBitstr(bitstr.sext_to(bitwidth));
  
  return result;
}

BitwiseContainer BitwiseContainer::operator&(const BitwiseContainer &rhs_ct) {
  BitwiseContainer result;

  // compute bitstr
  BitStr bs = this->bitstr & rhs_ct.bitstr;
  result.setBitstr(bs);

  // compute symbolic ranges
  // std::vector<SymbolicRange> sym_ranges = computeSymbolicRanges(bs);

  // inherit dependencies
  // result.inheritDependencies(sym_ranges, this->getDependencies(), rhs_ct.getDependencies());
  
  return result;
}

BitwiseContainer BitwiseContainer::operator|(const BitwiseContainer &rhs_ct) {
  BitwiseContainer result;

  // compute bitstr
  BitStr bs = this->bitstr | rhs_ct.bitstr;
  result.setBitstr(bs);
  
  // compute symbolic ranges
  // std::vector<SymbolicRange> sym_ranges = computeSymbolicRanges(bs);

  // inherit dependencies
  // result.inheritDependencies(sym_ranges, this->getDependencies(), rhs_ct.getDependencies());
  
  return result;
}

BitwiseContainer BitwiseContainer::operator^(const BitwiseContainer &rhs_ct)
{
  BitwiseContainer result;

  // compute bitstr
  BitStr bs = this->bitstr ^ rhs_ct.bitstr;
  result.setBitstr(bs);
  
  return result;
}

BitwiseContainer BitwiseContainer::shl(int shift_amount, bool nuw = false, bool nsw = false)
{
  BitwiseContainer result;

  // compute bitstr
  result.setBitstr(bitstr.shl(shift_amount, nuw, nsw));
  
  return result;
}

BitwiseContainer BitwiseContainer::lshr(int shift_amount, bool exact = false)
{
  BitwiseContainer result;

  // compute bitstr
  result.setBitstr(bitstr.lshr(shift_amount, exact));
  
  // update dependencies
  /*for (auto it = dependencies.begin(); it != dependencies.end(); ++it) {
    DependencyContainer tmp = *it;
    tmp.pos_l -= shift_amount; tmp.pos_h -= shift_amount;
    if (tmp.pos_h < 0)
      continue;
    if (tmp.pos_l < 0) {
      tmp.var_lsb -= tmp.pos_l;
      tmp.pos_l = 0;
    }
    
    result.pushDependency(tmp);
  }*/
  
  return result;
}

BitwiseContainer BitwiseContainer::ashr(int shift_amount, bool exact = false)
{
  BitwiseContainer result;

  // compute bitstr
  result.setBitstr(bitstr.ashr(shift_amount, exact));
  
  return result;
}

BitwiseContainer BitwiseContainer::select(const BitwiseContainer &tru_ct, const BitwiseContainer &fals_ct)
{
  BitwiseContainer result;

  std::string cond = this->getBitstr().tri_str;

  if (cond == "1")
    result.setBitstr(tru_ct.getBitstr());
  else if (cond == "0")
    result.setBitstr(fals_ct.getBitstr());
  else
  {
    BitStr bs = bitstr.intersect(tru_ct.getBitstr(), fals_ct.getBitstr()); // ISSUE: select instead of intersect?
    result.setBitstr(bs);
  }

  return result;
}

/*std::vector<BitwiseContainer::SymbolicRange> BitwiseContainer::computeSymbolicRanges(BitStr bs) {
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
}*/

/*void BitwiseContainer::inheritDependencies(std::vector<SymbolicRange> &sym_ranges, std::vector<DependencyContainer> lhs_dps, std::vector<DependencyContainer> rhs_dps) {
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
}*/

/*void BitwiseContainer::insertDependency(DependencyContainer dc) {
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
}*/

/*bool BitwiseContainer::isOverlapConsistent(DependencyContainer dc) {
  // TODO: improve efficiency
  for (auto dp_it = dependencies.begin(); dp_it != dependencies.end(); ++dp_it) {
    if (!(dp_it->pos_h < dc.pos_l || dp_it->pos_l > dc.pos_h)) { // overlapped
      if (dp_it->var_ptr != dc.var_ptr) return false;
    }
  }
  return true;
}*/

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

BitStr BitStr::sext_to(int bitwidth) {
  BitStr result;
  char signbit = tri_str.back();
  result.tri_str = tri_str;
  result.tri_str.append(bitwidth - tri_str.size(), signbit);

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

BitStr BitStr::operator^(const BitStr &rhs_bs) {
  BitStr result;
  
  result.tri_str = std::string((this->tri_str).size(), 'x');
  for (size_t i = 0; i < (this->tri_str).size(); i++) {
    if ( (this->tri_str[i] == '0' && rhs_bs.tri_str[i] == '0')
        || (this->tri_str[i] == '1' && rhs_bs.tri_str[i] == '1') )
      result.tri_str[i] = '0';
    else if ( (this->tri_str[i] == '0' && rhs_bs.tri_str[i] == '1')
        || (this->tri_str[i] == '1' && rhs_bs.tri_str[i] == '0'))
      result.tri_str[i] = '1';
    else if (this->tri_str[i] == 'x' && rhs_bs.tri_str[i] == 'x')
      result.tri_str[i] = 'z'; // NOTE: overlap of symbolic values
  }
  
  return result;
}

BitStr BitStr::shl(int shift_amount, bool nuw = false, bool nsw = false)
{
  int str_len = tri_str.size();
  
  BitStr result;
  result.tri_str = std::string(str_len, 'x');
  
  // check if shl returns a poison value
  if (shift_amount > str_len)
    return result;
  else if (nuw)
  {
    for (int i = str_len - 1; i > str_len - 1 - shift_amount; i--)
      if (tri_str[i] != '0')
        return result;
  }
  else if (nsw)
  {
    for (int i = str_len - 1; i > str_len - 1 - shift_amount; i--)
    {
      if (tri_str[i] != tri_str[str_len - 1 - shift_amount])
        return result;
    }
  }
  
  // left shift tri_str to obtain result.tri_str
  result.tri_str.replace(0, shift_amount, std::string(shift_amount, '0'));
  result.tri_str.replace(shift_amount, str_len - shift_amount, tri_str, 0, str_len - shift_amount);

  return result;
}

BitStr BitStr::lshr(int shift_amount, bool exact = false)
{
  int str_len = tri_str.size();
  
  BitStr result;
  result.tri_str = std::string(str_len, 'x');
  
  // check if shl returns a poison value
  if (shift_amount > str_len)
    return result;
  else if (exact)
  {
    for (int i = 0; i < shift_amount; i++)
      if (tri_str[i] != '0')
        return result;
  }
  
  result.tri_str.replace(0, str_len - shift_amount, tri_str, shift_amount, str_len - shift_amount);
  result.tri_str.replace(str_len - shift_amount, shift_amount, std::string(shift_amount, '0'));

  return result;
}

BitStr BitStr::ashr(int shift_amount, bool exact = false)
{
  int str_len = tri_str.size();
  
  BitStr result;
  result.tri_str = std::string(str_len, 'x');
  
  // check if shl returns a poison value
  if (shift_amount > str_len)
    return result;
  else if (exact)
  {
    for (int i = 0; i < shift_amount; i++)
      if (tri_str[i] != '0')
        return result;
  }
  
  result.tri_str.replace(0, str_len - shift_amount, tri_str, shift_amount, str_len - shift_amount);
  result.tri_str.replace(str_len - shift_amount, shift_amount, std::string(shift_amount, tri_str[str_len - 1]));

  return result;
}

BitStr BitStr::intersect(const BitStr &val1_bs, const BitStr &val2_bs) {
  BitStr result;

  result.tri_str = std::string((val1_bs.tri_str).size(), 'x');
  for (size_t i = 0; i < (val1_bs.tri_str).size(); i++) {
    if (val1_bs.tri_str[i] == '1' && val2_bs.tri_str[i] == '1')
      result.tri_str[i] = '1';
    else if (val1_bs.tri_str[i] == '0' && val2_bs.tri_str[i] == '0')
      result.tri_str[i] = '0';
    else
      result.tri_str[i] = 'x';
  }

  return result;
}
