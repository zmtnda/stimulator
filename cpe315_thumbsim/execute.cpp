#include "thumbsim.hpp"
// These are just the register NUMBERS
#define PC_REG 15
#define LR_REG 14
#define SP_REG 13

// These are the contents of those registers
#define PC rf[PC_REG]
#define LR rf[LR_REG]
#define SP rf[SP_REG]

Stats stats;
Caches caches(0);

// CPE 315: you'll need to implement some custom sign-extension functions
// in addition to the ones given below, particularly for the unconditional
// branch instruction, which has an 11-bit immediate field
unsigned int signExtend16to32ui(short i) {
   return static_cast<unsigned int>(static_cast<int>(i));
}

unsigned int signExtend8to32ui(char i) {
   return static_cast<unsigned int>(static_cast<int>(i));
}

unsigned int signExtend11to32ui(short imme) {
   unsigned short signBit = imme & 0x0400;
   //get the sign bit
   //signBit = imme & (signBit << 10);
   if(signBit)
   {
      imme |= 0xF800; 
   }
   //mask the original bits
   //imme &= ~(1 << 10);
   //imme |= (signBit << 5);
   return signExtend16to32ui(imme); 
}
ASPR flags = {0, 0, 0, 0};

// CPE 315: You need to implement a function to set the Negative and Zero
// flags for each instruction that does that. It only needs to take
// one parameter as input, the result of whatever operation is executing

// This function is complete, you should not have to modify it
void setCarryOverflow (int num1, int num2, OFType oftype) {
   switch (oftype) {
      case OF_ADD:
         if (((unsigned long long int)num1 + (unsigned long long int)num2) ==
             ((unsigned int)num1 + (unsigned int)num2)) {
            flags.C = 0;
         }
         else {
            flags.C = 1;
         }
         if (((long long int)num1 + (long long int)num2) ==
             ((int)num1 + (int)num2)) {
            flags.V = 0;
         }
         else {
            flags.V = 1;
         }
         break;
      case OF_SUB:
         if (num1 >= num2) {
            flags.C = 1;
         }
         else if (((unsigned long long int)num1 - (unsigned long long int)num2) ==
                  ((unsigned int)num1 - (unsigned int)num2)) {
            flags.C = 0;
         }
         else {
            flags.C = 1;
         }
         if (((num1==0) && (num2==0)) ||
             (((long long int)num1 - (long long int)num2) ==
              ((int)num1 - (int)num2))) {
                flags.V = 0;
             }
         else {
            flags.V = 1;
         }
         break;
      case OF_SHIFT:
         // C flag unaffected for shifts by zero
         if (num2 != 0) {
            if (((unsigned long long int)num1 << (unsigned long long int)num2) ==
                ((unsigned int)num1 << (unsigned int)num2)) {
               flags.C = 0;
            }
            else {
               flags.C = 1;
            }
         }
         // Shift doesn't set overflow
         break;
      default:
         cerr << "Bad OverFlow Type encountered." << __LINE__ << __FILE__ << endl;
         exit(1);
   }
}

void setZeroNeg(int result) {
   flags.Z = 0;
   flags.N = 0;
   
   if (result == 0)
      flags.Z = 1;
   else if (result < 0)
      flags.N = 1;
}

// CPE E15: You're given the code for evaluating BEQ, and you'll need to
// complete the rest of these conditions. See Page 99 of the armv6 manual
static int checkCondition(unsigned short cond) {
                                                                                                         //stats.numRegReads++;
   switch(cond) {
      case EQ:
         if (flags.Z == 1) {
            return TRUE;
         }
         break;
      case NE:
         if (flags.Z == 0) {
            return TRUE;
         }
         break;
         /*carry set*/
      case CS:
         if (flags.C == 1)
            return TRUE;
         break;
         /*carry clear*/
      case CC:
         if (flags.C == 0)
            return TRUE;
         break;
         /*Minus, negative*/
      case MI:
         if (flags.N == 1)
            return TRUE;
         break;
         /*plus, positive or zero*/
      case PL:
         if (flags.N == 0)
            return TRUE;
         break;
         /*Overflow*/
      case VS:
         if (flags.V == 1)
            return TRUE;
         break;
         /*NO overflow*/
      case VC:
         if (flags.V == 0)
            return TRUE;
         break;
         /*unsigned higher*/
      case HI:
         if (flags.C == 1 && flags.Z == 0)
            return TRUE;
         break;
         /*unsigned lower or same*/
      case LS:
         if (flags.C == 0 && flags.Z == 1)
            return TRUE;
         break;
         /*signed >=*/
      case GE:
         if (flags.N == flags.V)
            return TRUE;
         break;
         /*signed <*/
      case LT:
         if (flags.N !=flags.V)
            return TRUE;
         break;
         /*signed >*/
      case GT:
         if (flags.Z == 0 && flags.N == flags.V)
            return TRUE;
         break;
         /*signed <=*/
      case LE:
         if (flags.Z == 1 || flags.N != flags.V)
            return TRUE;
         break;
      case AL:
         return TRUE;
         break;
   }
   return FALSE;
}

int countBits(int toCount, int mBit) {
   int numBits = 0;
   int mask = 0x01;
   
   while (toCount) {
      numBits += toCount & mask;
      toCount >>= 1;
   }
   
   numBits += 0x1 & mBit;
   
   return numBits;
}

void execute() {
   Data16 instr = imem[PC];
   Data16 instr2;
   Data32 temp(0);
   Thumb_Types itype;
   unsigned int pctarget = PC + 2;
   unsigned int addr;
   int i, n, offset;
   unsigned int list, mask;
   int num1, num2, result, BitCount;
   unsigned int bit;
   stats.numRegReads++;
   /* Convert instruction to correct type */
   BL_Type blupper(instr);
   ALU_Type alu(instr);
   SP_Type sp(instr);
   DP_Type dp(instr);
   LD_ST_Type ld_st(instr);
   MISC_Type misc(instr);
   COND_Type cond(instr);
   UNCOND_Type uncond(instr);
   LDM_Type ldm(instr);
   STM_Type stm(instr);
   LDRL_Type ldrl(instr);
   ADD_SP_Type addsp(instr);
   
   BL_Ops bl_ops;
   ALU_Ops add_ops;
   DP_Ops dp_ops;
   SP_Ops sp_ops;
   LD_ST_Ops ldst_ops;
   MISC_Ops misc_ops;
   
   rf.write(PC_REG, pctarget);
   stats.numRegWrites++;
   
   itype = decode(ALL_Types(instr));
   
   stats.instrs++;
   
   // CPE 315: The bulk of your work is in the following switch statement
   // All instructions will need to have stats and cache access info added
   // as appropriate for that instruction.
   switch(itype) {
      case ALU:
         add_ops = decode(alu);
         switch(add_ops) {
               //page150
            case ALU_LSLI:
               rf.write(alu.instr.lsli.rd, rf[alu.instr.lsli.rm] << alu.instr.lsli.imm);
               stats.numRegWrites++;
               setCarryOverflow(rf[alu.instr.lsli.rm] ,alu.instr.lsli.imm, OF_SHIFT);
               stats.numRegReads++;
               setZeroNeg(rf[alu.instr.lsli.rd]);
               break;
               //page 152
            case ALU_LSRI:
               rf.write(alu.instr.lsri.rd, rf[alu.instr.lsri.rm] >> alu.instr.lsri.imm);
               stats.numRegWrites++;
               setCarryOverflow(rf[alu.instr.lsri.rm],alu.instr.lsri.imm, OF_SHIFT);
               stats.numRegReads++;
               setZeroNeg(rf[alu.instr.lsri.rd]);
               break;
            //page 117
            case ALU_ASRI: // Works just fine
               rf.write(alu.instr.asri.rd, rf[alu.instr.asri.rm] >> alu.instr.asri.imm);
               stats.numRegWrites++;
               setCarryOverflow(rf[alu.instr.asri.rm],alu.instr.asri.imm, OF_SHIFT);
               stats.numRegReads++;
               setZeroNeg(rf[alu.instr.asri.rd]);
               break;
            case ALU_ADDR:
               //cout << "\tPuts r" << alu.instr.addr.rn << "(Value: " << rf[alu.instr.addr.rn]  << ") + r" << alu.instr.addr.rm << "(Value: " << rf[alu.instr.addr.rn] << ") in r" << alu.instr.addr.rd << "\n";
               rf.write(alu.instr.addr.rd, rf[alu.instr.addr.rn] + rf[alu.instr.addr.rm]);     // Original
               stats.numRegWrites++;
               //cout << "\t\tFinal value: " << rf[alu.instr.addr.rd] << "\n";
               setCarryOverflow(rf[alu.instr.addr.rn],rf[alu.instr.addr.rm], OF_ADD);
               stats.numRegReads += 2;
               setZeroNeg(rf[alu.instr.addr.rd]);
               break;
            case ALU_SUBR:
               //cout << "\tPuts r" << alu.instr.addr.rn << "(Value: " << rf[alu.instr.addr.rn]  << ") - r" << alu.instr.addr.rm << "(Value: " << rf[alu.instr.addr.rn] << ") in r" << alu.instr.addr.rd << "\n";
               rf.write(alu.instr.subr.rd, rf[alu.instr.subr.rn] - rf[alu.instr.subr.rm]);
               stats.numRegWrites++;
               //cout << "\t\tFinal value: " << rf[alu.instr.addr.rd] << "\n";
               setCarryOverflow(rf[alu.instr.subr.rn] ,rf[alu.instr.subr.rm], OF_SUB);
               stats.numRegReads += 2;
               setZeroNeg(rf[alu.instr.subr.rd]);
               break;
            case ALU_ADD3I:
               //cout << "\tPuts r" << alu.instr.add3i.rn << "(Value: " << rf[alu.instr.add3i.rn]  << ") - imm (Value: " << rf[alu.instr.add3i.imm] << ") in r" << alu.instr.add3i.rd << "\n";
               rf.write(alu.instr.add3i.rd, rf[alu.instr.add3i.rn] + alu.instr.add3i.imm);     // Original
               stats.numRegWrites++;
               //cout << "\t\tFinal value: " << rf[alu.instr.add3i.rd] << "\n";
               setCarryOverflow(rf[alu.instr.add3i.rn],alu.instr.add3i.imm, OF_ADD);
               setZeroNeg(rf[alu.instr.add3i.rd]);
               stats.numRegReads++;
               break;
            case ALU_SUB3I:
               //cout << "\tPuts r" << alu.instr.sub3i.rn << "(Value: " << rf[alu.instr.sub3i.rn]  << ") - imm (Value: " << rf[alu.instr.sub3i.imm] << ") in r" << alu.instr.sub3i.rd << "\n";
               rf.write(alu.instr.sub3i.rd, rf[alu.instr.sub3i.rn] - alu.instr.sub3i.imm);
               stats.numRegWrites++;
               //cout << "\t\tFinal value: " << rf[alu.instr.sub3i.rd] << "\n";
               setCarryOverflow(rf[alu.instr.sub3i.rn],alu.instr.sub3i.imm, OF_SUB);
               setZeroNeg(rf[alu.instr.sub3i.rd]);
               stats.numRegReads++;
               break;
            //page 155
            case ALU_MOV:
               //cout << "\tMoving " << alu.instr.mov.imm << " into r" << alu.instr.mov.rdn << "\n";
               rf.write(alu.instr.mov.rdn, alu.instr.mov.imm);                                  // Original
               stats.numRegWrites++;
               setZeroNeg(rf[alu.instr.mov.rdn]);
               //cout << "\t\tFinal value: " << rf[alu.instr.mov.rdn] << "\n";
               break;
            case ALU_CMP:
               //cout << "\tComparing r" << alu.instr.cmp.rdn << " (value: " << rf[alu.instr.cmp.rdn] << ") with " << alu.instr.cmp.imm << "\n";
               setCarryOverflow(rf[alu.instr.cmp.rdn], alu.instr.cmp.imm, OF_SUB);
               setZeroNeg(rf[alu.instr.cmp.rdn] - alu.instr.cmp.imm);
               stats.numRegReads++;
               //cout << "\t\tFlags are: C: " << (int) flags.C << " O: " << (int) flags.V << " Z: " << (int) flags.Z << " N:" << (int) flags.N << " \n";
               break;
            case ALU_ADD8I:
               //cout << "\tPuts r" << alu.instr.add8i.rdn << "(Value: " << rf[alu.instr.add8i.rdn]  << ") - imm (Value: " << rf[alu.instr.add8i.imm] << ") in r" << alu.instr.add8i.rdn << "\n";
               rf.write(alu.instr.add8i.rdn, rf[alu.instr.add8i.rdn] + alu.instr.add8i.imm);    // Original
               stats.numRegWrites++;
               stats.numRegReads++;
               //cout << "\t\tFinal value: " << rf[alu.instr.add8i.rdn] << "\n";
               setCarryOverflow(rf[alu.instr.add8i.rdn],alu.instr.add8i.imm, OF_ADD);
               setZeroNeg(rf[alu.instr.add8i.rdn]);
               break;
            case ALU_SUB8I:
               //cout << "\tPuts r" << alu.instr.sub8i.rdn << "(Value: " << rf[alu.instr.sub8i.rdn]  << ") - imm (Value: " << rf[alu.instr.sub8i.imm] << ") in r" << alu.instr.sub8i.rdn << "\n";
               rf.write(alu.instr.sub8i.rdn, rf[alu.instr.sub8i.rdn] - alu.instr.sub8i.imm);
               stats.numRegWrites++;
               stats.numRegReads++;
               //cout << "\t\tFinal value: " << rf[alu.instr.sub8i.rdn] << "\n";
               setCarryOverflow(rf[alu.instr.sub8i.rdn], alu.instr.sub8i.imm, OF_SUB);
               setZeroNeg(rf[alu.instr.sub8i.rdn]);
               break;
            default:
               break;
         }
         break;
      case BL:
         // This instruction is complete, nothing needed here
         bl_ops = decode(blupper);
         if (bl_ops == BL_UPPER) {
            // PC has already been incremented above
            instr2 = imem[PC];
            BL_Type bllower(instr2);
            if (blupper.instr.bl_upper.s) {
               addr = static_cast<unsigned int>(0xff<<24) |
               ((~(bllower.instr.bl_lower.j1 ^ blupper.instr.bl_upper.s))<<23) |
               ((~(bllower.instr.bl_lower.j2 ^ blupper.instr.bl_upper.s))<<22) |
               ((blupper.instr.bl_upper.imm10)<<12) |
               ((bllower.instr.bl_lower.imm11)<<1);
            }
            else {
               addr = ((blupper.instr.bl_upper.imm10)<<12) |
               ((bllower.instr.bl_lower.imm11)<<1);
            }
            // return address is 4-bytes away from the start of the BL insn
            rf.write(LR_REG, PC + 2);
            // Target address is also computed from that point
            rf.write(PC_REG, PC + 2 + addr);
            
            stats.numRegReads += 1;
            stats.numRegWrites += 2;
         }
         else {
            cerr << "Bad BL format." << endl;
            exit(1);
         }
         break;
      case DP:
         decode(dp);
         break;
      case SPECIAL:
         sp_ops = decode(sp);
         switch(sp_ops) {
            case SP_MOV:
               if (sp.instr.mov.d) {
                  //cout << "\tMoving r" << sp.instr.mov.rm << " into SP\n";
                  rf.write(SP_REG, rf[sp.instr.mov.rm]);
                  //cout << "\t\tSP is now " << rf[SP_REG] << "\n";
                  stats.numRegWrites++;
                  stats.numRegReads++;
                  //cout << "\t\tSP is now " << rf[SP_REG] << "\n";
               }
               else {
                  rf.write(sp.instr.mov.rd, rf[sp.instr.mov.rm]);
                  stats.numRegWrites++;
                  stats.numRegReads++;
               }
               break;
         }
         break;
      case LD_ST:
         // You'll want to use these load and store models
         // to implement ldrb/strb, ldm/stm and push/pop
         ldst_ops = decode(ld_st);
         switch(ldst_ops) {
               //page 177
               //STR <Rt>, [SP, #<imm8>]
            case STRI:
               addr = rf[ld_st.instr.ld_st_imm.rn] + ld_st.instr.ld_st_imm.imm * 4;
               dmem.write(addr, rf[ld_st.instr.ld_st_imm.rt]);
               caches.access(addr);
               stats.numMemWrites++;
               stats.numRegReads += 2;
               break;
               
            case LDRI:
               // Permitted values are multiples of 4 page 139
               addr = rf[ld_st.instr.ld_st_imm.rn] + ld_st.instr.ld_st_imm.imm * 4;
               rf.write(ld_st.instr.ld_st_imm.rt, dmem[addr]);
               stats.numMemReads++;
               stats.numRegWrites++;
               stats.numRegReads++;
               caches.access(addr);
               break;
               //STR <Rt>, [<Rn> {,#<imm5>}]
            case STRBI:
               // dont need the address to be multiple of 4
               addr = rf[ld_st.instr.ld_st_imm.rn] + ld_st.instr.ld_st_imm.imm;
               //read into temp
               temp = dmem[addr];
               stats.numMemReads++;
               //modified first byte of temp with first byte of rt
               //get a byte from the register
               temp.set_data_ubyte4(0, rf[ld_st.instr.ld_st_reg.rt] & 0xff);
               //write it back to addr
               dmem.write(addr, temp);
               stats.numRegReads += 2;
               caches.access(addr);
               break;
               //page 144
            case LDRBI:
               //get the address
               addr = rf[ld_st.instr.ld_st_imm.rn] + ld_st.instr.ld_st_imm.imm;
               //read the
               temp = rf[ld_st.instr.ld_st_reg.rt];
               temp.set_data_ubyte4(0, dmem[addr] & 0xff);
               stats.numMemReads++;
               rf.write(ld_st.instr.ld_st_imm.rt, temp);
               stats.numRegWrites++;
               stats.numRegReads += 2;
               caches.access(addr);
               break;
               //page 181
               //STR <Rt>, [<Rn>, <Rm>]
            case STRBR:
               //calculate the address
               addr = rf[ld_st.instr.ld_st_imm.rn] + rf[ld_st.instr.ld_st_reg.rm];
               temp = dmem[addr];
               stats.numMemReads++;
               //store the word in target register
               temp.set_data_ubyte4(0, rf[ld_st.instr.ld_st_reg.rt] & 0xff);
               dmem.write(addr, temp);
               caches.access(addr);
               stats.numRegReads += 3;
               //read a byte only
               break;
               //LDRB <Rt>, [<rn>, <Rm>]
               //page 145
            case LDRBR:
               //calculate the address
               addr = rf[ld_st.instr.ld_st_imm.rn] + rf[ld_st.instr.ld_st_reg.rm];
               temp = rf[ld_st.instr.ld_st_reg.rt];
               //read a byte only
               temp.set_data_ubyte4(0, dmem[addr] & 0xff);
               stats.numMemReads++;
               //write the word to register
               rf.write(ld_st.instr.ld_st_reg.rt, temp);
               stats.numRegWrites++;
               stats.numRegReads += 3;
               caches.access(addr);
               break;
         }
         break;
      case MISC:
         misc_ops = decode(misc);
         switch(misc_ops) {
            case MISC_PUSH:
               //cout << "\tPushing: " << rf[alu.instr.addr.rd] << "\n";
               BitCount = countBits(misc.instr.push.reg_list, misc.instr.push.m);
               // Don't forget to count m bit
               addr = SP - 4 * BitCount; // Number of registers
               //cerr << "Made it to Push\n";
               //// Debug, remove later
               
               for (i = 0; i < 8; i++) {
                  //                  cerr << "\tShould I push r" << i << " ?\n";                                          // Debug, remove later
                  if (misc.instr.push.reg_list & (int) pow(2, i)) {
                     //cout << "\t\tPushing r" << i << ", value: " << rf[i] <<"\n";                                         // Debug, remove later
                     dmem.write(addr, rf[i]);
                     stats.numMemWrites++;
                     caches.access(addr);
                     stats.numRegReads++;
                     addr += 4;
                  }
               }
               if (misc.instr.push.m & 0x1) {
                  dmem.write(addr, LR);
                  stats.numMemWrites++;
                  caches.access(addr);
               stats.numRegReads++;
               }
               rf.write(SP_REG, SP - 4 * BitCount);
               stats.numRegWrites++;
               stats.numRegReads++;
               break;
            case MISC_POP:
               BitCount = countBits(misc.instr.pop.reg_list, misc.instr.pop.m);
               addr = SP;
               stats.numRegReads++;
               //               cerr << "Made it to Pop\n";                                          // Debug, remove later
               for (i = 0; i < 8; i++) {
                  //                  cerr << "\tShould I pop r" << i << " ?\n";                                          // Debug, remove later
                  if (misc.instr.pop.reg_list & (1 << i)) {
                     //                     cerr << "\t\tPopping r" << i << "\n";                                          // Debug, remove later
                     rf.write(i, dmem[addr]);
                     stats.numRegWrites++;
                     stats.numMemReads++;
                     caches.access(addr);
                     //cout << "\tPopping r" << i << ", value: " << rf[i] <<"\n";                                          // Debug, remove later
                     addr += 4;
                  }
               }
               
               rf.write(SP_REG, SP + 4 * BitCount);
               stats.numRegWrites++;
               
               if (misc.instr.pop.m & 0x1) {
                  rf.write(PC_REG, dmem[addr]);
                  stats.numMemReads++;
                  stats.numRegWrites++;
                  caches.access(addr);
               }
               
               
               break;
            case MISC_SUB:
               //cout << "\tPuts SP (Value: " << rf[SP_REG]  << ") - imm (Value: " << misc.instr.sub.imm << ") in SP\n";
               rf.write(SP_REG, SP - (misc.instr.sub.imm*4));
               //cout << "\            t\tFinal value: " << rf[SP_REG] << "\n";
               stats.numRegWrites++;
               //cout << "\t\tFinal value: " << rf[SP_REG] << "\n";
               stats.numRegReads++;
               break;
            case MISC_ADD:
               //cout << "\tPuts SP (Value: " << rf[SP_REG]  << ") + imm (Value: " << misc.instr.add.imm << ") in SP\n";
               rf.write(SP_REG, SP + (misc.instr.add.imm*4));
               //cout << "\t\tFinal value: " << rf[SP_REG] << "\n";
               stats.numRegWrites++;
            stats.numRegReads++;
               //cout <<iii "\t\tFinal value: " << rf[SP_REG] << "\n";
               break;
         }
         break;
      case COND:
         decode(cond);
         // Once you've completed the checkCondition function,
         // this should work for all your conditional branches.
         offset = 2 * signExtend8to32ui(cond.instr.b.imm) + 2;
     
         if (checkCondition(cond.instr.b.cond)){
           if(offset > 0)
            //if (cond.instr.b.imm < 0)
            {
               stats.numForwardBranchesTaken++;
               cout << "forward " << stats.numForwardBranchesTaken << "\n";
            }
            else
               stats.numBackwardBranchesTaken++;
            rf.write(PC_REG, PC + offset);
            stats.numRegWrites++;
            stats.numRegReads++;
            stats.numBranches++;
         }
         else {
            if(offset > 0)
            
            //if (cond.instr.b.imm < 0)
               stats.numForwardBranchesNotTaken++;
            else
               stats.numBackwardBranchesNotTaken++;
         }
         break;
      case UNCOND:
         // Essentially the same as the conditional branches, but with no
         // condition check, and a different sized immediate field
         decode(uncond);
         rf.write(PC_REG, PC + 2 * signExtend11to32ui(uncond.instr.b.imm) + 2);
         stats.numRegWrites++;
         stats.numRegReads++;
         stats.numBranches++;
          //if(offset > 0)
         //if (uncond.instr.b.imm < 0)
         /*{   stats.numForwardBranchesTaken++;
               cout << "forward " << stats.numForwardBranchesTaken << "\n";
         }
         else
            stats.numBackwardBranchesTaken++;*/
         break;
      case LDM:
         decode(ldm);
         //BASE REGISTER
         addr = rf[ldm.instr.ldm.rn];
         stats.numRegReads++;
         BitCount = countBits(ldm.instr.ldm.reg_list, 0);
         for(i = 0; i < 8; i++){
            if(ldm.instr.ldm.reg_list & (1 << i)){
               rf.write(i, dmem[addr]);
               stats.numMemReads++;
               stats.numRegWrites++;
               caches.access(addr);
               //cout << "\t\tLoading multiple" << i << ", value:" << rf[i] << "\n";
               addr += 4;
            }
         }
         if((ldm.instr.ldm.reg_list & (1 << ldm.instr.ldm.rn)) == 0){
            rf.write(ldm.instr.ldm.rn, rf[ldm.instr.ldm.rn] + 4 * BitCount);
            stats.numRegWrites++;
         }
         break;
      case STM:
         decode(stm);
         //BASE REGISTER
         addr = rf[stm.instr.stm.rn];
         BitCount = countBits(stm.instr.stm.reg_list, 0);
         for(i = 0; i < 8; i++){
            if(stm.instr.stm.reg_list & (1 << i)){
               dmem.write(addr, rf[i]);
               stats.numRegReads++;
               stats.numMemWrites++;
               caches.access(addr);
               //cout << "\t\tLoading multiple" << i << ", value:" << rf[i] << "\n";
               addr += 4;
            }
         }
         rf.write(stm.instr.stm.rn, rf[stm.instr.stm.rn] + 4 * BitCount);
         stats.numRegWrites++;
         stats.numRegReads++;
         break;
      case LDRL:
         // This instruction is complete, nothing needed
         decode(ldrl);
         // Need to check for alignment by 4
         if (PC & 2) {
            addr = PC + 2 + (ldrl.instr.ldrl.imm)*4;
         }
         else {
            addr = PC + (ldrl.instr.ldrl.imm)*4;
         }
         // Requires two consecutive imem locations pieced together
         temp = imem[addr] | (imem[addr+2]<<16);  // temp is a Data32
         rf.write(ldrl.instr.ldrl.rt, temp);
         stats.numRegWrites++;
         
         // One write for updated reg
         stats.numRegWrites++;
         // One read of the PC
         stats.numRegReads++;
         // One mem read, even though it's imem, and there's two of them
         stats.numMemReads++;
         break;
      case ADD_SP:
         decode(addsp);
         //cout << "\tPuts SP (Value: " << rf[SP_REG]  << ") + imm (Value: " << misc.instr.add.imm << ") in r" << addsp.instr.add.rd << "\n";
         rf.write(addsp.instr.add.rd, SP + (addsp.instr.add.imm*4));
         //cout << "\t\tFinal value: " << rf[addsp.instr.add.rd] << "\n";
         stats.numRegWrites++;
         stats.numRegReads++;
         break;
      default:
         cout << "[ERROR] Unknown Instruction to be executed" << endl;
         exit(1);
         break;
   }
}
