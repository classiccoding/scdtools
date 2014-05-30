###############################################################################
# Copyright (c) 2014 by bgvanbur
#
# This program is free software; you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by the
# Free Software Foundation; either version 2 of the License, or (at your
# option) any later version.
#
# This program is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License along
# with this program; if not, write to the Free Software Foundation, Inc.,
# 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
###############################################################################
# Disassemble68K Support (for scddisassemble)
###############################################################################

# TODO current disassembly format support
# TODO asmx listing format, biggest obstacle need to track long usage to group
# TODO 68kd format, biggest obstacle is to make a switch for label usage
#      and use *-0x8 [0x3C8] instead for branches etc
# TODO asl listing format, probably pretty easy after asmx/68kd formats supported
# TODO SNASM68K listing format, same as asl probably

package SCDTools::Dis68K;
use strict;
use warnings;

use constant DIS_68K_SIZE_BYTE             => 0x0000;
use constant DIS_68K_SIZE_WORD             => 0x0040;
use constant DIS_68K_SIZE_LONG             => 0x0080;


use constant DIS_68K_EA_DN                 => 0x00000001;   # Dn
use constant DIS_68K_EA_AN                 => 0x00000002;   # An
use constant DIS_68K_EA_MEM_AN             => 0x00000004;   # (An)
use constant DIS_68K_EA_MEM_AN_POSTINC     => 0x00000008;   # (An)+
use constant DIS_68K_EA_MEM_AN_PREDEC      => 0x00000010;   # -(An)
use constant DIS_68K_EA_MEM_D16_AN         => 0x00000020;   # (d16,An)
use constant DIS_68K_EA_MEM_D8_AN_XN       => 0x00000040;   # (d8,An,Xn)
use constant DIS_68K_EA_MEM_D16            => 0x00000100;   # (xxx).W
use constant DIS_68K_EA_MEM_D32            => 0x00000200;   # (xxx).L
use constant DIS_68K_EA_IMMEDIATE          => 0x00001000;   # #<data>
use constant DIS_68K_EA_MEM_D16_PC         => 0x00000400;   # (d16,PC)
use constant DIS_68K_EA_MEM_D8_PC_XN       => 0x00000800;   # (d8,PC,Xn)

use constant DIS_68K_EA_ALL                => 0x00001F7F;
# all except An, IMMEDIATE, (d16,PC), (d8,PC,Xn)
# TODO see page 61 of PRM
use constant DIS_68K_EA_ALTERABLE          => 0x0000037F;
use constant DIS_68K_EA_DATA_ALTERABLE     => 0x0000037D;
use constant DIS_68K_EA_CONTROL_ALTERABLE  => 0x00000364;
use constant DIS_68K_EA_MEM_ALTERABLE      => 0x0000037C;
use constant DIS_68K_EA_DATA_ADDRESSING    => 0x00001F7D;
use constant DIS_68K_EA_CONTROL_ADDRESSING => 0x00000F64;


# SNASM68K optimization options
# op- 	Enable PC relative optimizations. Switch to PC relative addressing when
#       absolute long addressing is used and the code allows such a switch.
# os- 	Enable short branch optimizations. Backwards relative branches will use
#       the short form if possible.
# ow- 	Enable absolute word addressing optimizations. If absolute long
#       addressing is specified, but the address will fit in a word, the shorter
#       form is used. Optimization is not made if the size is specified.
# oz- 	Enable zero displacement (offset-zero) optimizations. If an instruction
#       uses the Indirect Addressing with Displacement addressing mode and the
#       displacement is zero, the instruction will be assembled to Indirect
#       Addressing mode.
# oaq- 	Enable ADDQ optimizations. ADD instructions that can be coded as ADDQ
#       instructions will be assembled as ADDQ instructions.
# osq- 	Enable SUBQ optimizations. Like above. 
# omq- 	Enable MOVEQ optimizations. Like above. 
use constant DIS_68K_DIFF_OPT_PC_RELATIVE                   => 0x80000000;
use constant DIS_68K_DIFF_OPT_SHORT_BRANCH                  => 0x40000000;
use constant DIS_68K_DIFF_OPT_ABS_WORD                      => 0x20000000;
use constant DIS_68K_DIFF_OPT_ZERO_DISPLACEMENT             => 0x10000000;
use constant DIS_68K_DIFF_OPT_ADD_TO_ADDQ                   => 0x08000000;
use constant DIS_68K_DIFF_OPT_SUB_TO_SUBQ                   => 0x04000000;
use constant DIS_68K_DIFF_OPT_MOVE_TO_MOVEQ                 => 0x02000000;

# TODO VASM optimizations (removed non-68000 ones and ones only for speed, only care about size related ones)
# - ADDA.? #0,An and SUBA.? #0,An will be deleted.
# - LEA (0,An),An and LEA (An),An will be deleted.
# - MOVEM.? <reglist> is deleted, when the register list was empty.
# - MULS.?/MULU.? #1,Dn is deleted (-opt-mul).
use constant DIS_68K_DIFF_OPT_NOP                          => 0x01000000;

# - ADDA.? #x,An optimized to LEA (x,An),An, when x is between -32768 and 32767.
# - ANDI.? #-1,<ea> optimized to TST.? <ea>.
# - ASL.? #1,Dn optimized to ADD.? Dn,Dn for 68000 and 68010.
# - ASL.? #2,Dn optimized into a sequence of two ADD.? Dn,Dn for 68000 and 68010, when the operation size is either byte or word and the options -opt-speed and -opt-lsl are given.
# - B<cc> <label> translated into a combination of B!<cc> *+8 and JMP <label>, when <label> is not defined in the same section (and option -opt-brajmp is given), or outside the range of -32768 to 32767 bytes from the current address when the selected CPU is not 68020 up, CPU32 or ColdFire ISA_B/C.
# - B<cc> <label> is automatically optimized to 8-bit, 16-bit or 32-bit (68020 up, CPU32, MCF5407 only), whatever fits best. When the selected CPU doesn’t support 32-bit branches it will try to change the conditional branch into a B<!cc> *+8 and JMP <label> sequence.
# - BRA <label> translated to JMP <label>, when <label> is not defined in the same section (and option -opt-brajmp is given), or outside the range of -32768 to 32767 bytes from the current address when the selected CPU is not 68020 up, CPU32 or ColdFire ISA_B/C.
# - BSR <label> translated to JSR <label>, when <label> is not defined in the same section (and option -opt-brajmp is given), or outside the range of -32768 to 32767 bytes from the current address when the selected CPU is not 68020 up, CPU32 or ColdFire ISA_B/C.
# - <cp>B<cc> <label> is automatically optimized to 16-bit or 32-bit, whatever fits best. <cp> means coprocessor and is P for the PMMU and F for the FPU.
# - CLR.L Dn optimized to MOVEQ #0,Dn.
# - CMP.? #0,<ea> optimized to TST.? <ea>. The selected CPU type must be MC68020 up, ColdFire or CPU32 to support address register direct as effective address (<ea>).
# - DIVS.W #-1,Dn optimized to the sequence of NEG.W Dn and MVZ.W Dn,Dn (-opt-div and -opt-speed).
# - DIVS.L/DIVU.L #1,Dn optimized to TST.L Dn (-opt-div).
# - DIVS.L #-1,Dn optimized to NEG.L Dn (-opt-div).
# - DIVU.L #2..256,Dn optimized to LSR.L #x,Dn (-opt-div).
# - EORI.? #-1,<ea> optimized to NOT.? <ea>.
# - EORI.? #0,<ea> optimized to TST.? <ea>.
# - FxDIV.? #m,FPn optimized to FxMUL.? #1/m,FPn when m is a power of 2 and option -opt-fconst is given.
# - JMP <label> optimized to BRA.? <label>, when <label> is defined in the same section and in the range of -32768 to 32767 bytes from the current address.
# - JSR <label> optimized to BSR.? <label>, when <label> is defined in the same section and in the range of -32768 to 32767 bytes from the current address.
# - LEA 0,An optimized to SUBA.L An,An.
# - LEA (d,An),An is optimized to ADDQ.L #d,An when d is between 1 and 8 and to SUBQ.L #-d,An when d is between -1 and -8.
# - LEA (d,Am),An will be translated into a combination of MOVEA and ADDA.L for 68000 and 68010, when d is lower than -32768 or higher than 32767. The MOVEA will be omitted when Am and An are identical. Otherwise -opt-speed is required.
# - LINK.L An,#x optimized to LINK.W An,#x, when x is between -32768 and 32767.
# - LINK.W An,#x translated to LINK.L An,#x, when x is not between -32768 and 32767 and selected CPU supports this instruction.
# - LSL.? #1,Dn optimized to ADD.? Dn,Dn for 68000 and 68010, when option -opt-lsl is given.
# - LSL.? #2,Dn optimized into a sequence of two ADD.? Dn,Dn for 68000 and 68010, when the operation size is either byte or word and the options -opt-speed and -opt-lsl are given.
# - MOVE.? #x,-(SP) optimized to PEA x, when allowed by the option -opt-pea. The move-size must not be byte (.b).
# - MOVE.B #-1,<ea> optimized to ST <ea>, when allowed by the option -opt-st.
# - MOVE.L #x,Dn optimized to MOVEQ #x,Dn, when x is between -128 and 127.
# - MOVEA.? #0,An optimized to SUBA.L An,An.
# - MOVEA.L #x,An optimized to MOVEA.W #x,An, when x is between -32768 and 32767.
# - MOVEA.L #label,An optimized to LEA label,An, which could allow further optimization to LEA label(PC),An.
# - MOVEM.? <ea>,An optimized to MOVE.? <ea>,An, when the register list only contains a single address register.
# - MOVEM.? <ea>,Rn optimized to MOVE.? <ea>,Rn and MOVEM.? Rn,<ea> optimized to MOVE.? Rn,<ea>, when allowed by the option -opt-movem or when just loading an address register.
# - MULS.?/MULU.? #0,Dn optimized to MOVEQ #0,Dn (-opt-mul).
# - MULS.W #-1,Dn optimized to the sequence EXT.L Dn and NEG.L Dn (-opt-mul and -opt-speed).
# - MULS.L #-1,Dn optimized to NEG.L Dn (-opt-mul).
# - MULS.W #2..256,Dn optimized to the sequence EXT.L Dn and ASL.L #x,Dn (-opt-mul and -opt-speed).
# - MULS.W #-2..-256,Dn optimized to the sequence EXT.L Dn, ASL.L #x,Dn and NEG.L Dn (-opt-mul and -opt-speed).
# - MULS.L #2..256,Dn optimized to ASL.L #x,Dn (-opt-mul).
# - MULS.L #-2..-256,Dn optimized to the sequence ASL.L #x,Dn and NEG.L Dn (-opt-mul and -opt-speed).
# - MULU.L #2..256,Dn optimized to LSL.L #x,Dn (-opt-mul).
# - ORI.? #0,<ea> optimized to TST.? <ea>.
# - SUB.? #x,<ea> optimized to SUBQ.? #x,<ea>, when x is between 1 and 8.
# - SUB.? #x,<ea> optimized to ADDQ.? #x,<ea>, when x is between -1 and -8.
# - SUBA.? #x,An optimized to LEA (-x,An),An, when x is between -32767 and 32768. 


# other disassembly affecting aspects
use constant DIS_68K_DIFF_MNEM_TO_MNEM_IMMEDIATE_OR_AND     => 0x00000001;
use constant DIS_68K_DIFF_MNEM_TO_MNEM_IMMEDIATE_ADD_SUB    => 0x00000002;
use constant DIS_68K_DIFF_MNEM_TO_MNEM_IMMEDIATE_CMP        => 0x00000004;
use constant DIS_68K_DIFF_NON_ZERO_UPPER_BYTE_MOVE_TO_CCR   => 0x00000008;
use constant DIS_68K_DIFF_UNSORTED_EXG                      => 0x00000010;
use constant DIS_68K_DIFF_BIT_NUMBER_BITS_UPPER_BYTE        => 0x00000020;
use constant DIS_68K_DIFF_BIT_NUMBER_BITS_LOWER_BYTE        => 0x00000040;
use constant DIS_68K_DIFF_IMMEDIATE_BYTE_IGNORED_NONZERO    => 0x00000080;
use constant DIS_68K_DIFF_SHORT_BRANCH_TO_FF                => 0x00000100;

# not marked in diffEncounterable
use constant DIS_68K_DIFF_EARLY_PASSES_ASSUMES_SHORT_BRANCH => 0x00001000; 

use constant DIS_68K_DIFF_ASL =>
    DIS_68K_DIFF_OPT_ZERO_DISPLACEMENT |
    DIS_68K_DIFF_NON_ZERO_UPPER_BYTE_MOVE_TO_CCR |
    DIS_68K_DIFF_BIT_NUMBER_BITS_UPPER_BYTE |
    DIS_68K_DIFF_IMMEDIATE_BYTE_IGNORED_NONZERO |
    DIS_68K_DIFF_EARLY_PASSES_ASSUMES_SHORT_BRANCH;
use constant DIS_68K_DIFF_ASMX =>
    DIS_68K_DIFF_OPT_ZERO_DISPLACEMENT |
    DIS_68K_DIFF_MNEM_TO_MNEM_IMMEDIATE_OR_AND |
    DIS_68K_DIFF_BIT_NUMBER_BITS_UPPER_BYTE |
    DIS_68K_DIFF_BIT_NUMBER_BITS_LOWER_BYTE;
use constant DIS_68K_DIFF_GAS =>
    DIS_68K_DIFF_MNEM_TO_MNEM_IMMEDIATE_OR_AND |
    DIS_68K_DIFF_MNEM_TO_MNEM_IMMEDIATE_ADD_SUB |
    DIS_68K_DIFF_MNEM_TO_MNEM_IMMEDIATE_CMP |
    DIS_68K_DIFF_OPT_ZERO_DISPLACEMENT |
    DIS_68K_DIFF_OPT_PC_RELATIVE |
    DIS_68K_DIFF_BIT_NUMBER_BITS_UPPER_BYTE |
    DIS_68K_DIFF_IMMEDIATE_BYTE_IGNORED_NONZERO |
    DIS_68K_DIFF_SHORT_BRANCH_TO_FF |
    DIS_68K_DIFF_OPT_MOVE_TO_MOVEQ;
use constant DIS_68K_DIFF_SNASM68K =>
    DIS_68K_DIFF_MNEM_TO_MNEM_IMMEDIATE_OR_AND |
    DIS_68K_DIFF_MNEM_TO_MNEM_IMMEDIATE_ADD_SUB |
    DIS_68K_DIFF_MNEM_TO_MNEM_IMMEDIATE_CMP |
    DIS_68K_DIFF_UNSORTED_EXG |
    DIS_68K_DIFF_BIT_NUMBER_BITS_UPPER_BYTE |
    DIS_68K_DIFF_IMMEDIATE_BYTE_IGNORED_NONZERO;
use constant DIS_68K_DIFF_VASM =>
    DIS_68K_DIFF_BIT_NUMBER_BITS_UPPER_BYTE |
    DIS_68K_DIFF_IMMEDIATE_BYTE_IGNORED_NONZERO;

# these are the SNASM68K options for optimizations
use constant DIS_68K_DIFF_SNASM68K_OPT =>
    DIS_68K_DIFF_OPT_PC_RELATIVE |
    DIS_68K_DIFF_OPT_SHORT_BRANCH |
    DIS_68K_DIFF_OPT_ABS_WORD |
    DIS_68K_DIFF_OPT_ZERO_DISPLACEMENT |
    DIS_68K_DIFF_OPT_ADD_TO_ADDQ |
    DIS_68K_DIFF_OPT_SUB_TO_SUBQ |
    DIS_68K_DIFF_OPT_MOVE_TO_MOVEQ;

# combine all targeted processors
my $diff = DIS_68K_DIFF_ASL | DIS_68K_DIFF_ASMX | DIS_68K_DIFF_SNASM68K | DIS_68K_DIFF_SNASM68K_OPT;
my $diffEncountered = 0;

my $regD   = 'D';
my $regA   = 'A';
my $regPC  = 'PC';
my $regCCR = 'CCR';
my $regSR  = 'SR';
my $regUSP = 'USP';

my $forceWord = '.W';
my $forceLong = '.L';



use SCDTools::DisCommon;
use SCDTools::DisFormat;
use SCDTools::DisPick;


sub Disassemble68K_NiceCheck {
    print STDERR sprintf( "Could have been assembled by asl: %s, asmx: %s, gas: %s, SNASM68K: %s, vasm: %s\n",
			  ( $diffEncountered & DIS_68K_DIFF_ASL      ? 'N' : 'Y' ),
			  ( $diffEncountered & DIS_68K_DIFF_ASMX     ? 'N' : 'Y' ),
			  ( $diffEncountered & DIS_68K_DIFF_GAS      ? 'N' : 'Y' ),
			  ( $diffEncountered & DIS_68K_DIFF_SNASM68K ? 'N' : 'Y' ),
			  ( $diffEncountered & DIS_68K_DIFF_VASM     ? 'N' : 'Y' ) );
    print STDERR sprintf( "Could use SNASM68K opts op: %s, os: %s, ow: %s, oz: %s, oaq: %s, osq: %s, omq: %s\n",
			  ( $diffEncountered & DIS_68K_DIFF_OPT_PC_RELATIVE       ? 'Y' : 'N' ),
			  ( $diffEncountered & DIS_68K_DIFF_OPT_SHORT_BRANCH      ? 'Y' : 'N' ),
			  ( $diffEncountered & DIS_68K_DIFF_OPT_ABS_WORD          ? 'Y' : 'N' ),
			  ( $diffEncountered & DIS_68K_DIFF_OPT_ZERO_DISPLACEMENT ? 'Y' : 'N' ),
			  ( $diffEncountered & DIS_68K_DIFF_OPT_ADD_TO_ADDQ       ? 'Y' : 'N' ),
			  ( $diffEncountered & DIS_68K_DIFF_OPT_SUB_TO_SUBQ       ? 'Y' : 'N' ),
			  ( $diffEncountered & DIS_68K_DIFF_OPT_MOVE_TO_MOVEQ     ? 'Y' : 'N' ) );
}

sub Disassemble68K_MnemCheck {
    # TODO better Bcc/BRA/DBcc/Scc support?
    return ( $_[0] =~ /^(ABCD|ADD|ADDA|ADDI|ADDQ|ADDX|AND|ANDI|ASL|ASR|(B|DB|S)(CS|LT|LS|LE|EQ|NE|HI|GT|CC|GE|PL|MI|VC|VS|RA|F|T|SF|ST)|BCHG|BCLR|BSET|BSR|BTST|CHK|CLR|CMP|CMPA|CMPI|CMPM|DIVS|DIVU|EOR|EORI|EXG|EXT|ILLEGAL|JMP|JSR|LEA|LINK|LSL|LSR|MOVE|MOVEA|MOVEM|MOVEP|MOVEQ|MULS|MULU|NBCD|NEG|NEGX|NOP|NOT|OR|ORI|PEA|RESET|ROL|ROR|ROXL|ROXR|RTE|RTR|RTS|SBCD|STOP|SUB|SUBA|SUBI|SUBQ|SUBX|SWAP|TAS|TRAP|TRAPV|TST|UNLK)(\.[SBWL])?$/i );
}

sub Disassemble68K_RegisterList {
    my ($problem,$dir,$addr,$width) = @_;
    my $data2 = &GetWord($addr+$width);
    $width += 2;
    my $list = '';
    if ( $data2 == 0x0000 ) {
	$problem |= DIS_PRBM_UNREPRESENTABLE;
    } else {
	if ( $dir ) {
	    # order high to low is d0 to d7 to a0 to a7
	    # flip around so can use same routine for both directions
	    $data2 = &FlipWord($data2);
	}

	# order high to low is a7 to a0 to d7 to d0

	# do D0-D7 first
	my $listStart = 0;
	my $listLength = 0;
	for ( my $i = 0; $i < 8; $i++ ) {
	    if ( ( $data2 >> $i ) & 1 ) {
		if ( $listLength == 0 ) {
		    $listStart = $i;
		}
		$listLength++;
	    } elsif ( $listLength ) {
		if ( $list ne '' ) {
		    $list .= '/';
		}
		if ( $listLength > 1 ) {
		    $list .= &Disassemble68K_Dn($listStart).'-'.&Disassemble68K_Dn($listStart+$listLength-1);
		} else {
		    $list .= &Disassemble68K_Dn($listStart);
		}
		$listLength = 0;
	    }
	}
	if ( $listLength ) {
	    if ( $list ne '' ) {
		$list .= '/';
	    }
	    if ( $listLength > 1 ) {
		$list .= &Disassemble68K_Dn($listStart).'-'.&Disassemble68K_Dn($listStart+$listLength-1);
	    } else {
		$list .= &Disassemble68K_Dn($listStart);
	    }
	}

	# do A0-A7 first
	$listStart = 0;
	$listLength = 0;
	for ( my $i = 0; $i < 8; $i++ ) {
	    if ( ( $data2 >> ($i+8) ) & 1 ) {
		if ( $listLength == 0 ) {
		    $listStart = $i;
		}
		$listLength++;
	    } elsif ( $listLength ) {
		if ( $list ne '' ) {
		    $list .= '/';
		}
		if ( $listLength > 1 ) {
		    $list .= &Disassemble68K_An($listStart).'-'.&Disassemble68K_An($listStart+$listLength-1);
		} else {
		    $list .= &Disassemble68K_An($listStart);
		}
		$listLength = 0;
	    }
	}
	if ( $listLength ) {
	    if ( $list ne '' ) {
		$list .= '/';
	    }
	    if ( $listLength > 1 ) {
		$list .= &Disassemble68K_An($listStart).'-'.&Disassemble68K_An($listStart+$listLength-1);
	    } else {
		$list .= &Disassemble68K_An($listStart);
	    }
	}
    }
    return ($problem,$list,$width);
}

# pass in [0] allowed effective address types
# returns [1] DIS_PRBM to note any disassembly problems
# pass in [2] address of start of instruction
# pass in [3] current width of decoded instruction (to determine next extension word location)
# pass in [4] size field from instruction (used for immediate effective address type)
# pass in [5] mode field from instruction
# pass in [6] reg field from instruction
# pass in [7] code pointer (1 if effective address points to code aka JSR/JMP)
# pass in [8] odd byte check (example PEA does not need this)
# returns [0] DIS_PRBM to note any disassembly problems
# returns [1] string representation of effective address
# returns [2] updated width in bytes
sub Disassemble68K_EffectiveAddress {
    my ($allowed,$problem,$addr,$width,$size,$mode,$reg,$code,$oddByteCheck) = @_;
    my $ea = '';
    if ( $mode == 0x0000 ) {
	if ( $allowed & DIS_68K_EA_DN ) {
	    $ea = &Disassemble68K_Dn($reg);
	} else {
	    $problem |= DIS_PRBM_UNSUPPORTED;
	}
    } elsif ( $mode == 0x0008 ) {
	if ( ( $allowed & DIS_68K_EA_AN ) &&
	     $size != DIS_68K_SIZE_BYTE ) {
	    $ea = &Disassemble68K_An($reg);
	} else {
	    $problem |= DIS_PRBM_UNSUPPORTED;
	}
    } elsif ( $mode == 0x0010 ) {
	if ( $allowed & DIS_68K_EA_MEM_AN ) {
	    $ea = &Disassemble68K_Mem_An($reg);
	} else {
	    $problem |= DIS_PRBM_UNSUPPORTED;
	}
    } elsif ( $mode == 0x0018 ) {
	if ( $allowed & DIS_68K_EA_MEM_AN_POSTINC ) {
	    $ea = &Disassemble68K_Mem_An_PostInc($reg);
	} else {
	    $problem |= DIS_PRBM_UNSUPPORTED;
	}
    } elsif ( $mode == 0x0020 ) {
	if ( $allowed & DIS_68K_EA_MEM_AN_PREDEC ) {
	    $ea = &Disassemble68K_Mem_An_PreDec($reg);
	} else {
	    $problem |= DIS_PRBM_UNSUPPORTED;
	}
    } elsif ( $mode == 0x0028 ) {
	if ( $allowed & DIS_68K_EA_MEM_D16_AN ) {
	    my $data2 = &GetWord($addr+$width);
	    $width += 2;
	    # TODO not a real problem but not recommended
	    # option to complain?
	    #if ( $oddByteCheck &&
	    #     $size != DIS_68K_SIZE_BYTE &&
	    #     ( $data2 & 0x0001 ) ) {
	    # $problem |= DIS_PRBM_UNOPT;
	    #}
	    ($problem,$ea) = &Disassemble68K_Mem_D16_An($problem,$data2,$reg);
	} else {
	    $problem |= DIS_PRBM_UNSUPPORTED;
	}
    } elsif ( $mode == 0x0030 ) {
	if ( $allowed & DIS_68K_EA_MEM_D8_AN_XN ) {
	    my $data2   = &GetWord($addr+$width);
	    $width += 2;
	    my $regXType =   $data2         & 0x8000;
	    my $regX     = ( $data2 >> 12 ) & 0x0007;
	    my $regXSize =   $data2         & 0x0800;
	    my $d8       =   $data2         & 0x00FF;
	    if ( $data2 & 0x0700 ) {
		$problem |= DIS_PRBM_IGNORED;
	    }
	    # TODO not a real problem but not recommended
	    # for example An is odd address, or intentionally want odd addr behavior
	    if ( $oddByteCheck &&
		 $size != DIS_68K_SIZE_BYTE &&
		 ( $d8 & 0x0001 ) ) {
		$problem |= DIS_PRBM_UNOPT;
	    }
	    $ea = &Disassemble68K_Mem_D8_An_Xn($d8,$reg,$regX,$regXType,$regXSize);
	} else {
	    $problem |= DIS_PRBM_UNSUPPORTED;
	}
    } elsif ( $mode == 0x0038 ) {
	if ( $reg == 0x0000 ) {
	    if ( $allowed & DIS_68K_EA_MEM_D16 ) {
		my $data2 = &GetWord($addr+$width);
		$width += 2;
		# TODO asl does allow this on MOVE.[WL], so where is this from?
		# TODO asl does not allow this (sometimes)
		if ( $oddByteCheck && 
		     $size != DIS_68K_SIZE_BYTE &&
		     ( $data2 & 0x0001 ) ) {
		    $problem |= DIS_PRBM_UNOPT;
		}
		my $data2ext = &SignExtendWord($data2);
		# TODO 6 or 8 nibbles?
		# TODO PERF comment?
		my $target = $data2ext & 0xFFFFFF;
		my $problem2 = DIS_PRBM_NONE;
		my $label;
		($problem2,$label) = &LabelGeneric($problem2,$target);
		if ( $code ) {
		    $problem |= $problem2;
		    my $rel = ( $target - ( $addr + 2 ) );
		    if ( ( $rel & 0xFFFFFF80 ) == 0x00000000 ||
			 ( $rel & 0xFFFFFF80 ) == 0xFFFFFF80 ) {
			#$diffEncountered |= DIS_Z80_OPT_JP_TO_JR;
			# can be optimized to short version or BRA/BSR
			&AddUnoptimizedBytes(2);
		    }
		}
		if ( ! $problem2 ) {
		    if ( &GetAssembler() & ASM_GAS ) {
			$ea = $label.$forceWord;
		    } else {
			$ea = '('.$label.')'.$forceWord;
		    }
		} else {
		    if ( &GetAssembler() & ASM_GAS ) {
			$ea = &Disassemble_Value_Long($data2ext).$forceWord;
		    } else {
			$ea = '('.&Disassemble_Value_Long($data2ext).')'.$forceWord;
		    }
		}
		if ( $code ) {
		    &AddToCodePointCache($target);
		}
	    } else {
		$problem |= DIS_PRBM_UNSUPPORTED;
	    }
	} elsif ( $reg == 0x0001 ) {
	    if ( $allowed & DIS_68K_EA_MEM_D32 ) {
		my $data4 = &GetLong($addr+$width);
		$width += 4;
		# TODO needed?
		if ( $oddByteCheck && 
		     $size != DIS_68K_SIZE_BYTE &&
		     ( $data4 & 0x0001 ) ) {
		    $problem |= DIS_PRBM_UNOPT;
		}
		my $optimized = 0;
		my $optimizable = ( ( $data4 & 0x00FF8000 ) == 0x00000000 ||
				    ( $data4 & 0x00FF8000 ) == 0x00FF8000 );
		if ( $optimizable ) {
		    $diffEncountered |= DIS_68K_DIFF_OPT_ABS_WORD;
		    $optimized = 2;
		    &AddUnoptimizedBytes(2);
		}

		# TODO $howToTreatUnused on upper 8 bits?
		# TODO 6 or 8 nibbles?
		my $pcrelopt = $data4-($addr+$width);
		if ( ! $optimizable &&
		     ( ( $pcrelopt & 0xFFFF8000 ) == 0x00000000 ||
		       ( $pcrelopt & 0xFFFF8000 ) == 0xFFFF8000 ) ) {
		    $diffEncountered |= DIS_68K_DIFF_OPT_PC_RELATIVE;
		    if ( $diff & DIS_68K_DIFF_OPT_PC_RELATIVE ) {
			$problem |= DIS_PRBM_UNOPT;
			$optimized = 2;
			&AddUnoptimizedBytes(2);
		    }
		}

		if ( $data4 & 0xFF000000 ) {
		    # TODO unused bits still assemblable?
		    if ( &GetAssembler() & ASM_GAS ) {
			$ea = &Disassemble_Value_Long($data4).$forceLong;
		    } else {
			$ea = '('.&Disassemble_Value_Long($data4).')'.$forceLong;
		    }
		} else {
		    my $target = $data4 & 0xFFFFFF;
		    my $problem2 = DIS_PRBM_NONE;
		    my $label;
		    # TODO PERF comment?
		    ($problem2,$label) = &LabelGeneric($problem2,$target);
		    if ( $code ) {
			$problem |= $problem2;
			my $rel = ( $target - ( $addr + 2 ) );
			if ( ( $rel & 0xFFFFFF80 ) == 0x00000000 ||
			     ( $rel & 0xFFFFFF80 ) == 0xFFFFFF80 ) {
			    #$diffEncountered |= DIS_Z80_OPT_JP_TO_JR;
			    # can be optimized to short version or BRA/BSR
			    &AddUnoptimizedBytes(4-$optimized);
			    $optimized = 4;
			} elsif ( ( $rel & 0xFFFF8000 ) == 0x00000000 ||
				  ( $rel & 0xFFFF8000 ) == 0xFFFF8000 ) {
			    &AddUnoptimizedBytes(2-$optimized);
			    $optimized = 2;
			}
		    }
		    if ( ! $problem2 ) {
			if ( $optimizable ) {
			    if ( &GetAssembler() & ASM_GAS ) {
				$ea = $label.$forceLong;
			    } else {
				$ea = '('.$label.')'.$forceLong;
			    }
			} else {
			    $ea = $label;
			}
			if ( $code ) {
			    &AddToCodePointCache($target);
			}
		    } else {
			if ( $optimizable ) {
			    if ( &GetAssembler() & ASM_GAS ) {
				$ea = &Disassemble_Value_24Bits($data4).$forceLong;
			    } else {
				$ea = '('.&Disassemble_Value_24Bits($data4).')'.$forceLong;
			    }
			} else {
			    $ea = &Disassemble_Value_24Bits($data4);
			}
		    }
		}
	    } else {
		$problem |= DIS_PRBM_UNSUPPORTED;
	    }
	} elsif ( $reg == 0x0002 ) {
	    if ( $allowed & DIS_68K_EA_MEM_D16_PC ) {
		my $target = $addr+$width;
		my $data2 = &GetWord($addr+$width);
		$width += 2;
		$target = ( $target + &SignExtendWord($data2) ) & 0xFFFFFF;
		my $label;
		($problem,$label) = &LabelGeneric($problem,$target);
		if ( &GetAssembler() & ASM_GAS ) {
		    $ea = sprintf("%s@(%s)",$regPC,$label);
		} else {
		    $ea = sprintf("%s(%s)",$label,$regPC);
		}
	    } else {
		$problem |= DIS_PRBM_UNSUPPORTED;
	    }
	} elsif ( $reg == 0x0003 ) {
	    if ( $allowed & DIS_68K_EA_MEM_D8_PC_XN ) {
		my $target = $addr+$width;
		my $data2   = &GetWord($addr+$width);
		$width += 2;
		$target = ( $target + &SignExtendByte($data2) ) & 0xFFFFFF;
		my $regType = $data2 & 0x8000;
		my $regX    = ( $data2 >> 12 ) & 0x0007;
		my $size    = $data2 & 0x0800;
		my $ignored = $data2 & 0x0700;
		my $d8      = $data2 & 0x00FF;
		if ( $data2 & 0x0700 ) {
		    $problem |= DIS_PRBM_IGNORED;
		}
		my $strRegX = &Disassemble68K_Xn($regX,$regType);
		my $strSize = $size ? $forceLong : $forceWord;
		if ( $oddByteCheck && ( $target & 1 ) &&
		     $size != DIS_68K_SIZE_BYTE ) {
		    # TODO what to do for odd 68k labels?
		    # TODO these should work now...
		    $problem |= DIS_PRBM_TODO_BAD;
		} else {
		    my $label;
		    ($problem,$label) = &LabelGeneric($problem,$target);
		    if ( &GetAssembler() & ASM_GAS ) {
			# gas gives the following on zero displacement
			# Error: invalid byte branch offset
			if ( $d8 == 0x00 ) {
			    $ea = sprintf("%s@(%s%s)",$regPC,$strRegX,$strSize);
			} else {
			    $ea = sprintf("%s@(%s,%s%s)",$regPC,$label,$strRegX,$strSize);
			}
		    } else {
			$ea = sprintf("%s(%s,%s%s)",$label,$regPC,$strRegX,$strSize);
		    }
		}
	    } else {
		$problem |= DIS_PRBM_UNSUPPORTED;
	    }
	} elsif ( $reg == 0x0004 ) {
	    if ( $allowed & DIS_68K_EA_IMMEDIATE ) {
		($problem,$ea,$width) = &Disassemble68K_Immediate($problem,$addr,$width,$size);
	    } else {
		$problem |= DIS_PRBM_UNSUPPORTED;
	    }
	} else {
	    $problem |= DIS_PRBM_UNSUPPORTED;
	}
    } else {
	die "Bad mode logic: $mode\n";
    }
    return ($problem,$ea,$width);
}

sub Disassemble68K_Dn {
    my $reg = $_[0];
    return $regD.sprintf("%1.1X",($reg & 0x0007));
}
sub Disassemble68K_An {
    my $reg = $_[0];
    return $regA.sprintf("%1.1X",($reg & 0x0007));
}
sub Disassemble68K_Xn {
    my $reg = $_[0];
    my $regType = $_[1];
    return $regType ? &Disassemble68K_An($reg) : &Disassemble68K_Dn($reg);
}
sub Disassemble68K_Mem_An {
    my $reg = $_[0];
    if ( &GetAssembler() & ASM_GAS ) {
	return &Disassemble68K_An($reg).'@';
    }
    return '('.&Disassemble68K_An($reg).')';
}
sub Disassemble68K_Mem_An_PostInc {
    my $reg = $_[0];
    if ( &GetAssembler() & ASM_GAS ) {
	return &Disassemble68K_An($reg).'@+';
    }
    return '('.&Disassemble68K_An($reg).')+';
}
sub Disassemble68K_Mem_An_PreDec {
    my $reg = $_[0];
    if ( &GetAssembler() & ASM_GAS ) {
	return &Disassemble68K_An($reg).'@-';
    }
    return '-('.&Disassemble68K_An($reg).')';
}
sub Disassemble68K_Mem_D16_An {
    my ($problem,$d16,$reg) = @_;
    if ( $d16 == 0x0000 ) {
	$diffEncountered |= DIS_68K_DIFF_OPT_ZERO_DISPLACEMENT;
	if ( $diff & DIS_68K_DIFF_OPT_ZERO_DISPLACEMENT ) {
	    $problem |= DIS_PRBM_UNOPT;
	    &AddUnoptimizedBytes(2);
	}
    }
    my $ea;
    my $num;
    if ( $d16 < 8 ) {
	# if small displacement, don't use hex notation
	$num = &Disassemble_Value_3Bits($d16);
    } else {
	$num = &Disassemble_Value_Word_Signed($d16);
    }
    if ( &GetAssembler() & ASM_GAS ) {
	# %an(number)
	$ea = sprintf("%s@(%s)",&Disassemble68K_An($reg),$num);
    } else {
	# number(an)
	$ea = sprintf("%s(%s)",$num,&Disassemble68K_An($reg));
    }
    return ($problem,$ea);
}
sub Disassemble68K_Mem_D8_An_Xn {
    my ($d8,$reg,$regX,$regXType,$regXSize) = @_;
    my $strSize = $regXSize ? $forceLong : $forceWord;
    my $an = &Disassemble68K_An($reg);
    my $xn = &Disassemble68K_Xn($regX,$regXType);
    my $ea;
    my $num;
    if ( $d8 < 8 ) {
	# if small displacement, don't use hex notation
	$num = &Disassemble_Value_3Bits($d8);
    } else {
	$num = &Disassemble_Value_Byte_Signed($d8);
    }
    if ( &GetAssembler() & ASM_GAS ) {
	# %an(number,xn)
	$ea = sprintf("%s@(%s,%s%s)",&Disassemble68K_An($reg),$num,$xn,$strSize);
    } else {
	# number(an,xn)
	$ea = sprintf("%s(%s,%s%s)",$num,&Disassemble68K_An($reg),$xn,$strSize);
    }
    return $ea;
}

# TODO other types PC...


# pass in [0] DIS_PRBM to note any disassembly problems
# pass in [1] address of start of instruction
# pass in [2] current width of instruction (example 2 if no other words needed for decode yet)
# pass in [3] size field from instruction (used for immediate effective address type)
# returns [0] DIS_PRBM to note any disassembly problems
# returns [1] string representation of effective address
# returns [2] updated width in bytes
sub Disassemble68K_Immediate {
    my ($problem,$addr,$width,$size) = @_;
    if ( $size == DIS_68K_SIZE_BYTE ) {
	return &Disassemble68K_ImmediateByte($problem,$addr,$width);
    } elsif ( $size == DIS_68K_SIZE_WORD ) {
	return &Disassemble68K_ImmediateWord($problem,$addr,$width);
    } elsif ( $size == DIS_68K_SIZE_LONG ) {
	return &Disassemble68K_ImmediateLong($problem,$addr,$width);
    }
    $problem |= DIS_PRBM_UNSUPPORTED;
    return ($problem,'?',$width);
}
sub Disassemble68K_ImmediateByte {
    my ($problem,$addr,$width) = @_;
    my $data2 = &GetWord($addr+$width);
    $width += 2;
    my $ea;
    if ( $data2 & 0xFF00 ) {
	$diffEncountered |= DIS_68K_DIFF_IMMEDIATE_BYTE_IGNORED_NONZERO;
	if ( $diff & DIS_68K_DIFF_IMMEDIATE_BYTE_IGNORED_NONZERO ) {
	    $problem |= DIS_PRBM_IGNORED;
	}
	$ea = &Disassemble68K_Immediate_16Bits($data2);
    } else {
	$data2 = $data2 & 0x00FF;
	$ea = &Disassemble68K_Immediate_8Bits($data2);
    }
    return ($problem,$ea,$width);
}
sub Disassemble68K_ImmediateWord {
    my ($problem,$addr,$width) = @_;
    my $data2 = &GetWord($addr+$width);
    $width += 2;
    my $ea = &Disassemble68K_Immediate_16Bits($data2);
    return ($problem,$ea,$width);
}
sub Disassemble68K_ImmediateWord_Signed {
    my ($problem,$addr,$width) = @_;
    my $data2 = &GetWord($addr+$width);
    $width += 2;
    my $ea = &Disassemble68K_Immediate_16Bits_Signed($data2);
    return ($problem,$ea,$width);
}
sub Disassemble68K_ImmediateLong {
    my ($problem,$addr,$width) = @_;
    my $data4 = &GetLong($addr+$width);
    $width += 4;
    my $ea = &Disassemble68K_Immediate_32Bits($data4);
    return ($problem,$ea,$width);
}

sub Disassemble68K_GetImmediate {
    my ($addr,$width,$size) = @_;
    if ( $size == DIS_68K_SIZE_BYTE ) {
	return &GetByte($addr+$width+1);
    } elsif ( $size == DIS_68K_SIZE_WORD ) {
	return &GetWord($addr+$width);
    } elsif ( $size == DIS_68K_SIZE_LONG ) {
	return &GetLong($addr+$width);
    }
    return 0;
}

# TODO move to DisCommon?
sub Disassemble68K_Immediate_3Bits {
    return '#'.&Disassemble_Value_3Bits($_[0]);
}
sub Disassemble68K_Immediate_4Bits {
    return '#'.&Disassemble_Value_Nibble($_[0]);
}
sub Disassemble68K_Immediate_8Bits {
    return '#'.&Disassemble_Value_Byte($_[0]);
}
sub Disassemble68K_Immediate_8Bits_Signed {
    return '#'.&Disassemble_Value_Byte_Signed($_[0]);
}
sub Disassemble68K_Immediate_16Bits {
    return '#'.&Disassemble_Value_Word($_[0]);
}
sub Disassemble68K_Immediate_16Bits_Signed {
    return '#'.&Disassemble_Value_Word_Signed($_[0]);
}
sub Disassemble68K_Immediate_32Bits {
    return '#'.&Disassemble_Value_Long($_[0]);
}


# pass in [0] mode field from instruction
# pass in [1] reg field from instruction
# returns     string representation of effective address
sub Disassemble68K_DecimalRegister {
    my ($mode,$reg) = @_;
    return ( $mode & 0x0008 ) ? &Disassemble68K_Mem_An_PreDec($reg) : &Disassemble68K_Dn($reg);
}

sub Disassemble68K_Size {
    my ($problem,$size) = @_;
    my $str;
    if ( $size == DIS_68K_SIZE_BYTE ) {
	$str = '.B';
    } elsif ( $size == DIS_68K_SIZE_WORD ) {
	$str = '.W';
    } elsif ( $size == DIS_68K_SIZE_LONG ) {
	$str = '.L';
    } else {
	$problem |= DIS_PRBM_UNSUPPORTED;
	$str = '.?';
    }
    return ($problem,$str);
}


sub Disassemble68K_Condition {
    my $condition = $_[0];
    my $str;
    if ( $condition == 0x0000 ) {
	$str = 'T';
    } elsif ( $condition == 0x0100 ) {
	$str = 'F';
    } elsif ( $condition == 0x0200 ) {
	$str = 'HI';
    } elsif ( $condition == 0x0300 ) {
	$str = 'LS';
    } elsif ( $condition == 0x0400 ) {
	# also called HI
	$str = 'CC';
    } elsif ( $condition == 0x0500 ) {
	# also called LO
	$str = 'CS';
    } elsif ( $condition == 0x0600 ) {
	$str = 'NE';
    } elsif ( $condition == 0x0700 ) {
	$str = 'EQ';
    } elsif ( $condition == 0x0800 ) {
	$str = 'VC';
    } elsif ( $condition == 0x0900 ) {
	$str = 'VS';
    } elsif ( $condition == 0x0A00 ) {
	$str = 'PL';
    } elsif ( $condition == 0x0B00 ) {
	$str = 'MI';
    } elsif ( $condition == 0x0C00 ) {
	$str = 'GE';
    } elsif ( $condition == 0x0D00 ) {
	$str = 'LT';
    } elsif ( $condition == 0x0E00 ) {
	$str = 'GT';
    } elsif ( $condition == 0x0F00 ) {
	$str = 'LE';
    } else {
	$str = '?';
	die "Bad condition logic: $condition\n";
    }
    return $str ;
}

# can pass in 1, 3, or 4 parameters
# pass in [0] mnemonic sans without any size
# pass in [1] size suffix for mnemonic
# pass in [2] operand 1
# pass in [3] operand 2
# returns     instruction as single assembly line

sub Disassemble68K_Setup {
    $diff = 0;
    my $forcePrefix = '.';
    my $regPrefix = '';

    if ( &GetAssembler() & ASM_ASL ) {
	$diff |= DIS_68K_DIFF_ASL;
    }
    if ( &GetAssembler() & ASM_ASMX ) {
	$diff |= DIS_68K_DIFF_ASMX;
    }
    if ( &GetAssembler() & ASM_GAS ) {
	$diff |= DIS_68K_DIFF_GAS;
	$regPrefix = '%';
	$forcePrefix = ':';
    }
    if ( &GetAssembler() & ASM_GAS_MRI ) {
	$diff |= DIS_68K_DIFF_GAS;
    }
    if ( &GetAssembler() & ASM_SNASM68K ) {
	$diff |= DIS_68K_DIFF_SNASM68K;
    }
    if ( &GetAssembler() & ASM_VASM ) {
	$diff |= DIS_68K_DIFF_VASM;
    }

    $regD    = $regPrefix . &ApplyCaseReg('D');
    $regA    = $regPrefix . &ApplyCaseReg('A');
    $regPC   = $regPrefix . &ApplyCaseReg('PC');
    $regCCR  = $regPrefix . &ApplyCaseReg('CCR');
    $regSR   = $regPrefix . &ApplyCaseReg('SR');
    $regUSP  = $regPrefix . &ApplyCaseReg('USP');

    $forceWord = $forcePrefix . &ApplyCaseMnem('W');
    $forceLong = $forcePrefix . &ApplyCaseMnem('L');
}

sub Disassemble68K {
    my $addr = $_[0];

    my $problem = DIS_PRBM_NONE;
    my $inst = '';
    my $width = 2;

    # since most instructions are sequential, assume sequential
    my $seq = DATA_USAGE_NEXT_SEQUENTIAL;

    if ( $addr & 1 ) {
	# odd data address not supported for 68K instructions
	$problem |= DIS_PRBM_UNSUPPORTED;
	return ($problem,$seq,$inst,$width);
    }

    my $data = &GetWord($addr);

    # common parts of most instructions
    my $opHi  = ( $data & 0xF000 );
    my $opMid = ( $data & 0x0E00 );
    my $opLo  = ( $data & 0x0100 );
    my $size  = ( $data & 0x00C0 );
    my $mode  = ( $data & 0x0038 );
    my $reg   = ( $data & 0x0007 );

    # TODO addressing modes check

    if ( $opHi == 0x0000 ) {
	# 0b0000
	if ( $opLo == 0x0000 ) {
	    # 0b0000_xxx0
	    if ( $opMid == 0x0000 ) {
		# 0b0000_0000
		# ORI generic, ORI to CCR, ORI to SR
		if ( ( $size | $mode | $reg ) == 0x003C ) {
		    # ORI to CCR
		    my $ea;
		    ($problem,$ea,$width) = &Disassemble68K_ImmediateByte($problem,$addr,$width);
		    $inst = &Disassemble_Instruction('ORI',$ea,$regCCR);
		} elsif ( ( $size | $mode | $reg ) == 0x007C ) {
		    # ORI to SR
		    my $ea;
		    ($problem,$ea,$width) = &Disassemble68K_ImmediateWord($problem,$addr,$width);
		    $inst = &Disassemble_Instruction('ORI',$ea,$regSR);
		} else {
		    # ORI generic
		    my $strSize;
		    ($problem,$strSize) = &Disassemble68K_Size($problem,$size);
		    my $ea1;
		    ($problem,$ea1,$width) = &Disassemble68K_Immediate($problem,$addr,$width,$size);
		    my $ea2;
		    ($problem,$ea2,$width) = &Disassemble68K_EffectiveAddress(DIS_68K_EA_DATA_ALTERABLE,$problem,$addr,$width,$size,$mode,$reg,0,1);
		    $inst = &Disassemble_Instruction('ORI'.$strSize,$ea1,$ea2);
		}
	    } elsif ( $opMid == 0x0200 ) {
		# 0b0000_0010
		# ANDI generic, ANDI to CCR, ANDI to SR
		if ( ( $size | $mode | $reg ) == 0x003C ) {
		    # ANDI to CCR
		    my $ea;
		    ($problem,$ea,$width) = &Disassemble68K_ImmediateByte($problem,$addr,$width);
		    $inst = &Disassemble_Instruction('ANDI',$ea,$regCCR);
		} elsif ( ( $size | $mode | $reg ) == 0x007C ) {
		    # ANDI to SR
		    my $ea;
		    ($problem,$ea,$width) = &Disassemble68K_ImmediateWord($problem,$addr,$width);
		    $inst = &Disassemble_Instruction('ANDI',$ea,$regSR);
		} else {
		    # ANDI generic
		    my $strSize;
		    ($problem,$strSize) = &Disassemble68K_Size($problem,$size);
		    my $ea1;
		    ($problem,$ea1,$width) = &Disassemble68K_Immediate($problem,$addr,$width,$size);
		    my $ea2;
		    ($problem,$ea2,$width) = &Disassemble68K_EffectiveAddress(DIS_68K_EA_DATA_ALTERABLE,$problem,$addr,$width,$size,$mode,$reg,0,1);
		    $inst = &Disassemble_Instruction('ANDI'.$strSize,$ea1,$ea2);
		}
	    } elsif ( $opMid == 0x0400 ) {
		# 0b0000_0100
		# SUBI
		my $imm = &Disassemble68K_GetImmediate($addr,$width,$size);
		if ( $imm > 0 && $imm <= 8 ) {
		    $diffEncountered |= DIS_68K_DIFF_OPT_SUB_TO_SUBQ;
		    if ( $size == DIS_68K_SIZE_LONG ) {
			&AddUnoptimizedBytes(4);
		    } else {
			&AddUnoptimizedBytes(2);
		    }
		}
		my $strSize;
		($problem,$strSize) = &Disassemble68K_Size($problem,$size);
		my $ea1;
		($problem,$ea1,$width) = &Disassemble68K_Immediate($problem,$addr,$width,$size);
		my $ea2;
		($problem,$ea2,$width) = &Disassemble68K_EffectiveAddress(DIS_68K_EA_DATA_ALTERABLE,$problem,$addr,$width,$size,$mode,$reg,0,1);
		$inst = &Disassemble_Instruction('SUBI'.$strSize,$ea1,$ea2);
	    } elsif ( $opMid == 0x0600 ) {
		# 0b0000_0110
		# ADDI
		my $imm = &Disassemble68K_GetImmediate($addr,$width,$size);
		if ( $imm > 0 && $imm <= 8 ) {
		    $diffEncountered |= DIS_68K_DIFF_OPT_ADD_TO_ADDQ;
		    if ( $size == DIS_68K_SIZE_LONG ) {
			&AddUnoptimizedBytes(4);
		    } else {
			&AddUnoptimizedBytes(2);
		    }
		}
		my $strSize;
		($problem,$strSize) = &Disassemble68K_Size($problem,$size);
		my $ea1;
		($problem,$ea1,$width) = &Disassemble68K_Immediate($problem,$addr,$width,$size);
		my $ea2;
		($problem,$ea2,$width) = &Disassemble68K_EffectiveAddress(DIS_68K_EA_DATA_ALTERABLE,$problem,$addr,$width,$size,$mode,$reg,0,1);
		$inst = &Disassemble_Instruction('ADDI'.$strSize,$ea1,$ea2);
	    } elsif ( $opMid == 0x0800 ) {
		# 0b0000_1000
		# BTST, BCHG, BCLR, BSET (immediate)
		my $mnem;
		if ( $size == 0x0000 ) {
		    $mnem = 'BTST';
		} elsif ( $size == 0x0040 ) {
		    $mnem = 'BCHG';
		} elsif ( $size == 0x0080 ) {
		    $mnem = 'BCLR';
		} elsif ( $size == 0x00C0 ) {
		    $mnem = 'BSET';
		} else {
		    die "Bad logic Bxxx immediate\n";
		}
		my $data2 = &GetWord($addr+$width);
		$width += 2;
		my $num;
		if ( $data2 & 0xFF00 ) {
		    # PRM says bit number if 8 bits
		    # SNASM68K and asl do not allow this
		    # asmx allows it but masks the bits to zero
		    $diffEncountered |= DIS_68K_DIFF_BIT_NUMBER_BITS_UPPER_BYTE;
		    if ( $diff & DIS_68K_DIFF_BIT_NUMBER_BITS_UPPER_BYTE ) {
			$problem |= DIS_PRBM_IGNORED;
		    }
		}
		if ( $mode == 0x0000 ) {
		    # allow 0-31
		    $num = &Disassemble68K_Immediate_8Bits($data2 & 0x00FF);
		    $size = DIS_68K_SIZE_LONG;
		    if ( $data2 & 0xFFE0 ) {
			$diffEncountered |= DIS_68K_DIFF_BIT_NUMBER_BITS_LOWER_BYTE;
			if ( $diff & DIS_68K_DIFF_BIT_NUMBER_BITS_LOWER_BYTE ) {
			    $problem |= DIS_PRBM_IGNORED;
			}
		    }
		} else {
		    # allow 0-7
		    if ( $data2 & 0x00F8 ) {
			$num = &Disassemble68K_Immediate_8Bits($data2 & 0x00FF);
		    } else {
			$num = &Disassemble68K_Immediate_3Bits($data2 & 0x0007);
		    }
		    $size = DIS_68K_SIZE_BYTE;
		    if ( $data2 & 0xFFF8 ) {
			$diffEncountered |= DIS_68K_DIFF_BIT_NUMBER_BITS_LOWER_BYTE;
			if ( $diff & DIS_68K_DIFF_BIT_NUMBER_BITS_LOWER_BYTE ) {
			    $problem |= DIS_PRBM_IGNORED;
			}
		    }
		}
		my $ea;
		($problem,$ea,$width) = &Disassemble68K_EffectiveAddress(DIS_68K_EA_DATA_ALTERABLE,$problem,$addr,$width,$size,$mode,$reg,0,1);
		$inst = &Disassemble_Instruction($mnem,$num,$ea);
	    } elsif ( $opMid == 0x0A00 ) {
		# 0b0000_1010
		# EORI generic, EORI to CCR, EORI to SR
		if ( ( $size | $mode | $reg ) == 0x003C ) {
		    # EORI to CCR
		    my $ea;
		    ($problem,$ea,$width) = &Disassemble68K_ImmediateByte($problem,$addr,$width);
		    $inst = &Disassemble_Instruction('EORI',$ea,$regCCR);
		} elsif ( ( $size | $mode | $reg ) == 0x007C ) {
		    # EORI to SR
		    my $ea;
		    ($problem,$ea,$width) = &Disassemble68K_ImmediateWord($problem,$addr,$width);
		    $inst = &Disassemble_Instruction('EORI',$ea,$regSR);
		} else {
		    # EORI generic
		    my $strSize;
		    ($problem,$strSize) = &Disassemble68K_Size($problem,$size);
		    my $ea1;
		    ($problem,$ea1,$width) = &Disassemble68K_Immediate($problem,$addr,$width,$size);
		    my $ea2;
		    ($problem,$ea2,$width) = &Disassemble68K_EffectiveAddress(DIS_68K_EA_DATA_ALTERABLE,$problem,$addr,$width,$size,$mode,$reg,0,1);
		    $inst = &Disassemble_Instruction('EORI'.$strSize,$ea1,$ea2);
		}
	    } elsif ( $opMid == 0x0C00 ) {
		# 0b0000_1100
		# CMPI
		my $strSize;
		($problem,$strSize) = &Disassemble68K_Size($problem,$size);
		my $ea1;
		($problem,$ea1,$width) = &Disassemble68K_Immediate($problem,$addr,$width,$size);
		my $ea2;
		# PRM states data addressing but table is correct
		# later versions allow more EA types
		($problem,$ea2,$width) = &Disassemble68K_EffectiveAddress(DIS_68K_EA_DATA_ALTERABLE,$problem,$addr,$width,$size,$mode,$reg,0,1);
		$inst = &Disassemble_Instruction('CMPI'.$strSize,$ea1,$ea2);
	    }
	} else {
	    # 0b0000_xxx1
	    if ( $mode == 0x0008 ) {
		# MOVEP
		my $strSize = ( $size & 0x0040 ) ? '.L' : '.W';
		my $dn = ( $data >> 9 ) & 0x0007;
		my $strDn = &Disassemble68K_Dn($dn);
		my $data2 = &GetWord($addr+$width);
		$width += 2;
		my $ea;
		($problem,$ea) = &Disassemble68K_Mem_D16_An($problem,$data2,$reg);
		if ( ( $size & 0x0080 ) == 0x0000 ) {
		    $inst = &Disassemble_Instruction('MOVEP'.$strSize,$ea,$strDn);
		} else {
		    $inst = &Disassemble_Instruction('MOVEP'.$strSize,$strDn,$ea);
		}
	    } else {
		# BTST, BCHG, BCLR, BSET (Dn)
		my $mnem;
		if ( $size == 0x0000 ) {
		    $mnem = 'BTST';
		} elsif ( $size == 0x0040 ) {
		    $mnem = 'BCHG';
		} elsif ( $size == 0x0080 ) {
		    $mnem = 'BCLR';
		} elsif ( $size == 0x00C0 ) {
		    $mnem = 'BSET';
		} else {
		    die "Bad logic Bxxx Dn\n";
		}
		my $dn = ( $data >> 9 ) & 0x0007;
		my $strDn = &Disassemble68K_Dn($dn);
		my $ea;
		($problem,$ea,$width) = &Disassemble68K_EffectiveAddress(DIS_68K_EA_DATA_ALTERABLE,$problem,$addr,$width,$size,$mode,$reg,0,1);
		$inst = &Disassemble_Instruction($mnem,$strDn,$ea);
	    }
	}
    } elsif ( ( $opHi & 0xC000 ) == 0x0000 ) {
	# 0b00xx (where xx is not 00)
	# MOVE or MOVEA
	my $destReg  = ( $data >> 9 ) & 0x0007;
	my $destMode = ( $data >> 3 ) & 0x0038;
	my $strSize;
	if ( ( $opHi & 0x3000 ) == 0x1000 ) {
	    $strSize = '.B';
	    $size = DIS_68K_SIZE_BYTE;
	} elsif ( ( $opHi & 0x3000 ) == 0x3000 ) {
	    $strSize = '.W';
	    $size = DIS_68K_SIZE_WORD;
	} elsif ( ( $opHi & 0x3000 ) == 0x2000 ) {
	    $strSize = '.L';
	    $size = DIS_68K_SIZE_LONG;
	} else {
	    $strSize = '.?';
	    die "Bad logic\n";
	}
	if ( $destMode == 0x0008 ) {
	    # MOVEA
	    if ( $size != DIS_68K_SIZE_BYTE ) {
		my $eaDest = &Disassemble68K_An($destReg);
		my $eaSource;
		($problem,$eaSource,$width) = &Disassemble68K_EffectiveAddress(DIS_68K_EA_ALL,$problem,$addr,$width,$size,$mode,$reg,0,1);
		$inst = &Disassemble_Instruction('MOVEA'.$strSize,$eaSource,$eaDest);
	    }
	} else {
	    # MOVE
	    if ( ( $mode | $reg ) == 0x3C &&
		 $destMode == 0x00 &&
		 $size == DIS_68K_SIZE_LONG ) {
		my $imm = &Disassemble68K_GetImmediate($addr,$width,$size);
		if ( $imm >= 0x00000000 && $imm <= 0x0000007F ||
		     $imm >= 0xFFFFFF80 && $imm <= 0xFFFFFFFF ) {
		    $diffEncountered |= DIS_68K_DIFF_OPT_MOVE_TO_MOVEQ;
		    &AddUnoptimizedBytes(4);
		    if ( $diff & DIS_68K_DIFF_OPT_MOVE_TO_MOVEQ ) {
			$problem |= DIS_PRBM_UNOPT;
		    }
		}
	    }
	    my $eaSource;
	    my $eaDest;
	    ($problem,$eaSource,$width) = &Disassemble68K_EffectiveAddress(DIS_68K_EA_ALL,$problem,$addr,$width,$size,$mode,$reg,0,1);
	    ($problem,$eaDest,$width) = &Disassemble68K_EffectiveAddress(DIS_68K_EA_DATA_ALTERABLE,$problem,$addr,$width,$size,$destMode,$destReg,0,1);
	    $inst = &Disassemble_Instruction('MOVE'.$strSize,$eaSource,$eaDest);
	}
    } elsif ( $opHi == 0x4000 ) {
	# 0b0100
	if ( $opLo == 0x0000 ) {
	    # 0b0100_xxx0
	    if ( $opMid == 0x0000 ) {
		# 0b0100_0000
		if ( $size == 0x00C0 ) {
		    # 0b0100_0000_11
		    # MOVE from SR
		    my $ea;
		    $size = DIS_68K_SIZE_WORD;
		    ($problem,$ea,$width) = &Disassemble68K_EffectiveAddress(DIS_68K_EA_DATA_ALTERABLE,$problem,$addr,$width,$size,$mode,$reg,0,1);
		    $inst = &Disassemble_Instruction('MOVE',$regSR,$ea);
		} else {
		    # 0b0100_0000_xx (where xx is not 11)
		    # NEGX
		    my $strSize;
		    ($problem,$strSize) = &Disassemble68K_Size($problem,$size);
		    my $ea;
		    ($problem,$ea,$width) = &Disassemble68K_EffectiveAddress(DIS_68K_EA_DATA_ALTERABLE,$problem,$addr,$width,$size,$mode,$reg,0,1);
		    $inst = &Disassemble_Instruction('NEGX'.$strSize,$ea);
		}
	    } elsif ( $opMid == 0x0200 ) {
		# 0b0100_0010
		# CLR
		my $strSize;
		($problem,$strSize) = &Disassemble68K_Size($problem,$size);
		my $ea;
		($problem,$ea,$width) = &Disassemble68K_EffectiveAddress(DIS_68K_EA_DATA_ALTERABLE,$problem,$addr,$width,$size,$mode,$reg,0,1);
		$inst = &Disassemble_Instruction('CLR'.$strSize,$ea);
	    } elsif ( $opMid == 0x0400 ) {
		# 0b0100_0100
		if ( $size == 0x00C0 ) {
		    # 0b0100_0100_11
		    # MOVE to CCR
		    $size = DIS_68K_SIZE_WORD;
		    if ( ( $mode | $reg ) == 0x3C ) {
			# not allow data in upper byte of immediate
			$diffEncountered |= DIS_68K_DIFF_NON_ZERO_UPPER_BYTE_MOVE_TO_CCR;
			if ( $diff & DIS_68K_DIFF_NON_ZERO_UPPER_BYTE_MOVE_TO_CCR ) {
			    $problem |= DIS_PRBM_UNREPRESENTABLE;
			}
		    }
		    my $ea;
		    ($problem,$ea,$width) = &Disassemble68K_EffectiveAddress(DIS_68K_EA_DATA_ADDRESSING,$problem,$addr,$width,$size,$mode,$reg,0,1);
		    $inst = &Disassemble_Instruction('MOVE',$ea,$regCCR);
		} else {
		    # 0b0100_0100_xx (where xx is not 11)
		    # NEG
		    my $strSize;
		    ($problem,$strSize) = &Disassemble68K_Size($problem,$size);
		    my $ea;
		    ($problem,$ea,$width) = &Disassemble68K_EffectiveAddress(DIS_68K_EA_DATA_ALTERABLE,$problem,$addr,$width,$size,$mode,$reg,0,1);
		    $inst = &Disassemble_Instruction('NEG'.$strSize,$ea);
		}
	    } elsif ( $opMid == 0x0600 ) {
		# 0b0100_0110
		if ( $size == 0x00C0 ) {
		    # 0b0100_0110_11
		    # MOVE to SR
		    my $ea;
		    $size = DIS_68K_SIZE_WORD;
		    ($problem,$ea,$width) = &Disassemble68K_EffectiveAddress(DIS_68K_EA_DATA_ADDRESSING,$problem,$addr,$width,$size,$mode,$reg,0,1);
		    $inst = &Disassemble_Instruction('MOVE',$ea,$regSR);
		} else {
		    # 0b0100_0110_xx (where xx is not 11)
		    # NOT
		    my $strSize;
		    ($problem,$strSize) = &Disassemble68K_Size($problem,$size);
		    my $ea;
		    ($problem,$ea,$width) = &Disassemble68K_EffectiveAddress(DIS_68K_EA_DATA_ALTERABLE,$problem,$addr,$width,$size,$mode,$reg,0,1);
		    $inst = &Disassemble_Instruction('NOT'.$strSize,$ea);
		}
	    } elsif ( $opMid == 0x0800 ) {
		# 0b0100_1000
		if ( $size == 0x0000 ) {
		    # 0b0100_1000_00
		    # NBCD
		    my $ea;
		    $size = DIS_68K_SIZE_BYTE;
		    ($problem,$ea,$width) = &Disassemble68K_EffectiveAddress(DIS_68K_EA_DATA_ALTERABLE,$problem,$addr,$width,$size,$mode,$reg,0,1);
		    $inst = &Disassemble_Instruction('NBCD',$ea);
		} elsif ( $size == 0x0040 ) {
		    # 0b0100_1000_01
		    if ( $mode == 0x0000 ) {
			# SWAP
			my $ea = &Disassemble68K_Dn($reg);
			$inst = &Disassemble_Instruction('SWAP',$ea);
		    } else {
			# PEA
			my $ea;
			$size = DIS_68K_SIZE_LONG;
			# since not really accessing data, don't need to check for odd byte
			($problem,$ea,$width) = &Disassemble68K_EffectiveAddress(DIS_68K_EA_CONTROL_ADDRESSING,$problem,$addr,$width,$size,$mode,$reg,0,0);
			$inst = &Disassemble_Instruction('PEA',$ea);
		    }
		} else {
		    # 0b0100_1000_1
		    if ( $mode == 0x0000 ) {
			# EXT
			my $strSize;
			if ( $size == 0x0080 ) {
			    $strSize = '.W';
			} elsif ( $size == 0x00C0 ) {
			    $strSize = '.L';
			} else {
			    $strSize = '.?';
			    die "Bad logic\n";
			}
			my $ea = &Disassemble68K_Dn($reg);
			$inst = &Disassemble_Instruction('EXT'.$strSize,$ea);
		    } else {
			# MOVEM (register to memory)
			my $strSize = $size == 0x0080 ? '.W' : '.L';
			my $dir = $mode == 0x0020 ? 1 : 0;
			my $list;
			($problem,$list,$width) = &Disassemble68K_RegisterList($problem,$dir,$addr,$width);
			my $ea;
			($problem,$ea,$width) = &Disassemble68K_EffectiveAddress(DIS_68K_EA_CONTROL_ALTERABLE|DIS_68K_EA_MEM_AN_PREDEC,$problem,$addr,$width,$size,$mode,$reg,0,1);
			$inst = &Disassemble_Instruction('MOVEM'.$strSize,$list,$ea);
		    }
		}
	    } elsif ( $opMid == 0x0A00 ) {
		# 0b0100_1010
		if ( $size == 0x00C0 ) {
		    # 0b0100_1010_11
		    if ( ( $mode | $reg ) == 0x003C ) {
			 # 0b0100_1010_1111_1100
			$inst = &Disassemble_Instruction('ILLEGAL');
			$seq = DATA_USAGE_NEXT_NONSEQUENTIAL;
		    } else {
			# TAS
			my $ea;
			$size = DIS_68K_SIZE_BYTE;
			($problem,$ea,$width) = &Disassemble68K_EffectiveAddress(DIS_68K_EA_DATA_ALTERABLE,$problem,$addr,$width,$size,$mode,$reg,0,1);
			$inst = &Disassemble_Instruction('TAS',$ea);
		    }
		} else {
		    # TST
		    my $strSize;
		    ($problem,$strSize) = &Disassemble68K_Size($problem,$size);
		    my $ea;
		    # later versions allow more EA types
		    ($problem,$ea,$width) = &Disassemble68K_EffectiveAddress(DIS_68K_EA_DATA_ALTERABLE,$problem,$addr,$width,$size,$mode,$reg,0,1);
		    $inst = &Disassemble_Instruction('TST'.$strSize,$ea);
		}
	    } elsif ( $opMid == 0x0C00 ) {
		# 0b0100_1100
		if ( ( $data & 0x0080 ) == 0x0080 ) {
		    # 0b0100_1100_1
		    # MOVEM (memory to register)
		    my $strSize = $size == 0x0080 ? '.W' : '.L';
		    my $list;
		    ($problem,$list,$width) = &Disassemble68K_RegisterList($problem,0,$addr,$width);
		    my $ea;
		    ($problem,$ea,$width) = &Disassemble68K_EffectiveAddress(DIS_68K_EA_CONTROL_ALTERABLE|DIS_68K_EA_MEM_AN_POSTINC,$problem,$addr,$width,$size,$mode,$reg,0,1);
		    $inst = &Disassemble_Instruction('MOVEM'.$strSize,$ea,$list);
		}
	    } elsif ( $opMid == 0x0E00 ) {
		# 0b0100_1110
		if ( $size == 0x0040 ) {
		    # 0b0100_1110_01
		    if ( ( $mode & 0x0030 ) == 0x0000 ) {
			# TRAP
			my $vector = &Disassemble68K_Immediate_4Bits($data & 0x000F);
			$inst = &Disassemble_Instruction('TRAP',$vector);
			$seq = DATA_USAGE_NEXT_NONSEQUENTIAL;
		    } elsif ( $mode == 0x0010 ) {
			# LINK
			my $ea = &Disassemble68K_An($reg);
			my $disp;
			# gas likes signed
			($problem,$disp,$width) = &Disassemble68K_ImmediateWord_Signed($problem,$addr,$width);
			$inst = &Disassemble_Instruction('LINK',$ea,$disp);
		    } elsif ( $mode == 0x0018 ) {
			# UNLK
			my $ea = &Disassemble68K_An($reg);
			$inst = &Disassemble_Instruction('UNLK',$ea);
		    } elsif ( ( $mode & 0x0030 ) == 0x0020 ) {
			# MOVE USP
			my $ea = &Disassemble68K_An($reg);
			# asm68k requires size (weird SNASM68K doesn't though)
			if ( $mode == 0x0020 ) {
			    $inst = &Disassemble_Instruction('MOVE.L',$ea,$regUSP);
			} elsif ( $mode == 0x0028 ) {
			    $inst = &Disassemble_Instruction('MOVE.L',$regUSP,$ea);
			} else {
			    die "Bad logic (MOVE USP)\n";
			}
		    } elsif ( $mode == 0x0030 ) {
			 # 0b0100_1110_0111_0
			if ( $reg == 0x0000 ) {
			    # 0b0100_1110_0111_0000
			    $inst = &Disassemble_Instruction('RESET');
			    $seq = DATA_USAGE_NEXT_NONSEQUENTIAL;
			} elsif ( $reg == 0x0001 ) {
			    # 0b0100_1110_0111_0001
			    $inst = &Disassemble_Instruction('NOP');
			} elsif ( $reg == 0x0002 ) {
			    # 0b0100_1110_0111_0010
			    my $imm;
			    ($problem,$imm,$width) = &Disassemble68K_ImmediateWord($problem,$addr,$width);
			    $inst = &Disassemble_Instruction('STOP',$imm);
			    $seq = DATA_USAGE_NEXT_NONSEQUENTIAL;
			} elsif ( $reg == 0x0003 ) {
			    # 0b0100_1110_0111_0011
			    $inst = &Disassemble_Instruction('RTE');
			    $seq = DATA_USAGE_NEXT_NONSEQUENTIAL;
			} elsif ( $reg == 0x0005 ) {
			    # 0b0100_1110_0111_0101
			    $inst = &Disassemble_Instruction('RTS');
			    $seq = DATA_USAGE_NEXT_NONSEQUENTIAL;
			} elsif ( $reg == 0x0006 ) {
			    # 0b0100_1110_0111_0110
			    $inst = &Disassemble_Instruction('TRAPV');
			} elsif ( $reg == 0x0007 ) {
			    # 0b0100_1110_0111_0111
			    $inst = &Disassemble_Instruction('RTR');
			    $seq = DATA_USAGE_NEXT_NONSEQUENTIAL;
			}
		    }
		} else {
		    # JSR/JMP
		    my $mnem = '';
		    if ( $size == 0x0080 ) {
			# 0b0100_1110_10
			# JSR
			$mnem = 'JSR';
			$seq = DATA_USAGE_NEXT_CONDSEQUENTIAL;
		    } elsif ( $size == 0x00C0 ) {
			# 0b0100_1110_11
			# JMP
			$mnem = 'JMP';
			$seq = DATA_USAGE_NEXT_NONSEQUENTIAL;
		    }
		    if ( $mnem ne '' ) {
			my $ea;
			($problem,$ea,$width) = &Disassemble68K_EffectiveAddress(DIS_68K_EA_CONTROL_ADDRESSING,$problem,$addr,$width,$size,$mode,$reg,1,1);
			$inst = &Disassemble_Instruction($mnem,$ea);
			if ( &GetCodePointNice() ) {
			    &CheckForCodeTables($addr,$reg);
			}
		    }
		}
	    }
	} else {
	    # 0b0100_xxx1
	    if ( $size == 0x0080 ) {
		# CHK
		my $ea;
		$size = DIS_68K_SIZE_WORD;
		($problem,$ea,$width) = &Disassemble68K_EffectiveAddress(DIS_68K_EA_DATA_ADDRESSING,$problem,$addr,$width,$size,$mode,$reg,0,1);
		my $dn = ( $data >> 9 ) & 0x0007;
		my $strDn = &Disassemble68K_Dn($dn);
		$inst = &Disassemble_Instruction('CHK',$ea,$strDn);
	    } elsif ( $size == 0x00C0 ) {
		# LEA
		my $ea1;
		$size = DIS_68K_SIZE_LONG;
		# since not really accessing data, don't need to check for odd byte
		($problem,$ea1,$width) = &Disassemble68K_EffectiveAddress(DIS_68K_EA_CONTROL_ADDRESSING,$problem,$addr,$width,$size,$mode,$reg,0,0);
		my $ea2 = &Disassemble68K_An(($opMid>>9));
		$inst = &Disassemble_Instruction('LEA',$ea1,$ea2);
	    }
	}
    } elsif ( $opHi == 0x5000 ) {
	# 0b0101
	# ADDQ, SUBQ, DBcc, Scc
	if ( $size == 0x00C0 ) {
	    # 0b0101_xxxx_11
	    # DBcc, Scc
	    my $condition = ( $opMid | $opLo );
	    my $strCondition = &Disassemble68K_Condition($condition);
	    if ( $mode == 0x0008 ) {
		# DBcc
		my $strDn = &Disassemble68K_Dn($reg);
		my $target;
		my $data2ext = &SignExtendWord(&GetWord($addr+$width));
		$width += 2;
		$target = ( $addr + 2 + $data2ext ) & 0xFFFFFF;
		my $label;
		($problem,$label) = &LabelGeneric($problem,$target);
		my $mnem = 'DB'.$strCondition;
		$inst = &Disassemble_Instruction($mnem,$strDn,$label);
		if ( $condition == 0x0000 ) {
		    # DBT never branches to target
		    $seq = DATA_USAGE_NEXT_SEQUENTIAL;
		} else {
		    $seq = DATA_USAGE_NEXT_CONDSEQUENTIAL;
		    &AddToCodePointCache($target);
		}
	    } else {
		# Scc
		my $ea;
		$size = DIS_68K_SIZE_BYTE;
		($problem,$ea,$width) = &Disassemble68K_EffectiveAddress(DIS_68K_EA_DATA_ALTERABLE,$problem,$addr,$width,$size,$mode,$reg,0,1);
		my $mnem = 'S'.$strCondition;
		$inst = &Disassemble_Instruction($mnem,$ea);
	    }
	} else {
	    # 0b0101_xxxx_yy (where yy is not 11)
	    # ADDQ, SUBQ
	    my $mnem = $opLo ? 'SUBQ' : 'ADDQ';
	    my $strSize;
	    ($problem,$strSize) = &Disassemble68K_Size($problem,$size);
	    my $imm = ( $data >> 9 ) & 0x0007;
	    if ( $imm == 0 ) {
		$imm = 8;
	    }
	    my $strImm = &Disassemble68K_Immediate_3Bits($imm);
	    my $ea;
	    ($problem,$ea,$width) = &Disassemble68K_EffectiveAddress(DIS_68K_EA_ALTERABLE,$problem,$addr,$width,$size,$mode,$reg,0,1);
	    $inst = &Disassemble_Instruction($mnem.$strSize,$strImm,$ea);
	}
    } elsif ( $opHi == 0x6000 ) {
	# 0b0110
	# BRA, BSR, Bcc
	my $disp = ( $size | $mode | $reg );
	my $target;
	my $strSize;
	if ( $disp == 0x0000 ) {
	    my $data2ext = &SignExtendWord(&GetWord($addr+$width));
	    $width += 2;
	    $target = ( $addr + 2 + $data2ext ) & 0xFFFFFF;
	    if ( $data2ext != 0x00000000 &&
		 ( ( $data2ext & 0x00FFFF80 ) == 0x00000000 ||
		   ( $data2ext & 0x00FFFF80 ) == 0x00FFFF80 ) ) {
		$diffEncountered |= DIS_68K_DIFF_OPT_SHORT_BRANCH;
		&AddUnoptimizedBytes(2);
		$strSize = '.W';
 	    } elsif ( $diff & DIS_68K_DIFF_EARLY_PASSES_ASSUMES_SHORT_BRANCH ) {
		$strSize = '.W';
	    } else {
		$strSize = '';
	    }
	} else {
	    if ( $disp == 0xFF ) {
		$diffEncountered |= DIS_68K_DIFF_SHORT_BRANCH_TO_FF;
		if ( $diff & DIS_68K_DIFF_SHORT_BRANCH_TO_FF ) {
		    $problem |= DIS_PRBM_UNOPT;
		}
	    }
	    $target = ( $addr + 2 + &SignExtendByte($disp) ) & 0xFFFFFF;
	    $strSize = '.S';
	}
	my $labelTarget;
	# TODO what about branches not in code block disassembling?
	($problem,$labelTarget) = &LabelGeneric($problem,$target);
	if ( $opMid == 0x0000 ) {
	    # 0b0110_000
	    if ( $opLo == 0x0000 ) {
		# BRA
		$inst = &Disassemble_Instruction('BRA'.$strSize,$labelTarget);
		$seq = DATA_USAGE_NEXT_NONSEQUENTIAL;
	    } else {
		# BSR
		$inst = &Disassemble_Instruction('BSR'.$strSize,$labelTarget);
		$seq = DATA_USAGE_NEXT_CONDSEQUENTIAL;
	    }
	} else {
	    # Bcc
	    my $condition = ( $opMid | $opLo );
	    my $strCondition = &Disassemble68K_Condition($condition);
	    my $mnem = 'B'.$strCondition;
	    $inst = &Disassemble_Instruction($mnem.$strSize,$labelTarget);
	    $seq = DATA_USAGE_NEXT_CONDSEQUENTIAL;
	}
	&AddToCodePointCache($target);
    } elsif ( $opHi == 0x7000 ) {
	# 0b0111
	if ( $opLo == 0x0000 ) {
	    # 0b0111_xxx0
	    # MOVEQ
	    # TODO gas needs them signed
	    my $d8 = $data & 0x00FF;
	    my $strD8 = &Disassemble68K_Immediate_8Bits_Signed($d8);
	    my $dn = ( $data >> 9 ) & 0x0007;
	    my $strDn = &Disassemble68K_Dn($dn);
	    $inst = &Disassemble_Instruction('MOVEQ',$strD8,$strDn);
	}
    } elsif ( $opHi == 0x8000 ) {
	# 0b1000
	# DIVU, DIVS, SBCD, OR
	if ( $size == 0x00C0 ) {
	    # 0b1000_xxxx_11
	    # DIVU, DIVS
	    my $mnem = $opLo ? 'DIVS.W' : 'DIVU.W';
	    my $dn = ( $data >> 9 ) & 0x0007;
	    my $strDn = &Disassemble68K_Dn($dn);
	    $size = DIS_68K_SIZE_WORD;
	    my $ea;
	    ($problem,$ea,$width) = &Disassemble68K_EffectiveAddress(DIS_68K_EA_DATA_ADDRESSING,$problem,$addr,$width,$size,$mode,$reg,0,1);
	    $inst = &Disassemble_Instruction($mnem,$ea,$strDn);
	} else {
	    if ( ( $data & 0x01F0 ) == 0x0100 ) {
		# 0b1000_xxx1_0000
		# SBCD
		my $regy = ( $data >> 9 ) & 0x0007;
		my $eay = &Disassemble68K_DecimalRegister($mode,$regy);
		my $eax = &Disassemble68K_DecimalRegister($mode,$reg);
		$inst = &Disassemble_Instruction('SBCD',$eax,$eay);
	    } else {
		# OR
		if ( ( $mode | $reg ) == 0x3C ) {
		    # converts OR #<ea>,Dn to ORI
		    $diffEncountered |= DIS_68K_DIFF_MNEM_TO_MNEM_IMMEDIATE_OR_AND;
		    if ( $diff & DIS_68K_DIFF_MNEM_TO_MNEM_IMMEDIATE_OR_AND ) {
			$problem |= DIS_PRBM_UNREPRESENTABLE;
		    }
		}
		my $strSize;
		($problem,$strSize) = &Disassemble68K_Size($problem,$size);
		my $ea;
		my $dn = ( $data >> 9 ) & 0x0007;
		my $strDn = &Disassemble68K_Dn($dn);
		if ( $opLo == 0x0000 ) {
		    ($problem,$ea,$width) = &Disassemble68K_EffectiveAddress(DIS_68K_EA_DATA_ADDRESSING,$problem,$addr,$width,$size,$mode,$reg,0,1);
		    $inst = &Disassemble_Instruction('OR'.$strSize,$ea,$strDn);
		} else {
		    ($problem,$ea,$width) = &Disassemble68K_EffectiveAddress(DIS_68K_EA_MEM_ALTERABLE,$problem,$addr,$width,$size,$mode,$reg,0,1);
		    $inst = &Disassemble_Instruction('OR'.$strSize,$strDn,$ea);
		}
	    }
	}
    } elsif ( ( $opHi & 0xB000 ) == 0x9000 ) {
	# 0b1x01
	# SUB, SUBX, SUBA ( x = 0 )
	# ADD, ADDX, ADDA ( x = 1 )
	my $mnemRoot = ( $opHi & 0x4000 ) ? 'ADD' : 'SUB';
	if ( $size == 0x00C0 ) {
	    # 0b1x01_yyyy_11
	    # SUBA, ADDA
	    my $regy = ( $data >> 9 ) & 0x0007;
	    my $strAn = &Disassemble68K_An($regy);
	    my $strSize = $opLo ? '.L' : '.W';
	    my $size = $opLo ? DIS_68K_SIZE_LONG : DIS_68K_SIZE_WORD;
	    my $ea;
	    ($problem,$ea,$width) = &Disassemble68K_EffectiveAddress(DIS_68K_EA_ALL,$problem,$addr,$width,$size,$mode,$reg,0,1);
	    my $mnem = $mnemRoot.'A';
	    $inst = &Disassemble_Instruction($mnem.$strSize,$ea,$strAn);
	} elsif ( ( $data & 0x0130 ) == 0x0100 ) {
	    # 0b1x01_yyy1_zz00
	    # SUBX, ADDX
	    my $strSize;
	    ($problem,$strSize) = &Disassemble68K_Size($problem,$size);
	    my $regy = ( $data >> 9 ) & 0x0007;
	    my $eay = &Disassemble68K_DecimalRegister($mode,$regy);
	    my $eax = &Disassemble68K_DecimalRegister($mode,$reg);
	    my $mnem = $mnemRoot.'X';
	    $inst = &Disassemble_Instruction($mnem.$strSize,$eax,$eay);
	} else {
	    # SUB, ADD
	    my $strSize;
	    ($problem,$strSize) = &Disassemble68K_Size($problem,$size);
	    my $regy = ( $data >> 9 ) & 0x0007;
	    my $strDn = &Disassemble68K_Dn($regy);
	    my $ea;
	    if ( ( $mode | $reg ) == 0x3C ) {
		# converts ADD/SUB #<ea>,Dn to ADDI/SUBI
		$diffEncountered |= DIS_68K_DIFF_MNEM_TO_MNEM_IMMEDIATE_ADD_SUB;
		if ( $diff & DIS_68K_DIFF_MNEM_TO_MNEM_IMMEDIATE_ADD_SUB ) {
		    $problem |= DIS_PRBM_UNREPRESENTABLE;
		}
	    }
	    if ( ( $mode | $reg ) == 0x3C ) {
		my $imm = &Disassemble68K_GetImmediate($addr,$width,$size);
		if ( $imm > 0 && $imm <= 8 ) {
		    if ( $mnemRoot eq 'ADD' ) {
			$diffEncountered |= DIS_68K_DIFF_OPT_ADD_TO_ADDQ;
		    } elsif ( $mnemRoot eq 'SUB' ) {
			$diffEncountered |= DIS_68K_DIFF_OPT_SUB_TO_SUBQ;
		    } else {
			die "Bad logic: ADD/SUB to ADDQ/SUBQ check\n";
		    }
		    if ( $size == DIS_68K_SIZE_LONG ) {
			&AddUnoptimizedBytes(4);
		    } else {
			&AddUnoptimizedBytes(2);
		    }
		}
	    }
	    if ( $opLo == 0x0000 ) {
		# Dn operation <ea> -> Dn
		($problem,$ea,$width) = &Disassemble68K_EffectiveAddress(DIS_68K_EA_ALL,$problem,$addr,$width,$size,$mode,$reg,0,1);
		$inst = &Disassemble_Instruction($mnemRoot.$strSize,$ea,$strDn);
	    } else {
		# <ea> operation Dn -> <ea>
		($problem,$ea,$width) = &Disassemble68K_EffectiveAddress(DIS_68K_EA_MEM_ALTERABLE,$problem,$addr,$width,$size,$mode,$reg,0,1);
		$inst = &Disassemble_Instruction($mnemRoot.$strSize,$strDn,$ea);
	    }
	}
    } elsif ( $opHi == 0xB000 ) {
	# 0b1011
	# EOR, CMPM, CMP, CMPA
	if ( $size == 0x00C0 ) {
	    # 0b1001_xxxx_11
	    # CMPA
	    # TODO #codeaddress should use label
	    my $regy = ( $data >> 9 ) & 0x0007;
	    my $strAn = &Disassemble68K_An($regy);
	    my $strSize = $opLo ? '.L' : '.W';
	    my $size = $opLo ? DIS_68K_SIZE_LONG : DIS_68K_SIZE_WORD;
	    my $ea;
	    ($problem,$ea,$width) = &Disassemble68K_EffectiveAddress(DIS_68K_EA_ALL,$problem,$addr,$width,$size,$mode,$reg,0,1);
	    $inst = &Disassemble_Instruction('CMPA'.$strSize,$ea,$strAn);
	} else {
	    # 0b1001_xxxx_yy (where yy is no 11)
	    if ( $opLo == 0x0000 ) {
		# 0b1001_xxx0_yy (where yy is no 11)
		# CMP
		my $strSize;
		($problem,$strSize) = &Disassemble68K_Size($problem,$size);
		my $regy = ( $data >> 9 ) & 0x0007;
		my $strDn = &Disassemble68K_Dn($regy);
		my $ea;
		if ( ( $mode | $reg ) == 0x3C ) {
		    # convert CMP #<ea>,Dn to CMPI
		    $diffEncountered |= DIS_68K_DIFF_MNEM_TO_MNEM_IMMEDIATE_CMP;
		    if ( $diff & DIS_68K_DIFF_MNEM_TO_MNEM_IMMEDIATE_CMP ) {
			$problem |= DIS_PRBM_UNREPRESENTABLE;
		    }
		}
		($problem,$ea,$width) = &Disassemble68K_EffectiveAddress(DIS_68K_EA_ALL,$problem,$addr,$width,$size,$mode,$reg,0,1);
		$inst = &Disassemble_Instruction('CMP'.$strSize,$ea,$strDn);
	    } else {
		my $strSize;
		($problem,$strSize) = &Disassemble68K_Size($problem,$size);
		# 0b1001_xxx1_yy (where yy is no 11)
		if ( $mode == 0x0008 ) {
		    # 0b1001_xxx1_yy00_1 (where yy is no 11)
		    # CMPM
		    my $regx = ( $data >> 9 ) & 0x0007;
		    my $ax = &Disassemble68K_Mem_An_PostInc($regx);
		    my $ay = &Disassemble68K_Mem_An_PostInc($reg);
		    $inst = &Disassemble_Instruction('CMPM'.$strSize,$ay,$ax);
		} else {
		    # EOR
		    my $regy = ( $data >> 9 ) & 0x0007;
		    my $strDn = &Disassemble68K_Dn($regy);
		    my $ea;
		    # PRM says immediate not available, but then suggests EORI as alternative
		    ($problem,$ea,$width) = &Disassemble68K_EffectiveAddress(DIS_68K_EA_DATA_ALTERABLE,$problem,$addr,$width,$size,$mode,$reg,0,1);
		    $inst = &Disassemble_Instruction('EOR'.$strSize,$strDn,$ea);
		}
	    }
	}
    } elsif ( $opHi == 0xC000 ) {
	# 0b1100
	# MULU, MULS, ABCD, EXG, AND
	if ( $size == 0x00C0 ) {
	    # 0b1100_xxxx_11
	    # MULU, MULS
	    my $mnem = $opLo ? 'MULS.W' : 'MULU.W';
	    my $dn = ( $data >> 9 ) & 0x0007;
	    my $strDn = &Disassemble68K_Dn($dn);
	    $size = DIS_68K_SIZE_WORD;
	    my $ea;
	    ($problem,$ea,$width) = &Disassemble68K_EffectiveAddress(DIS_68K_EA_DATA_ADDRESSING,$problem,$addr,$width,$size,$mode,$reg,0,1);
	    $inst = &Disassemble_Instruction($mnem,$ea,$strDn);
	} else {
	    # 0b1100_xxxx_yy (where yy is not 11)
	    if ( ( $data & 0x01F0 ) == 0x0100 ) {
		# 0b1100_xxx1_0000
		# ABCD
		my $regy = ( $data >> 9 ) & 0x0007;
		my $eay = &Disassemble68K_DecimalRegister($mode,$regy);
		my $eax = &Disassemble68K_DecimalRegister($mode,$reg);
		$inst = &Disassemble_Instruction('ABCD',$eax,$eay);
	    } elsif ( ( $data & 0x0130 ) == 0x0100 ) {
		# 0b1100_xxx1_mm00_m
		# EXG
		my $eax = '';
		my $eay = '';
		my $regX = ( $data >> 9 ) & 0x0007;
		my $sameRegType = 0;
		if ( ( $data & 0x00F8 ) == 0x0040 ) {
		    $eax = &Disassemble68K_Dn($regX);
		    $eay = &Disassemble68K_Dn($reg);
		    $sameRegType = 1;
		} elsif ( ( $data & 0x00F8 ) == 0x0048 ) {
		    $eax = &Disassemble68K_An($regX);
		    $eay = &Disassemble68K_An($reg);
		    $sameRegType = 1;
		} elsif ( ( $data & 0x00F8 ) == 0x0088 ) {
		    $eax = &Disassemble68K_Dn($regX);
		    $eay = &Disassemble68K_An($reg);
		}
		if ( $sameRegType && $regX > $reg ) {
		    # sorts registers so cannot maintain original order
		    $diffEncountered |= DIS_68K_DIFF_UNSORTED_EXG;
		    if ( $diff & DIS_68K_DIFF_UNSORTED_EXG ) {
			$problem |= DIS_PRBM_UNOPT;
		    }
		}
		if ( $eax ne '' && $eay ne '' ) {
		    $inst = &Disassemble_Instruction('EXG',$eax,$eay);
		}
	    } else {
		# AND
		if ( ( $mode | $reg ) == 0x3C ) {
		    # converts AND #<ea>,Dn to ANDI
		    $diffEncountered |= DIS_68K_DIFF_MNEM_TO_MNEM_IMMEDIATE_OR_AND;
		    if ( $diff & DIS_68K_DIFF_MNEM_TO_MNEM_IMMEDIATE_OR_AND ) {
			$problem |= DIS_PRBM_UNREPRESENTABLE;
		    }
		}
		my $strSize;
		($problem,$strSize) = &Disassemble68K_Size($problem,$size);
		my $ea;
		my $dn = ( $data >> 9 ) & 0x0007;
		my $strDn = &Disassemble68K_Dn($dn);
		if ( $opLo == 0x0000 ) {
		    ($problem,$ea,$width) = &Disassemble68K_EffectiveAddress(DIS_68K_EA_DATA_ADDRESSING,$problem,$addr,$width,$size,$mode,$reg,0,1);
		    $inst = &Disassemble_Instruction('AND'.$strSize,$ea,$strDn);
		} else {
		    ($problem,$ea,$width) = &Disassemble68K_EffectiveAddress(DIS_68K_EA_MEM_ALTERABLE,$problem,$addr,$width,$size,$mode,$reg,0,1);
		    $inst = &Disassemble_Instruction('AND'.$strSize,$strDn,$ea);
		}
	    }
	}
    } elsif ( $opHi == 0xE000 ) {
	# 0b1110_xxxd (d is for direction)
	# shift/rotate instructions
	my $dir = $opLo ? 'L' : 'R';
	if ( $size == 0x00C0 ) {
	    if ( ( $opMid & 0x0800 ) == 0x0000 ) {
		# 0b1110_0xxd_11
		my $mnemRoot;
		if ( $opMid == 0x0000 ) {
		    $mnemRoot = 'AS';
		} elsif ( $opMid == 0x0200 ) {
		    $mnemRoot = 'LS';
		} elsif ( $opMid == 0x0400 ) {
		    $mnemRoot = 'ROX';
		} elsif ( $opMid == 0x0600 ) {
		    $mnemRoot = 'RO';
		}
		my $ea;
		($problem,$ea,$width) = &Disassemble68K_EffectiveAddress(DIS_68K_EA_MEM_ALTERABLE,$problem,$addr,$width,$size,$mode,$reg,0,1);
		my $mnem = $mnemRoot.$dir;
		$inst = &Disassemble_Instruction($mnem,$ea);
	    }
	} else {
	    # 0b1110_xxxd_yy (where yy is not 11)
	    my $rot = ( $data >> 9 ) & 0x0007;
	    my $mnemRoot;
	    my $strSize;
	    ($problem,$strSize) = &Disassemble68K_Size($problem,$size);
	    if ( ( $mode & 0x0018 ) == 0x0000 ) {
		$mnemRoot = 'AS';
	    } elsif ( ( $mode & 0x0018 ) == 0x0008 ) {
		$mnemRoot = 'LS';
	    } elsif ( ( $mode & 0x0018 ) == 0x0010 ) {
		$mnemRoot = 'ROX';
	    } elsif ( ( $mode & 0x0018 ) == 0x0018 ) {
		$mnemRoot = 'RO';
	    }
	    my $strDy = &Disassemble68K_Dn($reg);
	    my $ea;
	    if ( ( $mode & 0x0020 ) == 0x0000 ) {
		if ( $rot == 0 ) {
		    $rot = 8;
		}
		$ea = &Disassemble68K_Immediate_3Bits($rot);
	    } else {
		$ea = &Disassemble68K_Dn($rot);
	    }
	    my $mnem = $mnemRoot.$dir;
	    $inst = &Disassemble_Instruction($mnem.$strSize,$ea,$strDy);
	}
    }

    return ($problem,$seq,$inst,$width);
}

sub GenesisROM {
    # mark all the data locations first
    # then process all the code points found

    if ( ( &GetDataMasks(0x000,0x04) & DATA_USAGE_LABEL_CUSTOM ) == 0 ) {
	&AddComment(0x000,&Disassemble_Comment('Initial Stack Pointer'));
    }
    &SetAssembly(0x000,&Disassemble_Data_Long(0,4));
    &SetDataPoint(0x000,0x04);

    for ( my $addr = 0x004; $addr < 0x100; $addr += 0x04 ) {
	my $vectorUsed = ( ( $addr >= 0x04 && $addr < 0x30 ) ||
			   ( $addr >= 0x60 && $addr < 0xC0 ) );
	my $addrPointer = &GetLong($addr);
	my $problem = DIS_PRBM_NONE;
	my $label;
	my $str;
	if ( $vectorUsed ) {
	    ($problem,$label) = &LabelGeneric($problem,$addrPointer);
	} elsif ( $addrPointer > 0 ) {
	    # many games set unused vectors to same address as another vector
	    # if label already in use we will use that to make it cleaner looking
	    $label = &GetLabel($addrPointer);
	} else {
	    $label = '';
	}
	if ( ! $problem && $label ne '' ) {
	    $str = &Disassemble_Data_Long_Expr($label);
	} else {
	    $str = &Disassemble_Data_Long($addr,4);
	}
	&SetAssembly($addr,$str);
	&SetDataPoint($addr,0x04);
	if ( $vectorUsed ) {
	    &AddToCodePointCache($addrPointer);
	}
    }

    if ( ( &GetDataMasks(0x100,0x10) & DATA_USAGE_LABEL_CUSTOM ) == 0 ) {
	&AddComment(0x100,&Disassemble_Comment('Console'));
    }
    &SetAssembly(0x100,&Disassemble_Data_String(0x100,0x10));
    &SetDataPoint(0x100,0x10);

    if ( ( &GetDataMasks(0x110,0x10) & DATA_USAGE_LABEL_CUSTOM ) == 0 ) {
	&AddComment(0x110,&Disassemble_Comment('CartridgeDate'));
    }
    &SetAssembly(0x110,&Disassemble_Data_String(0x110,0x10));
    &SetDataPoint(0x110,0x10);

    if ( ( &GetDataMasks(0x120,0x30) & DATA_USAGE_LABEL_CUSTOM ) == 0 ) {
	&AddComment(0x120,&Disassemble_Comment('Title_Local'));
    }
    &SetAssembly(0x120,&Disassemble_Data_String(0x120,0x30));
    &SetDataPoint(0x120,0x30);

    if ( ( &GetDataMasks(0x150,0x30) & DATA_USAGE_LABEL_CUSTOM ) == 0 ) {
	&AddComment(0x150,&Disassemble_Comment('Title_Int'));
    }
    &SetAssembly(0x150,&Disassemble_Data_String(0x150,0x30));
    &SetDataPoint(0x150,0x30);

    if ( ( &GetDataMasks(0x180,0x0E) & DATA_USAGE_LABEL_CUSTOM ) == 0 ) {
	&AddComment(0x180,&Disassemble_Comment('Serial'));
    }
    &SetAssembly(0x180,&Disassemble_Data_String(0x180,0x0E));
    &SetDataPoint(0x180,0x0E);

    if ( ( &GetDataMasks(0x18E,0x02) & DATA_USAGE_LABEL_CUSTOM ) == 0 ) {
	&AddComment(0x18E,&Disassemble_Comment('CheckSum'));
    }
    &SetAssembly(0x18E,&Disassemble_Data_Word(0x18E,0x02));
    &SetDataPoint(0x18E,0x02);

    if ( ( &GetDataMasks(0x190,0x10) & DATA_USAGE_LABEL_CUSTOM ) == 0 ) {
	&AddComment(0x190,&Disassemble_Comment('I/O'));
    }
    &SetAssembly(0x190,&Disassemble_Data_String(0x190,0x10));
    &SetDataPoint(0x190,0x10);

    if ( ( &GetDataMasks(0x1A0,0x04) & DATA_USAGE_LABEL_CUSTOM ) == 0 ) {
	&AddComment(0x1A0,&Disassemble_Comment('RomStartLoc'));
    }
    &SetAssembly(0x1A0,&Disassemble_Data_Long(0x1A0,0x04));
    &SetDataPoint(0x1A0,0x04);

    if ( ( &GetDataMasks(0x1A4,0x04) & DATA_USAGE_LABEL_CUSTOM ) == 0 ) {
	&AddComment(0x1A4,&Disassemble_Comment('RomEndLoc'));
    }
    &SetAssembly(0x1A4,&Disassemble_Data_Long(0x1A4,0x04));
    &SetDataPoint(0x1A4,0x04);

    if ( ( &GetDataMasks(0x1A8,0x04) & DATA_USAGE_LABEL_CUSTOM ) == 0 ) {
	&AddComment(0x1A8,&Disassemble_Comment('RamStartLoc'));
    }
    &SetAssembly(0x1A8,&Disassemble_Data_Long(0x1A8,0x04));
    &SetDataPoint(0x1A8,0x04);

    if ( ( &GetDataMasks(0x1AC,0x04) & DATA_USAGE_LABEL_CUSTOM ) == 0 ) {
	&AddComment(0x1AC,&Disassemble_Comment('RamEndLoc'));
    }
    &SetAssembly(0x1AC,&Disassemble_Data_Long(0x1AC,0x04));
    &SetDataPoint(0x1AC,0x04);

    if ( ( &GetDataMasks(0x1B0,0x0C) & DATA_USAGE_LABEL_CUSTOM ) == 0 ) {
	&AddComment(0x1B0,&Disassemble_Comment('SRAMSupport'));
    }
    &SetAssembly(0x1B0,&Disassemble_Data_String(0x1B0,0x04));
    &SetDataPoint(0x1B0,0x04);
    &SetAssembly(0x1B4,&Disassemble_Data_String(0x1B4,0x04));
    &SetDataPoint(0x1B4,0x04);
    &SetAssembly(0x1B8,&Disassemble_Data_String(0x1B8,0x04));
    &SetDataPoint(0x1B8,0x04);

    if ( ( &GetDataMasks(0x1BC,0x34) & DATA_USAGE_LABEL_CUSTOM ) == 0 ) {
	&AddComment(0x1BC,&Disassemble_Comment('Notes'));
    }
    &SetAssembly(0x1BC,&Disassemble_Data_String(0x1BC,0x34));
    &SetDataPoint(0x1BC,0x34);

    if ( ( &GetDataMasks(0x1F0,0x10) & DATA_USAGE_LABEL_CUSTOM ) == 0 ) {
	&AddComment(0x1F0,&Disassemble_Comment('Region'));
    }
    &SetAssembly(0x1F0,&Disassemble_Data_String(0x1F0,0x10));
    &SetDataPoint(0x1F0,0x10);

    &ProcessCodePointCache();
}

sub CheckForCodeTables {
    my ($addr,$reg) = @_;
    # check for CODETBL based on previous instructions
    # example MOVEA.L LABEL(PC,D0.W),An followed by JMP(An)
    my $addrPrev = &PreviousInst($addr);
    if ( $addrPrev >= 0 ) {
	my $asm = &GetAssembly($addrPrev);
	if ( $asm =~ /^\s*MOVEA\.L\s*([0-9A-Za-z_]+)\(PC,D[0-7](?:\.[WL])?\),A([0-7])\s*$/i ) {
	    my $label = $1;
	    my $areg = $2;
	    if ( $areg == $reg ) {
		my $labelAddr = &GetAddrFromLabel($label);
		if ( $labelAddr >= 0 ) {
		    &AddComment($labelAddr,"\n".&Disassemble_Comment('CODETBL'));
		}
	    }
	} elsif ( $asm =~ /^\s*(?:MOVEA\.L|ADDA\.[WL])\s*0\(A([0-7]),D[0-7](?:\.[WL])?\),A([0-7])\s*$/i ) {
	    # ADDA uses offsets from table start
	    # MOVEA points to actual location
	    my $areg1 = $1;
	    my $areg2 = $2;
	    if ( $areg2 == $reg ) {
		my $addrPrev2 = &PreviousInst($addrPrev);
		if ( $addrPrev2 >= 0 ) {
		    my $asm2 = &GetAssembly($addrPrev2);
		    if ( $asm2 =~ /^\s*LEA\s*([0-9A-Za-z_]+),A([0-7])\s*$/i ||
			 $asm2 =~ /^\s*LEA\s*\(([0-9A-Za-z_]+)\)\.[WL],A([0-7])\s*$/i ) {
			# could be Label or (Label).L or (Label).W
			my $label = $1;
			my $areg3 = $2;
			if ( $areg1 == $areg3 ) {
			    my $labelAddr = &GetAddrFromLabel($label);
			    if ( $labelAddr >= 0 ) {
				&AddComment($labelAddr,"\n".&Disassemble_Comment('CODETBL'));
			    }
			}
		    }
		}
	    }
	}
    }
}


1;

