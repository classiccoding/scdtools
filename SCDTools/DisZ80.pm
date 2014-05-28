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
# DisassembleZ80 Support (for scddisassemble)
###############################################################################
# Main decode is based on Cristians Dinu's Decoding Z80 Opcodes document
# ref[1]: http://www.z80.info/decoding.htm
###############################################################################

# TODO IXH/IXL/IYH/IYL undocumented instructions not supported

package SCDTools::DisZ80;
use strict;
use warnings;

use constant DIS_Z80_PREFIX_MAIN     => 0x001;
use constant DIS_Z80_PREFIX_EXTENDED => 0x002;
use constant DIS_Z80_PREFIX_IX       => 0x010;
use constant DIS_Z80_PREFIX_IY       => 0x020;
use constant DIS_Z80_PREFIX_BIT      => 0x100;
use constant DIS_Z80_PREFIX_IX_BIT   => 0x110;
use constant DIS_Z80_PREFIX_IY_BIT   => 0x120;

use constant DIS_Z80_PREFIX_I_MASK   => 0x030;


# requires vasm built at May 2, 2014 (1.6d)
#   this adds in features I requested for greater z80 compatibility
#   supports 0ffh values
#   supports colon for equ
#   supports labels for RST (if org specified)

# +---------------------------------------------------------------------------+
# | assembler differences                                                     |
# +---+---+---+---+---+---+---------------------------------------------------+
# |   |asm|pas| t | v |z80|                                                   |
# |asl| x |mo |asm|asm|asm| description                                       |
# +---------------------------------------------------------------------------+
# | cannot support all assemblers at once differences                         |
# +---+---+---+---+---+---+---------------------------------------------------+
# | Y | Y | Y | N | Y | Y | supports ORG                                      |
# | ? | ? | ? | Y | ? | ? | supports .ORG                                     |
# +---+---+---+---+---+---+---------------------------------------------------+
# | Y | Y | Y | N | Y | Y | supports 01BB4H                                   |
# | Y | Y | Y | Y | Y | Y | supports 01BB4H except RST                        |
# | N | Y | Y |   | Y | Y | supports 0x1BB4                                   |
# | N | Y | Y | Y | ? | Y | supports  1BB4H                                   |
# | Y | N | Y |   | Y | N | supports &1BB4                                    |
# | Y | N | Y |   | Y | Y | supports $1BB4                                    |
# | N | N | Y |   | Y | Y | supports $1BB4 without warnings                   |
# +---+---+---+---+---+---+---------------------------------------------------+
# | other differences                                                         |
# +---+---+---+---+---+---+---------------------------------------------------+
# | Y | Y | Y | N | Y | Y | supports db                                       |
# | ? | Y | ? |   | N |   | supports dc.b                                     |
# | ? | ? | ? |   | ? | ? | supports .BYTE                                    |
# +---+---+---+---+---+---+---------------------------------------------------+
# | N | N | Y |   | Y | Y | supports SLL  (undoc 0xCB30)                      |
# | Y | N | N |   | N | N | supports SLIA (undoc 0xCB30)                      |
# | N | N | N |   | N | Y | supports SLI  (undoc 0xCB30)                      |
# +---+---+---+---+---+---+---------------------------------------------------+
# | Y | Y | Y | N | N | Y | supports RST label                                |
# +---+---+---+---+---+---+---------------------------------------------------+
# | Y | Y | Y |   | Y | Y | supports (IX-057h)                                |
# | N | Y | Y |   | Y | Y | supports (IX+0A9h)                                |
# +---+---+---+---+---+---+---------------------------------------------------+
# | Y | Y | Y |   | Y | Y | supports ADD A,C                                  |
# | N | N | N |   | Y | Y | supports ADD C                                    |
# +---+---+---+---+---+---+---------------------------------------------------+
# | Y | Y | Y |   | Y | Y | supports ADC/SBC A,C                              |
# | N | N | N |   | Y | N | supports ADC/SBC C                                |
# +---+---+---+---+---+---+---------------------------------------------------+
# | Y | Y | Y |   | Y | Y | supports SUB C                                    |
# | Y | N | N |   | Y | Y | supports SUB A,C                                  |
# +---+---+---+---+---+---+---------------------------------------------------+
# | Y | Y | Y |   | Y | Y | supports CP C                                     |
# | Y | N | N |   | Y | N | supports CP A,C                                   |
# +---+---+---+---+---+---+---------------------------------------------------+
# | Y | Y | Y |   | Y | Y | supports label: equ (note colon)                  |
# | Y | Y | Y |   | Y | N | supports label  equ (note no colon)               |
# +---+---+---+---+---+---+---------------------------------------------------+
# |   |   |   |   |   |   |                                                   |
# +---+---+---+---+---+---+---------------------------------------------------+

use constant DIS_Z80_DIFF_UNDOC_SLL          => 0x00000001;
use constant DIS_Z80_DIFF_RST_LABEL          => 0x00000002;
use constant DIS_Z80_OPT_JP_TO_JR            => 0x80000000;

use constant DIS_Z80_DIFF_ASL =>
    # supports it with SLIA
    DIS_Z80_DIFF_UNDOC_SLL;
use constant DIS_Z80_DIFF_ASMX =>
    DIS_Z80_DIFF_UNDOC_SLL;
use constant DIS_Z80_DIFF_GAS =>
    DIS_Z80_DIFF_RST_LABEL;
use constant DIS_Z80_DIFF_PASMO => 0;
use constant DIS_Z80_DIFF_SJASM => 0;
use constant DIS_Z80_DIFF_TASM =>
    DIS_Z80_DIFF_UNDOC_SLL |
    DIS_Z80_DIFF_RST_LABEL;
use constant DIS_Z80_DIFF_VASM => 0;
use constant DIS_Z80_DIFF_Z80ASM => 0;


my $diff = 0;
my $diffEncountered = 0;


use SCDTools::DisCommon;
use SCDTools::DisFormat;
use SCDTools::DisPick;

my $regA   = 'A';
my $regB   = 'B';
my $regC   = 'C';
my $regD   = 'D';
my $regE   = 'E';
my $regH   = 'H';
my $regI   = 'I';
my $regL   = 'L';
my $regR   = 'R';
my $regAF  = 'AF';
my $regBC  = 'BC';
my $regDE  = 'DE';
my $regHL  = 'HL';
my $regIX  = 'IX';
my $regIY  = 'IY';
my $regSP  = 'SP';




sub DisassembleZ80_NiceCheck {
    # TODO do nothing
}

sub DisassembleZ80_CheckMnem {
    # TODO do nothing for now
    return 0;
}

sub DisassembleZ80_indirect {
    return '('.$_[0].')';
}

sub DisassembleZ80_indirect_offset {
    my ($addr,$width,$reg) = @_;
    my $offset = &GetByte($addr+$width);
    $width++;
    my $ea;
    if ( $offset & 0x80 ) {
	my $offsetNeg = ( ($offset & 0xFF) ^ 0xFF ) + 1;
	$ea = &DisassembleZ80_indirect($reg.'-'.&Disassemble_Value_Byte($offsetNeg));
    } else {
	$ea = &DisassembleZ80_indirect($reg.'+'.&Disassemble_Value_Byte($offset));
    }
    return ($ea,$width);
}

sub DisassembleZ80_ry {
    my ($problem,$addr,$width,$y,$ireg,$unsupportedIfIndexing) = @_;
    my $z = $y >> 3;
    return &DisassembleZ80_rz($problem,$addr,$width,$z,$ireg,$unsupportedIfIndexing);
}

sub DisassembleZ80_rz {
    my ($problem,$addr,$width,$z,$ireg,$unsupportedIfIndexing) = @_;
    my $ea;
    if ( $z == 0x00 ) {
	$ea = $regB;
	$problem |= $unsupportedIfIndexing
    } elsif ( $z == 0x01 ) {
	$ea = $regC;
	$problem |= $unsupportedIfIndexing
    } elsif ( $z == 0x02 ) {
	$ea = $regD;
	$problem |= $unsupportedIfIndexing
    } elsif ( $z == 0x03 ) {
	$ea = $regE;
	$problem |= $unsupportedIfIndexing
    } elsif ( $z == 0x04 ) {
	$ea = $regH;
	$problem |= $unsupportedIfIndexing
    } elsif ( $z == 0x05 ) {
	$ea = $regL;
	$problem |= $unsupportedIfIndexing
    } elsif ( $z == 0x06 ) {
	if ( $ireg eq $regHL ) {
	    $ea = &DisassembleZ80_indirect($ireg);
	} else {
	    ($ea,$width) = &DisassembleZ80_indirect_offset($addr,$width,$ireg);
	}
    } elsif ( $z == 0x07 ) {
	$ea = $regA;
	$problem |= $unsupportedIfIndexing
    }
    return ($problem,$ea,$width);
}

sub DisassembleZ80_rpp {
    my ($p,$ireg) = @_;
    if ( $p == 0x00 ) {
	return $regBC;
    } elsif ( $p == 0x10 ) {
	return $regDE;
    } elsif ( $p == 0x20 ) {
	return $ireg;
    } elsif ( $p == 0x30 ) {
	return $regSP;
    }
    return '?';
}

sub DisassembleZ80_rp2p {
    my ($p,$ireg) = @_;
    if ( $p == 0x00 ) {
	return $regBC;
    } elsif ( $p == 0x10 ) {
	return $regDE;
    } elsif ( $p == 0x20 ) {
	return $ireg;
    } elsif ( $p == 0x30 ) {
	return $regAF;
    }
    return '?';
}

sub DisassembleZ80_ccy {
    my $y = $_[0];
    my $flag;
    if ( $y == 0x00 ) {
	$flag = 'NZ';
    } elsif ( $y == 0x08 ) {
	$flag = 'Z';
    } elsif ( $y == 0x10 ) {
	$flag = 'NC';
    } elsif ( $y == 0x18 ) {
	$flag = 'C';
    } elsif ( $y == 0x20 ) {
	$flag = 'PO';
    } elsif ( $y == 0x28 ) {
	$flag = 'PE';
    } elsif ( $y == 0x30 ) {
	$flag = 'P';
    } elsif ( $y == 0x38 ) {
	$flag = 'M';
    } else {
	$flag = '?';
    }
    return &ApplyCaseMnem($flag);
}

sub DisassembleZ80_ccy_mask3 {
    return &DisassembleZ80_ccy($_[0] & 0x18);
}

sub DisassembleZ80_aluy {
    my $y = $_[0];
    # do as documented since assemblers agree on that
    # ADD, ADC, and SBC require A field, others do not
    if ( $y == 0x00 ) {
	return ('ADD',1);
    } elsif ( $y == 0x08 ) {
	return ('ADC',1);
    } elsif ( $y == 0x10 ) {
	return ('SUB',0);
    } elsif ( $y == 0x18 ) {
	return ('SBC',1);
    } elsif ( $y == 0x20 ) {
	return ('AND',0);
    } elsif ( $y == 0x28 ) {
	return ('XOR',0);
    } elsif ( $y == 0x30 ) {
	return ('OR',0);
    } elsif ( $y == 0x38 ) {
	return ('CP',0);
    } 
    return ('?',0);
}

sub DisassembleZ80_roty {
    my $y = $_[0];
    if ( $y == 0x00 ) {
	return 'RLC';
    } elsif ( $y == 0x08 ) {
	return 'RRC';
    } elsif ( $y == 0x10 ) {
	return 'RL';
    } elsif ( $y == 0x18 ) {
	return 'RR';
    } elsif ( $y == 0x20 ) {
	return 'SLA';
    } elsif ( $y == 0x28 ) {
	return 'SRA';
    } elsif ( $y == 0x30 ) {
	# undocumented, also referred to as:
	# SLL (ref[1], z80asm)
	# SLIA (asl)
	# SLI (z80asm), SLS (z80table.pdf)
	return 'SLL';
    } elsif ( $y == 0x38 ) {
	return 'SRL';
    } 
    return '?';
}

sub DisassembleZ80_n_value {
    my ($addr,$width) = @_;
    my $n_value = &GetByte($addr+$width);
    $width++;
    return ($n_value,$width);
}

sub DisassembleZ80_nn_value {
    my ($addr,$width) = @_;
    my $data0 = &GetByte($addr+$width);
    $width++;
    my $data1 = &GetByte($addr+$width);
    $width++;
    my $nn_value = ( $data1 << 8 ) | $data0;
    return ($nn_value,$width);
}

sub DisassembleZ80_n {
    my ($addr,$width) = @_;
    my $n_value;
    ($n_value,$width) = &DisassembleZ80_n_value($addr,$width);
    my $n = &Disassemble_Value_Byte($n_value);
    return ($n,$width);
}

sub DisassembleZ80_nn {
    my ($addr,$width) = @_;
    my $nn_value;
    ($nn_value,$width) = &DisassembleZ80_nn_value($addr,$width);
    my $nn = &Disassemble_Value_Word($nn_value);
    return ($nn,$width);
}

sub DisassembleZ80_n_indirect {
    my ($addr,$width) = @_;
    my $n;
    ($n,$width) = &DisassembleZ80_n(@_);
    my $n_indirect = &DisassembleZ80_indirect($n);
    return ($n_indirect,$width);
}

sub DisassembleZ80_nn_indirect {
    my ($addr,$width) = @_;
    my $nn;
    ($nn,$width) = &DisassembleZ80_nn(@_);
    my $nn_indirect = &DisassembleZ80_indirect($nn);
    return ($nn_indirect,$width);
}

sub DisassembleZ80_ybit {
    return &Disassemble_Value_3Bits($_[0]>>3);
}

sub DisassembleZ80_y8 {
    return &Disassemble_Value_Byte($_[0]);
}


sub DisassembleZ80_Setup {
    $diff = 0;
    if ( &GetAssembler() & ASM_ASL ) {
	$diff |= DIS_Z80_DIFF_ASL;
    }
    if ( &GetAssembler() & ASM_ASMX ) {
	$diff |= DIS_Z80_DIFF_ASMX;
    }
    if ( &GetAssembler() & ASM_GAS ) {
	$diff |= DIS_Z80_DIFF_GAS;
    }
    if ( &GetAssembler() & ASM_PASMO ) {
	$diff |= DIS_Z80_DIFF_PASMO;
    }
    if ( &GetAssembler() & ASM_SJASM ) {
	$diff |= DIS_Z80_DIFF_SJASM;
    }
    if ( &GetAssembler() & ASM_TASM ) {
	$diff |= DIS_Z80_DIFF_TASM;
    }
    if ( &GetAssembler() & ASM_VASM ) {
	$diff |= DIS_Z80_DIFF_VASM;
    }
    if ( &GetAssembler() & ASM_Z80ASM ) {
	$diff |= DIS_Z80_DIFF_Z80ASM;
    }

    $regA    = &ApplyCaseReg('A');
    $regB    = &ApplyCaseReg('B');
    $regC    = &ApplyCaseReg('C');
    $regD    = &ApplyCaseReg('D');
    $regE    = &ApplyCaseReg('E');
    $regH    = &ApplyCaseReg('H');
    $regI    = &ApplyCaseReg('I');
    $regL    = &ApplyCaseReg('L');
    $regR    = &ApplyCaseReg('R');
    $regAF   = &ApplyCaseReg('AF');
    $regBC   = &ApplyCaseReg('BC');
    $regDE   = &ApplyCaseReg('DE');
    $regHL   = &ApplyCaseReg('HL');
    $regIX   = &ApplyCaseReg('IX');
    $regIX   = &ApplyCaseReg('IY');
    $regSP   = &ApplyCaseReg('SP');
}

sub DisassembleZ80 {
    my $addr = $_[0];

    my $problem = DIS_PRBM_NONE;
    my $inst = '';
    my $width = 1;

    # since most instructions are sequential, assume sequential
    my $seq = DATA_USAGE_NEXT_SEQUENTIAL;

    my $data = &GetByte($addr);

    # common parts of most instructions
    # TODO http://www.z80.info/decoding.htm

    my $prefixType = 0;
    
    if ( $data == 0xDD ) {
	$prefixType = DIS_Z80_PREFIX_IX;
	$data = &GetByte($addr+$width);
	$width++;
    } elsif ( $data == 0xFD ) {
	$prefixType = DIS_Z80_PREFIX_IY;
	$data = &GetByte($addr+$width);
	$width++;
    }

    if ( $data == 0xCB ) {
	$prefixType |= DIS_Z80_PREFIX_BIT;
	if ( $prefixType & DIS_Z80_PREFIX_I_MASK ) {
	    # displacement is before opcode byte
	    $width++;
	}
	$data = &GetByte($addr+$width);
	$width++;
    } elsif ( $prefixType == 0 && $data == 0xED ) {
	$prefixType = DIS_Z80_PREFIX_EXTENDED;
	$data = &GetByte($addr+$width);
	$width++;
    } else {
	$prefixType |= DIS_Z80_PREFIX_MAIN;
    }

    my $x = $data & 0xC0;
    my $y = $data & 0x38;
    my $z = $data & 0x07;
    my $p = $data & 0x30;
    my $q = $data & 0x08;

    # TODO branching/calling/jumping instructions stubbed since labels not supported yet

    my $ireg;
    my $unsupportedIfIndexing;
    if ( $prefixType & DIS_Z80_PREFIX_IX ) {
	$ireg = $regIX;
	$unsupportedIfIndexing = DIS_PRBM_UNSUPPORTED;
    } elsif ( $prefixType & DIS_Z80_PREFIX_IY ) {
	$ireg = $regIY;
	$unsupportedIfIndexing = DIS_PRBM_UNSUPPORTED;
    } else {
	$ireg = $regHL;
	$unsupportedIfIndexing = DIS_PRBM_NONE;
    }

    if ( $prefixType & DIS_Z80_PREFIX_MAIN ) {
	if ( $x == 0x00 ) {
	    if ( $z == 0x00 ) {
		if ( $y < 0x20 ) {
		    if ( $y == 0x00 ) {
			$inst = &Disassemble_Instruction('NOP');
			$problem |= $unsupportedIfIndexing;
		    } elsif ( $y == 0x08 ) {
			# TODO format out for AF'?
			$inst = &Disassemble_Instruction('EX',$regAF,$regAF.'\'');
			$problem |= $unsupportedIfIndexing;
		    } elsif ( $y == 0x10 ) {
			my $n_value;
			($n_value,$width) = &DisassembleZ80_n_value($addr,$width);
			my $target = ( $addr + 2 + &SignExtendByte($n_value) ) & 0xFFFF;
			my $label;
			($problem,$label) = &LabelGeneric($problem,$target);
			$inst = &Disassemble_Instruction('DJNZ',$label);
			$seq = DATA_USAGE_NEXT_CONDSEQUENTIAL;
			$problem |= $unsupportedIfIndexing;
			if ( ! $problem ) {
			    &AddToCodePointCache($target);
			}
		    } else {
			my $n_value;
			($n_value,$width) = &DisassembleZ80_n_value($addr,$width);
			my $target = ( $addr + 2 + &SignExtendByte($n_value) ) & 0xFFFF;
			my $label;
			($problem,$label) = &LabelGeneric($problem,$target);
			$inst = &Disassemble_Instruction('JR',$label);
			$seq = DATA_USAGE_NEXT_CONDSEQUENTIAL;
			$problem |= $unsupportedIfIndexing;
			if ( ! $problem ) {
			    &AddToCodePointCache($target);
			}
		    }
		} else {
		    my $ccy_mask3 = &DisassembleZ80_ccy_mask3($y);
		    my $n_value;
		    ($n_value,$width) = &DisassembleZ80_n_value($addr,$width);
		    my $target = ( $addr + 2 + &SignExtendByte($n_value) ) & 0xFFFF;
		    my $label;
		    ($problem,$label) = &LabelGeneric($problem,$target);
		    $inst = &Disassemble_Instruction('JR',$ccy_mask3,$label);
		    $seq = DATA_USAGE_NEXT_CONDSEQUENTIAL;
		    $problem |= $unsupportedIfIndexing;
		    if ( ! $problem ) {
			&AddToCodePointCache($target);
		    }
		}
	    } elsif ( $z == 0x01 ) {
		my $rpp = &DisassembleZ80_rpp($p,$ireg);
		if ( $q == 0x00 ) {
		    my $nn;
		    ($nn,$width) = &DisassembleZ80_nn($addr,$width);
		    $inst = &Disassemble_Instruction('LD',$rpp,$nn);
		    if ( $rpp ne $ireg ) {
			$problem |= $unsupportedIfIndexing;
		    }
		} else {
		    $inst = &Disassemble_Instruction('ADD',$ireg,$rpp);
		}
	    } elsif ( $z == 0x02 ) {
		my $ea1;
		my $ea2;
		if ( $p == 0x00 ) {
		    $ea1 = &DisassembleZ80_indirect($regBC);
		    $ea2 = $regA;
		    $problem |= $unsupportedIfIndexing;
		} elsif ( $p == 0x10 ) {
		    $ea1 = &DisassembleZ80_indirect($regDE);
		    $ea2 = $regA;
		    $problem |= $unsupportedIfIndexing;
		} elsif ( $p == 0x20 ) {
		    ($ea1,$width) = &DisassembleZ80_nn_indirect($addr,$width);
		    $ea2 = $ireg;
		} else {
		    ($ea1,$width) = &DisassembleZ80_nn_indirect($addr,$width);
		    $ea2 = $regA;
		    $problem |= $unsupportedIfIndexing;
		}
		if ( $q == 0x00 ) {
		    $inst = &Disassemble_Instruction('LD',$ea1,$ea2);
		} else {
		    $inst = &Disassemble_Instruction('LD',$ea2,$ea1);
		}
	    } elsif ( $z == 0x03 ) {
		my $rpp = &DisassembleZ80_rpp($p,$ireg);
		if ( $q == 0x00 ) {
		    $inst = &Disassemble_Instruction('INC',$rpp);
		} else {
		    $inst = &Disassemble_Instruction('DEC',$rpp);
		}
		if ( $rpp ne $ireg ) {
		    $problem |= $unsupportedIfIndexing;
		}
	    } elsif ( $z == 0x04 ) {
		my $ry;
		($problem,$ry,$width) = &DisassembleZ80_ry($problem,$addr,$width,$y,$ireg,$unsupportedIfIndexing);
		$inst = &Disassemble_Instruction('INC',$ry);
	    } elsif ( $z == 0x05 ) {
		my $ry;
		($problem,$ry,$width) = &DisassembleZ80_ry($problem,$addr,$width,$y,$ireg,$unsupportedIfIndexing);
		$inst = &Disassemble_Instruction('DEC',$ry);
	    } elsif ( $z == 0x06 ) {
		my $ry;
		($problem,$ry,$width) = &DisassembleZ80_ry($problem,$addr,$width,$y,$ireg,$unsupportedIfIndexing);
		my $n;
		($n,$width) = &DisassembleZ80_n($addr,$width);
		$inst = &Disassemble_Instruction('LD',$ry,$n);
	    } elsif ( $z == 0x07 ) {
		if ( $y == 0x00 ) {
		    $inst = &Disassemble_Instruction('RLCA');
		    $problem |= $unsupportedIfIndexing;
		} elsif ( $y == 0x08 ) {
		    $inst = &Disassemble_Instruction('RRCA');
		    $problem |= $unsupportedIfIndexing;
		} elsif ( $y == 0x10 ) {
		    $inst = &Disassemble_Instruction('RLA');
		    $problem |= $unsupportedIfIndexing;
		} elsif ( $y == 0x18 ) {
		    $inst = &Disassemble_Instruction('RRA');
		    $problem |= $unsupportedIfIndexing;
		} elsif ( $y == 0x20 ) {
		    $inst = &Disassemble_Instruction('DAA');
		    $problem |= $unsupportedIfIndexing;
		} elsif ( $y == 0x28 ) {
		    $inst = &Disassemble_Instruction('CPL');
		    $problem |= $unsupportedIfIndexing;
		} elsif ( $y == 0x30 ) {
		    $inst = &Disassemble_Instruction('SCF');
		    $problem |= $unsupportedIfIndexing;
		} else {
		    $inst = &Disassemble_Instruction('CCF');
		    $problem |= $unsupportedIfIndexing;
		}
	    }
	} elsif ( $x == 0x40 ) {
	    if ( ($y|$z) == 0x36 ) {
		$inst = &Disassemble_Instruction('HALT');
		$problem |= $unsupportedIfIndexing;
	    } else {
		if ( $y != 0x30 && $z != 0x06 ) {
		    $problem |= $unsupportedIfIndexing;
		}
		my $ry;
		($problem,$ry,$width) = &DisassembleZ80_ry($problem,$addr,$width,$y,$ireg,0);
		my $rz;
		($problem,$rz,$width) = &DisassembleZ80_rz($problem,$addr,$width,$z,$ireg,0);
		$inst = &Disassemble_Instruction('LD',$ry,$rz);
	    }
	} elsif ( $x == 0x80 ) {
	    my ($mnem,$needsA) = &DisassembleZ80_aluy($y);
	    my $rz;
	    ($problem,$rz,$width) = &DisassembleZ80_rz($problem,$addr,$width,$z,$ireg,$unsupportedIfIndexing);
	    if ( $needsA ) {
		$inst = &Disassemble_Instruction($mnem,$regA,$rz);
	    } else {
		$inst = &Disassemble_Instruction($mnem,$rz);
	    }
	} elsif ( $x == 0xC0 ) {
	    if ( $z == 0x00 ) {
		my $ccy = &DisassembleZ80_ccy($y);
		$inst = &Disassemble_Instruction('RET',$ccy);
		$seq = DATA_USAGE_NEXT_CONDSEQUENTIAL;
		$problem |= $unsupportedIfIndexing;
	    } elsif ( $z == 0x01 ) {
		if ( $q == 0x00 ) {
		    my $rp2p = &DisassembleZ80_rp2p($p,$ireg);
		    $inst = &Disassemble_Instruction('POP',$rp2p);
		    if ( $rp2p ne $ireg ) {
			$problem |= $unsupportedIfIndexing;
		    }
		} else {
		    if ( $p == 0x00 ) {
			$inst = &Disassemble_Instruction('RET');
			$seq = DATA_USAGE_NEXT_NONSEQUENTIAL;
			$problem |= $unsupportedIfIndexing;
		    } elsif ( $p == 0x10 ) {
			$inst = &Disassemble_Instruction('EXX');
			$problem |= $unsupportedIfIndexing;
		    } elsif ( $p == 0x20 ) {
			$inst = &Disassemble_Instruction('JP',&DisassembleZ80_indirect($ireg));
			$seq = DATA_USAGE_NEXT_NONSEQUENTIAL;
		    } else {
			$inst = &Disassemble_Instruction('LD',$regSP,$ireg);
		    }
		}
	    } elsif ( $z == 0x02 ) {
		my $ccy = &DisassembleZ80_ccy($y);
		my $target;
		($target,$width) = &DisassembleZ80_nn_value($addr,$width);
		my $rel = ( $target - ( $addr + 2 ) );
		if ( ( $y & 0x18 ) == $y &&
		     ( $rel & 0xFFFFFF80 ) == 0x00000000 ||
		     ( $rel & 0xFFFFFF80 ) == 0xFFFFFF80 ) {
		    # can be written as JR (though JP is faster)
		    # note JR only has subset of cond codes
		    $diffEncountered |= DIS_Z80_OPT_JP_TO_JR;
		    &AddUnoptimizedBytes(1);
		}
		my $label;
		($problem,$label) = &LabelGeneric($problem,$target);
		$inst = &Disassemble_Instruction('JP',$ccy,$label);
		$seq = DATA_USAGE_NEXT_CONDSEQUENTIAL;
		$problem |= $unsupportedIfIndexing;
		if ( ! $problem ) {
		    &AddToCodePointCache($target);
		}
	    } elsif ( $z == 0x03 ) {
		if ( $y == 0x00 ) {
		    my $target;
		    ($target,$width) = &DisassembleZ80_nn_value($addr,$width);
		    my $rel = ( $target - ( $addr + 2 ) );
		    if ( ( $rel & 0xFFFFFF80 ) == 0x00000000 ||
			 ( $rel & 0xFFFFFF80 ) == 0xFFFFFF80 ) {
			# can be written as JR (though JP is faster)
			$diffEncountered |= DIS_Z80_OPT_JP_TO_JR;
			&AddUnoptimizedBytes(1);
		    }
		    my $label;
		    ($problem,$label) = &LabelGeneric($problem,$target);
		    $inst = &Disassemble_Instruction('JP',$label);
		    $seq = DATA_USAGE_NEXT_NONSEQUENTIAL;
		    $problem |= $unsupportedIfIndexing;
		    if ( ! $problem ) {
			&AddToCodePointCache($target);
		    }
		} elsif ( $y == 0x08 ) {
		    die "Bad Z80 CB detection\n";
		} elsif ( $p == 0x10 ) {
		    # y is either 0x10 and 0x18
		    my $n_indirect;
		    ($n_indirect,$width) = &DisassembleZ80_n_indirect($addr,$width);
		    if ( $q == 0x00 ) {
			$inst = &Disassemble_Instruction('OUT',$n_indirect,$regA);
		    } else {
			$inst = &Disassemble_Instruction('IN',$regA,$n_indirect);
		    }
		    $problem |= $unsupportedIfIndexing;
		} elsif ( $y == 0x20 ) {
		    $inst = &Disassemble_Instruction('EX',&DisassembleZ80_indirect($regSP),$ireg);
		} elsif ( $y == 0x28 ) {
		    # does not have an IX/IY version
		    $inst = &Disassemble_Instruction('EX',$regDE,$regHL);
		    $problem |= $unsupportedIfIndexing;
		} elsif ( $y == 0x30 ) {
		    $inst = &Disassemble_Instruction('DI');
		    $problem |= $unsupportedIfIndexing;
		} else {
		    $inst = &Disassemble_Instruction('EI');
		    $problem |= $unsupportedIfIndexing;
		}
	    } elsif ( $z == 0x04 ) {
		my $ccy = &DisassembleZ80_ccy($y);
		my $target;
		($target,$width) = &DisassembleZ80_nn_value($addr,$width);
		my $label;
		($problem,$label) = &LabelGeneric($problem,$target);
		$inst = &Disassemble_Instruction('CALL',$ccy,$label);
		$seq = DATA_USAGE_NEXT_CONDSEQUENTIAL;
		$problem |= $unsupportedIfIndexing;
		if ( ! $problem ) {
		    &AddToCodePointCache($target);
		}
	    } elsif ( $z == 0x05 ) {
		if ( $q == 0x00 ) {
		    my $rp2p = &DisassembleZ80_rp2p($p,$ireg);
		    $inst = &Disassemble_Instruction('PUSH',$rp2p);
		    if ( $rp2p ne $ireg ) {
			$problem |= $unsupportedIfIndexing;
		    }
		} else {
		    if ( $p == 0x00 ) {
			my $target;
			($target,$width) = &DisassembleZ80_nn_value($addr,$width);
			my $label;
			($problem,$label) = &LabelGeneric($problem,$target);
			$inst = &Disassemble_Instruction('CALL',$label);
			$seq = DATA_USAGE_NEXT_CONDSEQUENTIAL;
			$problem |= $unsupportedIfIndexing;
			if ( ! $problem ) {
			    &AddToCodePointCache($target);
			}
		    } else {
			# do nothing byte value is 0xDD, 0xED, or 0xFD
		    }
		}
	    } elsif ( $z == 0x06 ) {
		my ($mnem,$needsA) = &DisassembleZ80_aluy($y);
		my $n;
		($n,$width) = &DisassembleZ80_n($addr,$width);
		if ( $needsA ) {
		    $inst = &Disassemble_Instruction($mnem,$regA,$n);
		} else {
		    $inst = &Disassemble_Instruction($mnem,$n);
		}
		$problem |= $unsupportedIfIndexing;
	    } else {
		my $target = $y;
		my $label;
		my $problem2 = 0;
		($problem2,$label) = &LabelGeneric($problem,$target);
		if ( $problem2 ) {
		    my $y8 = &DisassembleZ80_y8($y);
		    $inst = &Disassemble_Instruction('RST',$y8);
		} elsif ( $diff & DIS_Z80_DIFF_RST_LABEL ) {
		    my $y8 = &DisassembleZ80_y8($y);
		    $inst = &Disassemble_Instruction('RST',$y8);
		    &AddComment($addr,&Disassemble_Comment("goes to ".$label));
		} else {
		    $inst = &Disassemble_Instruction('RST',$label);
		}
		$seq = DATA_USAGE_NEXT_CONDSEQUENTIAL;
		$problem |= $unsupportedIfIndexing;
		if ( ! $problem ) {
		    &AddToCodePointCache($target);
		}
	    }
	}
    } elsif ( $prefixType & DIS_Z80_PREFIX_EXTENDED ) {
	$problem |= $unsupportedIfIndexing;
	if ( $x == 0x40 ) {
	    if ( $z == 0x00 ) {
		if ( $y == 0x30 ) {
		    $inst = &Disassemble_Instruction('IN',&DisassembleZ80_indirect($regC));
		    $problem |= DIS_PRBM_UNREPRESENTABLE;
		} else {
		    my $ry;
		    ($problem,$ry,$width) = &DisassembleZ80_ry($problem,$addr,$width,$y,$ireg,0);
		    $inst = &Disassemble_Instruction('IN',$ry,&DisassembleZ80_indirect($regC));
		}
	    } elsif ( $z == 0x01 ) {
		if ( $y == 0x30 ) {
		    $inst = &Disassemble_Instruction('OUT',&DisassembleZ80_indirect($regC));
		    $problem |= DIS_PRBM_UNREPRESENTABLE;
		} else {
		    my $ry;
		    ($problem,$ry,$width) = &DisassembleZ80_ry($problem,$addr,$width,$y,$ireg,0);
		    $inst = &Disassemble_Instruction('OUT',&DisassembleZ80_indirect($regC),$ry);
		}
	    } elsif ( $z == 0x02 ) {
		my $rpp = &DisassembleZ80_rpp($p,$ireg);
		if ( $q == 0x00 ) {
		    $inst = &Disassemble_Instruction('SBC',$ireg,$rpp);
		} else {
		    $inst = &Disassemble_Instruction('ADC',$ireg,$rpp);
		}
	    } elsif ( $z == 0x03 ) {
		my $rpp = &DisassembleZ80_rpp($p,$ireg);
		my $nn_indirect;
		($nn_indirect,$width) = &DisassembleZ80_nn_indirect($addr,$width);
		if ( $q == 0x00 ) {
		    $inst = &Disassemble_Instruction('LD',$nn_indirect,$rpp);
		} else {
		    $inst = &Disassemble_Instruction('LD',$rpp,$nn_indirect);
		}
		if ( $rpp eq $ireg ) {
		    # mirrored version of main table instruction
		    $problem |= DIS_PRBM_UNREPRESENTABLE;
		}
	    } elsif ( $z == 0x04 ) {
		$inst = &Disassemble_Instruction('NEG');
		if ( $y != 0x00 ) {
		    $problem |= DIS_PRBM_UNREPRESENTABLE;
		}
	    } elsif ( $z == 0x05 ) {
		if ( $y == 0x00 ) {
		    $inst = &Disassemble_Instruction('RETN');
		    $seq = DATA_USAGE_NEXT_NONSEQUENTIAL;
		} elsif ( $y == 0x08 ) {
		    $inst = &Disassemble_Instruction('RETI');
		    $seq = DATA_USAGE_NEXT_NONSEQUENTIAL;
		} else {
		    $inst = &Disassemble_Instruction('RETN');
		    $seq = DATA_USAGE_NEXT_NONSEQUENTIAL;
		    $problem |= DIS_PRBM_UNREPRESENTABLE;
		}
	    } elsif ( $z == 0x06 ) {
		if ( $y == 0x00 ) {
		    $inst = &Disassemble_Instruction('IM','0');
		} elsif ( $y == 0x08 ) {
		    $inst = &Disassemble_Instruction('IM','0/1');
		    $problem |= DIS_PRBM_UNREPRESENTABLE;
		} elsif ( $y == 0x10 ) {
		    $inst = &Disassemble_Instruction('IM','1');
		} elsif ( $y == 0x18 ) {
		    $inst = &Disassemble_Instruction('IM','2');
		} elsif ( $y == 0x20 ) {
		    $inst = &Disassemble_Instruction('IM','0');
		    $problem |= DIS_PRBM_UNREPRESENTABLE;
		} elsif ( $y == 0x30 ) {
		    $inst = &Disassemble_Instruction('IM','1');
		    $problem |= DIS_PRBM_UNREPRESENTABLE;
		} elsif ( $y == 0x38 ) {
		    $inst = &Disassemble_Instruction('IM','2');
		    $problem |= DIS_PRBM_UNREPRESENTABLE;
		} else {
		    $inst = &Disassemble_Instruction('IM','0/1');
		    $problem |= DIS_PRBM_UNREPRESENTABLE;
		}
	    } else {
		if ( $y == 0x00 ) {
		    $inst = &Disassemble_Instruction('LD',$regI,$regA);
		} elsif ( $y == 0x08 ) {
		    $inst = &Disassemble_Instruction('LD',$regR,$regA);
		} elsif ( $y == 0x10 ) {
		    $inst = &Disassemble_Instruction('LD',$regA,$regI);
		} elsif ( $y == 0x18 ) {
		    $inst = &Disassemble_Instruction('LD',$regA,$regR);
		} elsif ( $y == 0x20 ) {
		    $inst = &Disassemble_Instruction('RRD');
		} elsif ( $y == 0x28 ) {
		    $inst = &Disassemble_Instruction('RLD');
		}
	    }
	} elsif ( $x == 0x80 ) {
	    if ( $z == 0x00 ) {
		if ( $y == 0x20 ) {
		    $inst = &Disassemble_Instruction('LDI');
		} elsif ( $y == 0x28 ) {
		    $inst = &Disassemble_Instruction('LDD');
		} elsif ( $y == 0x30 ) {
		    $inst = &Disassemble_Instruction('LDIR');
		} elsif ( $y == 0x38 ) {
		    $inst = &Disassemble_Instruction('LDDR');
		}
	    } elsif ( $z == 0x01 ) {
		if ( $y == 0x20 ) {
		    $inst = &Disassemble_Instruction('CPI');
		} elsif ( $y == 0x28 ) {
		    $inst = &Disassemble_Instruction('CPD');
		} elsif ( $y == 0x30 ) {
		    $inst = &Disassemble_Instruction('CPIR');
		} elsif ( $y == 0x38 ) {
		    $inst = &Disassemble_Instruction('CPDR');
		}
	    } elsif ( $z == 0x02 ) {
		if ( $y == 0x20 ) {
		    $inst = &Disassemble_Instruction('INI');
		} elsif ( $y == 0x28 ) {
		    $inst = &Disassemble_Instruction('IND');
		} elsif ( $y == 0x30 ) {
		    $inst = &Disassemble_Instruction('INIR');
		} elsif ( $y == 0x38 ) {
		    $inst = &Disassemble_Instruction('INDR');
		}
	    } elsif ( $z == 0x03 ) {
		if ( $y == 0x20 ) {
		    $inst = &Disassemble_Instruction('OUTI');
		} elsif ( $y == 0x28 ) {
		    $inst = &Disassemble_Instruction('OUTD');
		} elsif ( $y == 0x30 ) {
		    $inst = &Disassemble_Instruction('OTIR');
		} elsif ( $y == 0x38 ) {
		    $inst = &Disassemble_Instruction('OTDR');
		}
	    }
	}
    } elsif ( $prefixType & DIS_Z80_PREFIX_BIT ) {
	my $rz;
	if ( $prefixType & DIS_Z80_PREFIX_I_MASK ) {
	    # since displacement is before opcode,
	    # fake out the width and avoid fake width returned
	    my $fakeWidth;
	    ($problem,$rz,$fakeWidth) = &DisassembleZ80_rz($problem,$addr,$width-2,$z,$ireg,$unsupportedIfIndexing);
	} else {
	    ($problem,$rz,$width) = &DisassembleZ80_rz($problem,$addr,$width,$z,$ireg,$unsupportedIfIndexing);
	}
	if ( $x == 0x00 ) {
	    my $mnem = &DisassembleZ80_roty($y);
	    $inst = &Disassemble_Instruction($mnem,$rz);
	    if ( $mnem eq 'SLL' ) {
		$diffEncountered |= DIS_Z80_DIFF_UNDOC_SLL;
		if ( $diff & DIS_Z80_DIFF_UNDOC_SLL ) {
		    $problem |= DIS_PRBM_UNREPRESENTABLE;
		}
	    }
	} else {
	    my $ybit = &DisassembleZ80_ybit($y);
	    if ( $x == 0x40 ) {
		$inst = &Disassemble_Instruction('BIT',$ybit,$rz);
	    } elsif ( $x == 0x80 ) {
		$inst = &Disassemble_Instruction('RES',$ybit,$rz);
	    } else {
		$inst = &Disassemble_Instruction('SET',$ybit,$rz);
	    }
	}
    } else {
	die "Bad logic for z80 opcode prefix type decode\n";
    }

    return ($problem,$seq,$inst,$width);
}


1;

