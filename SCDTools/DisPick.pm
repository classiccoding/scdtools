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
# Disassemble Support (for scddisassemble)
###############################################################################

package SCDTools::DisPick;
use strict;
use warnings;

require Exporter;
our @ISA = qw(Exporter);
our @EXPORT = qw(
  CPU_68K
  CPU_Z80

  ASM_ASL
  ASM_ASMX
  ASM_GAS
  ASM_GAS_MRI
  ASM_PASMO
  ASM_SJASM
  ASM_SNASM68K
  ASM_TASM
  ASM_VASM
  ASM_Z80ASM

  PickReset
  PickCPU
  PickAssembler
  PickComplete

  GetCPU
  GetAssembler

  Disassemble
  Disassembly_NiceCheck
  Disassembly_MnemCheck
);


use constant CPU_MASK     => 0x000000FF;
use constant ASM_MASK     => 0x00FFFF00;

use constant CPU_68K      => 0x00000001;
use constant CPU_Z80      => 0x00000002;

# does both 68k and z80
use constant ASM_ASL      => 0x00000100;
use constant ASM_ASMX     => 0x00000200;
use constant ASM_VASM     => 0x00000400;

# special case does both 68k and z80
use constant ASM_GAS      => 0x00001000;

# does just 68k
use constant ASM_SNASM68K => 0x00010000;
use constant ASM_GAS_MRI  => 0x00020000;

# does just z80
use constant ASM_PASMO    => 0x00100000;
use constant ASM_SJASM    => 0x00200000;
use constant ASM_TASM     => 0x00400000;
use constant ASM_Z80ASM   => 0x00800000;

use constant ASM_MASK_68K => 0x000FFF00;
use constant ASM_MASK_Z80 => 0x00F0FF00;

my $target = 0;



use SCDTools::DisCommon;
use SCDTools::DisFormat;


sub PickReset {
    $target = 0;
}

sub PickCPU {
    $target = $target | ( $_[0] & CPU_MASK );
}

sub PickAssembler {
    $target = $target | ( $_[0] & ASM_MASK );
}

sub GetCPU {
    return ( $target & CPU_MASK );
}

sub GetAssembler {
    return ( $target & ASM_MASK );
}

sub PickComplete {
    my $cpu = &GetCPU();
    my $asm = &GetAssembler();
    if ( $cpu == 0 ) {
	$cpu = CPU_68K;
    }
    if ( $cpu == CPU_68K ) {
	if ( $asm == 0 ) {
	    # not GAS by default
	    $asm = ASM_SNASM68K | ASM_ASMX | ASM_ASL | ASM_VASM | ASM_GAS_MRI;
	    &PickAssembler($asm);
	}
	no warnings 'redefine';
	*Disassemble = \&SCDTools::Dis68K::Disassemble68K;
	*Disassembly_NiceCheck = \&SCDTools::Dis68K::Disassemble68K_NiceCheck;
	*Disassembly_MnemCheck = \&SCDTools::Dis68K::Disassemble68K_MnemCheck;
	&SetAlwaysOrg(0);
	&SetDataToBigEndian();
	&SetDataWidthToWord();
	&ChangeImmediateFormat('$','');
	&SetAddressWidth(24);
	&ChangeComment(';;');
	&ChangeDataDirectiveKeywords('DC.B','DC.W','DC.L');
	&ChangeDataStringDirectiveKeyword('DC.B');
	&SetDirectiveStringOnlyIfAllPrintable(0);
	&ChangeEquUsesColon(1);
	&ChangeEquUsesLabel(1);
	if ( $target & ASM_GAS ) {
	    &ChangeImmediateFormat('0x','');
	    &ChangeComment('|');
	    &ChangeDataDirectiveKeywords('.byte','.word','.long');
	    &ChangeDataStringDirectiveKeyword('.ascii');
	    &SetDirectiveStringOnlyIfAllPrintable(1);
	    &SetCaseReg(0);
	    &ChangeOrgDirectiveKeyword('.org');
	    &ChangeEquDirectiveKeyword('.equ');
	    &ChangeEquUsesLabel(0);
	}
	&SCDTools::Dis68K::Disassemble68K_Setup();
    } elsif ( $cpu == CPU_Z80 ) {
	if ( $asm == 0 ) {
	    # not TASM by default
	    $asm = ASM_ASL | ASM_ASMX | ASM_GAS | ASM_PASMO | ASM_SJASM | ASM_VASM | ASM_Z80ASM;
	    &PickAssembler($asm);
	}
	no warnings 'redefine';
	*Disassemble = \&SCDTools::DisZ80::DisassembleZ80;
	*Disassembly_NiceCheck = \&SCDTools::DisZ80::DisassembleZ80_NiceCheck;
	*Disassembly_MnemCheck = \&SCDTools::DisZ80::DisassembleZ80_MnemCheck;
	&SetAlwaysOrg(1);
	&SetDataToLittleEndian();
	&SetDataWidthToByte();
	&ChangeImmediateFormat('0','H');
	&SetAddressWidth(16);
	&ChangeComment(';;');
	&ChangeDataDirectiveKeywords('DB','DW','?');
	&ChangeDataStringDirectiveKeyword('DC.B');
	&SetDirectiveStringOnlyIfAllPrintable(0);
	&ChangeEquUsesColon(1);
	&ChangeEquUsesLabel(1);
	if ( $target & ASM_TASM ) {
	    &SetAlwaysOrg(0);
	    &ChangeDataDirectiveKeywords('.DB','.DW','?');
	}
        # VASM May 2, 2014 1.6 build does not need this
	# if ( $target & ASM_VASM ) {
	#     &ChangeImmediateFormat('0x','');
	#     &ChangeEquUsesColon(0);
	# }
	&SCDTools::DisZ80::DisassembleZ80_Setup();
    } else {
	die "Too many CPUs specified\n";
    }
}


# Disassemble
# pass in [0] address of instruction
# returns [0] DIS_PRBM to note any disassembly problems
# returns [1] DATA_USAGE_NEXT information
# returns [2] string representation of instruction
# returns [3] width of instruction in bytes

# TODO document other instruction common

1;
