###############################################################################
# Copyright (c) 2012 by bgvanbur
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
# Disassemble common Support (for scddisassemble)
###############################################################################

package SCDTools::DisCommon;
use strict;
use warnings;



my @data;

my %comments;

my %labels;
my %labelsToAddr;

my %assembly;

my @codePointCache;
my $codePointCacheEnabled = 1;
my $codePointsDisassembled = 0;
my $codePointsAttempted = 0;
my $codePointsBytes = 0;
my $codePointsBytesUnoptimized = 0;
my $codePointNice = 0;

my $dataPointsBytes = 0;

# default to 68K word data
my $dataValueShift = 1;
my $dataValueAlign = 2;
my $dataValueWidth = 16;
my $dataValueMask  = 0xFFFF;
my $dataValueBigEndian = 1;
# *GetByte, *GetWord, *GetLong, *Disassemble_Data_Align

# old format
# TODO needs to be 6.6 for larger ROMs
# TODO could be smaller for Z80
my $labelGenericFormat = "Label%5.5X";

# vasm 68K needs this to be 1
# gas needs this to be 1
my $labelEquNeedLabelToOffsetFrom = 1;

my $dataLength = 0;

my $org = 0;

my $printAddressCommentsAlways = 0;

require Exporter;
our @ISA = qw(Exporter);
our @EXPORT = qw(
  DATA_VALUE_MASK
  DATA_VALUE_MASK_BYTE
  DATA_VALUE_MASK_WORD
  DATA_USAGE_NONE
  DATA_USAGE_CODE_POINT
  DATA_USAGE_CODE_EXTRA
  DATA_USAGE_CODE
  DATA_USAGE_DATA_POINT
  DATA_USAGE_DATA_EXTRA
  DATA_USAGE_USED_MASK
  DATA_USAGE_LABEL_CUSTOM
  DATA_USAGE_LABEL_GENERIC
  DATA_USAGE_LABEL_MASK
  DATA_USAGE_NEXT_SEQUENTIAL
  DATA_USAGE_NEXT_NONSEQUENTIAL
  DATA_USAGE_NEXT_CONDSEQUENTIAL
  DATA_USAGE_NEXT_MASK
  DATA_USAGE_MASK
  DATA_INVALID_MASK

  DIS_PRBM_NONE
  DIS_PRBM_UNSUPPORTED
  DIS_PRBM_UNOPT
  DIS_PRBM_IGNORED
  DIS_PRBM_UNREPRESENTABLE
  DIS_PRBM_TODO_BAD
  DIS_PRBM_TODO_HORRIBLE
  DIS_PRBM_INVALID_MASK

  DIS_INST_SEQUENTIAL
  DIS_INST_NONSEQUENTIAL
  DIS_INST_CONDSEQUENTIAL

  SetDataWidthToByte
  SetDataWidthToWord
  SetDataToBigEndian
  SetDataToLittleEndian
  SetOrg
  SetLength

  GetDataMasks
  GetDataValue
  GetDataValueDouble
  GetByte
  GetWord
  GetLong
  SetData
  SetDataString
  OrData
  SetCodePoint
  SetDataPoint

  GetComment
  AddComment

  GetLabel
  GetAddrFromLabel
  SetLabel
  LabelGeneric

  GetAssembly
  SetAssembly

  GetCodePointsAttempted
  GetCodePointsDisassembled
  GetCodePointNice
  SetCodePointNice
  AddToCodePointCache
  CodePoint
  ProcessCodePointCache
  AttemptCodePointsAll
  PreviousInst
  UsageStats

  AddUnoptimizedBytes

  PrintAddressCommentsAlways
  PrintDisassembly

  SignExtendByte
  SignExtendWord
  FlipWord
);

###############################################################################
# Data enum
###############################################################################

use constant DATA_VALUE_MASK                  => 0x0000FFFF;
use constant DATA_VALUE_MASK_BYTE             => 0x000000FF;
use constant DATA_VALUE_MASK_WORD             => 0x0000FFFF;

use constant DATA_USAGE_NONE                  => 0x00000000;
use constant DATA_USAGE_CODE_POINT            => 0x00010000;
use constant DATA_USAGE_CODE_EXTRA            => 0x00020000;
use constant DATA_USAGE_CODE                  => 0x00030000;
use constant DATA_USAGE_DATA_POINT            => 0x00040000;
use constant DATA_USAGE_DATA_EXTRA            => 0x00080000;
use constant DATA_USAGE_USED_MASK             => 0x000F0000;
use constant DATA_USAGE_LABEL_CUSTOM          => 0x00100000;
use constant DATA_USAGE_LABEL_GENERIC         => 0x00200000;
use constant DATA_USAGE_LABEL_MASK            => 0x00F00000;
use constant DATA_USAGE_NEXT_SEQUENTIAL       => 0x01000000;
use constant DATA_USAGE_NEXT_NONSEQUENTIAL    => 0x02000000;
use constant DATA_USAGE_NEXT_CONDSEQUENTIAL   => 0x03000000;
use constant DATA_USAGE_NEXT_MASK             => 0x0F000000;
use constant DATA_USAGE_MASK                  => 0x0FFF0000;

use constant DATA_INVALID_MASK                => 0x80000000;

###############################################################################
# Problems enum
###############################################################################

# no problems (so far)
use constant DIS_PRBM_NONE             => 0x0000;
# disassembly not supported (invalid bit data)
# (example $FFFF byte for 68k does not represent a known instruction)
use constant DIS_PRBM_UNSUPPORTED      => 0x0001;
# unoptimized code that always gets optimized by at least one assembler
# (example (0,A0) always gets optimized to (A0) by asmx)
use constant DIS_PRBM_UNOPT            => 0x0010;
# ignored bits that will not properly be set by at least one assembler
# (example byte immediate with upper byte with non-zero data)
use constant DIS_PRBM_IGNORED          => 0x0100;
# cannot represent in assembly syntax but still know decode size
# (example MOVEM with empty list)
use constant DIS_PRBM_UNREPRESENTABLE  => 0x0200;
# disassembly uses an aspect not fully implemented, used to turn off until properly implemented
# BAD means doesn't affect decode size, HORRIBLE means affects decode size
use constant DIS_PRBM_TODO_BAD         => 0x4000;
use constant DIS_PRBM_TODO_HORRIBLE    => 0x8000;
# these problems mean we don't know the instruction decode size
use constant DIS_PRBM_INVALID_MASK     => 0x8001;

###############################################################################
# Sequential enum
###############################################################################

use constant DIS_INST_SEQUENTIAL        => 0x0001;
use constant DIS_INST_NONSEQUENTIAL     => 0x0002;
use constant DIS_INST_CONDSEQUENTIAL    => 0x0003;



use SCDTools::DisFormat;



###############################################################################
# Data
###############################################################################

sub SetDataWidthToByte {
    $dataValueShift = 0;
    $dataValueAlign = 1;
    $dataValueWidth = 8;
    $dataValueMask  = 0x00FF;
    no warnings 'redefine';
    *GetByte = \&SCDTools::DisCommon::GetDataValue;
    *GetWord = \&SCDTools::DisCommon::GetDataValueDouble;
    *GetLong = \&SCDTools::DisCommon::GetDataValueQuad;
    *Disassemble_Data_Align = \&SCDTools::DisCommon::Disassemble_Data_Byte;
}

sub SetDataWidthToWord {
    $dataValueShift = 1;
    $dataValueAlign = 2;
    $dataValueWidth = 16;
    $dataValueMask  = 0xFFFF;
    no warnings 'redefine';
    *GetByte = \&SCDTools::DisCommon::GetDataValueByteForWordAlign;
    *GetWord = \&SCDTools::DisCommon::GetDataValue;
    *GetLong = \&SCDTools::DisCommon::GetDataValueDouble;
    *Disassemble_Data_Align = \&SCDTools::DisCommon::Disassemble_Data_Word;
}

sub SetDataToBigEndian {
    $dataValueBigEndian = 1;
}

sub SetDataToLittleEndian {
    $dataValueBigEndian = 0;
}

sub SetOrg {
    $org = $_[0];
}

sub SetLength {
    $dataLength = $_[0] >> $dataValueShift;
}

sub GetData {
    my $addr = $_[0];
    my $dataIndex = ($addr-$org) >> $dataValueShift;
    if ( $dataIndex >= 0 && $dataIndex < $dataLength ) {
	return $data[$dataIndex];
    }
    return DATA_INVALID_MASK;
}

sub GetDatas {
    my ($addr,$count) = @_;
    my $dataIndex = ($addr-$org) >> $dataValueShift;
    my @result;
    for ( my $i = 0; $i < $count; $i++, $dataIndex++ ) {
	if ( $dataIndex >= 0 && $dataIndex < $dataLength ) {
	    push @result, $data[$dataIndex];
	} else {
	    push @result, DATA_INVALID_MASK;
	}
    }
    return @result;
}

sub GetDataMasks {
    my $addr = $_[0];
    my $width = $_[1];
    my $dataMasks = 0;
    for ( my $i = 0; $i < $width; $i += $dataValueAlign ) {
	$dataMasks |= &GetData($addr+$i);
    }
    return $dataMasks;
}

sub GetDataValue {
    return &GetData(@_) & $dataValueMask;
}

sub GetDataValueDouble {
    my $addr = $_[0];
    my @datas = &GetDatas($addr,2);
    if ( $dataValueBigEndian ) {
	return
	    ( ( $datas[0] & $dataValueMask ) << $dataValueWidth ) |
	    ( ( $datas[1] & $dataValueMask )                    );
    } else {
	return
	    ( ( $datas[1] & $dataValueMask ) << $dataValueWidth ) |
	    ( ( $datas[0] & $dataValueMask )                    );
    }
}

sub GetDataValueQuad {
    my $addr = $_[0];
    my @datas = &GetDatas($addr,4);
    if ( $dataValueBigEndian ) {
	return
	    ( ( $datas[0] & $dataValueMask ) << ($dataValueWidth<<2) ) |
	    ( ( $datas[1] & $dataValueMask ) << ($dataValueWidth<<1) ) |
	    ( ( $datas[2] & $dataValueMask ) <<  $dataValueWidth     ) |
	    ( ( $datas[3] & $dataValueMask )                         );
    } else {
	return
	    ( ( $datas[3] & $dataValueMask ) << ($dataValueWidth<<2) ) |
	    ( ( $datas[2] & $dataValueMask ) << ($dataValueWidth<<1) ) |
	    ( ( $datas[1] & $dataValueMask ) <<  $dataValueWidth     ) |
	    ( ( $datas[0] & $dataValueMask )                         );
    }
}

sub GetDataValueByteForWordAlign {
    my $addr = $_[0];
    my $dataValue = &GetDataValue($addr);
    if ( ( $addr & 1 ) == 0 ) {
	# upper byte of word data requested
	return ( $dataValue >> 8 ) & 0xFF;
    }
    # lower byte of word data requested
    return $dataValue & 0xFF;
}

sub SetData {
    my ($addr,$value) = @_;
    my $dataIndex = ($addr-$org) >> $dataValueShift;
    if ( $dataIndex >= 0 && $dataIndex < $dataLength ) {
	$data[$dataIndex] = $value;
    }
}

sub SetDataString {
    my ($addr,$value,$width) = @_;
    my $end = $width;
    # TODO gas --mri does not like this routine as it stands
    # determine last index, if outside, truncate data string end used
    my $dataIndex = ($addr+$width-$org) >> $dataValueShift;
    if ( $dataIndex >= $dataLength ) {
	$end -= ( $dataIndex - $dataLength ) << $dataValueShift;
    }
    if ( $end < 0 ) {
	return;
    }
    # determine first index, if outside, truncate data string start used
    $dataIndex = ($addr-$org) >> $dataValueShift;
    my $start = 0;
    if ( $dataIndex < 0 ) {
	$start = ( 0 - $dataIndex ) << $dataValueShift;
	$dataIndex = 0;
    }
    # set the data in the valid range
    if ( $dataValueAlign == 2 ) {
	for ( my $i = $start; $i < $end; $i += $dataValueAlign, $dataIndex++ ) {
	    $data[$dataIndex] = unpack("n",substr($value,$i,$dataValueAlign));
	}
    } elsif ( $dataValueAlign == 1 ) {
	for ( my $i = $start; $i < $end; $i += $dataValueAlign, $dataIndex++ ) {
	    $data[$dataIndex] = ord(substr($value,$i,$dataValueAlign));
	}
    } else {
	die "Bad logic: SetDataString\n";
    }
}

sub OrData {
    my ($addr,$value) = @_;
    my $dataIndex = ($addr-$org) >> $dataValueShift;
    if ( $dataIndex >= 0 && $dataIndex < $dataLength ) {
	$data[$dataIndex] |= $value;
    }
}

sub SetCodePoint {
    my ($addr,$width) = @_;
    &OrData($addr,DATA_USAGE_CODE_POINT);
    for ( my $i = $dataValueAlign; $i < $width; $i += $dataValueAlign ) {
	&OrData($addr+$i,DATA_USAGE_CODE_EXTRA);
    }
}

sub SetDataPoint {
    my ($addr,$width) = @_;
    &OrData($addr,DATA_USAGE_DATA_POINT);
    for ( my $i = $dataValueAlign; $i < $width; $i += $dataValueAlign ) {
	&OrData($addr+$i,DATA_USAGE_DATA_EXTRA);
    }
    $dataPointsBytes += $width;
}

###############################################################################
# Comments
###############################################################################

sub GetComment {
    my ($addr) = @_;
    return exists $comments{$addr} ? $comments{$addr} : '';
}

sub AddComment {
    my ($addr,$value) = @_;
    $comments{$addr} .= $value;
}

###############################################################################
# Labels
###############################################################################

sub GetLabel {
    my ($addr) = @_;
    return exists $labels{$addr} ? $labels{$addr} : '';
}

sub GetAddrFromLabel {
    my ($label) = @_;
    return exists $labelsToAddr{$label} ? $labelsToAddr{$label} : -1;
}

sub SetLabel {
    my ($addr,$label) = @_;
    $labels{$addr} = $label;
    $labelsToAddr{$label} = $addr;
}

# TODO rename
sub LabelGeneric {
    my ($problem,$addr) = @_;
    my $label;
    my $dataUsage = &GetData($addr);
    # TODO when doing small section of code how to handle out of scope labels
    if ( ( $dataUsage & DATA_INVALID_MASK ) == 0 ) {
	$label = &GetLabel($addr);
	if ( $label eq '' ) {
	    # TODO make labels when?
	    if ( 1 || $codePointNice ) {
		$label = sprintf($labelGenericFormat,$addr);
		&OrData($addr,DATA_USAGE_LABEL_GENERIC);
		&SetLabel($addr,$label);
	    } else {
		# TODO avoid labels when doing -all portion?
		$problem = DIS_PRBM_TODO_BAD;
		$label = '?';
	    }
	}
    } else {
	$problem = DIS_PRBM_TODO_BAD;
	$label = '?';
    }
    return ($problem,$label);
}

###############################################################################
# Assembly
###############################################################################

sub GetAssembly {
    my ($addr) = @_;
    return exists $assembly{$addr} ? $assembly{$addr} : '';
}

sub SetAssembly {
    my ($addr,$value) = @_;
    $assembly{$addr} = $value;
}


###############################################################################
# Code Points
###############################################################################

sub GetCodePointsAttempted {
    return $codePointsAttempted;
}

sub GetCodePointsDisassembled {
    return $codePointsDisassembled;
}

sub GetCodePointsBytes {
    return $codePointsBytes;
}

sub GetCodePointNice {
    return $codePointNice;
}

sub SetCodePointNice {
    $codePointNice = $_[0];
}


# useful for Disassembly functions as a shortcut to adding other code points
# for branches, jumps, subroutines and sequential code
sub AddToCodePointCache {
    if ( $codePointNice ) {
	push @codePointCache, @_;
    }
}

sub CodePoint {
    $codePointsAttempted++;
    &CodePointRun(@_);
    &ProcessCodePointCache();
}

sub ProcessCodePointCache {
    while ( $#codePointCache >= 0 ) {
	my $addr = pop @codePointCache;
	$codePointsAttempted++;
	&CodePointRun($addr);
    }
}

sub AttemptCodePointsAll {
    for ( my $i = 0; $i < $dataLength; $i++ ) {
	my $addr = $org + $i * $dataValueAlign;
	&CodePointRun($addr);
    }
}

# returns address or -1 on failure to get previous instruction
sub PreviousInst {
    my $addr = $_[0];
    my $dataUsage;
    do {
	if ( $addr == 0 ) {
	    return -1;
	}
	$addr -= $dataValueAlign;
	$dataUsage = &GetData($addr);
    } while ( $dataUsage & DATA_USAGE_CODE_EXTRA );
    if ( $dataUsage & DATA_USAGE_CODE_POINT ) {
	return $addr;
    }
    return -1;
}

# should not be called directly by outside this module
sub CodePointRun {
    my $addr = $_[0];
    my $dataUsage = &GetData($addr);
    # don't use invalid code location or already used location
    if ( ( $dataUsage & ( DATA_USAGE_USED_MASK | DATA_INVALID_MASK ) ) == 0 ) {
	my ($problem,$seq,$inst,$width) = &SCDTools::DisPick::Disassemble($addr);

	if ( $inst eq '' ) {
	    $problem |= DIS_PRBM_UNSUPPORTED;
	}
	if ( $problem & DIS_PRBM_INVALID_MASK ) {
	    $inst = '';
	}

	if ( ! ( $problem & DIS_PRBM_INVALID_MASK ) &&
	     ( &GetDataMasks($addr,$width) & ( DATA_USAGE_USED_MASK | DATA_INVALID_MASK ) ) == 0 ) {

	    if ( $problem ) {
		my $optCode = ( $inst ne '' );
		if ( $optCode ) {
		    chomp $inst;
		    $inst =~ s/^\s+//;
		    $inst =~ s/\s+$//;
		    $inst = &Disassemble_Comment("OPT ".$inst);
		} else {
		    $inst = '';
		}
		# if ( $codePointNice ) {
		#     print STDERR $inst;
		# }
		$inst .= &Disassemble_Data_Align($addr,$width);
		if ( $optCode ) {
		    $inst = &Disassemble_EndOfLineComment($inst,"UNOPT");
		}
	    }

	    &OrData($addr,$seq);
	    &SetCodePoint($addr,$width);
	    $assembly{$addr} = $inst;
	    $codePointsDisassembled++;
	    $codePointsBytes += $width;
	    if ( $seq & DATA_USAGE_NEXT_SEQUENTIAL ) {
		&AddToCodePointCache($addr+$width);
	    }
	} else {
	    # failure to disassemble
	    if ( $codePointNice ) {
		print STDERR sprintf("Code disassembly failure at address: %6.6X\n",$addr);
	    }
	}
    }
}

sub UsageStats {
    print STDERR sprintf("Code Points: %d\n",$codePointsDisassembled);
    print STDERR sprintf("Code Points Bytes: %d\n",$codePointsBytes);
    print STDERR sprintf("Code Points Bytes Unoptimized: %d\n",$codePointsBytesUnoptimized);
    print STDERR sprintf("Code Points Usage: %d%%\n",100*$codePointsBytes/($dataLength<<$dataValueShift));
    if ( $dataPointsBytes ) {
	print STDERR sprintf("Data Points Bytes: %d\n",$dataPointsBytes);
	print STDERR sprintf("Data Points Usage: %d%%\n",100*$dataPointsBytes/($dataLength<<$dataValueShift));
    }
}

sub AddUnoptimizedBytes {
    if ( $codePointNice ) {
	$codePointsBytesUnoptimized += $_[0];
    }
}

sub PrintAddressCommentsAlways {
    $printAddressCommentsAlways = $_[0];
}

sub PrintDisassembly {
    my ($asmFile) = @_;

    my $asmHandle;
    if ( $asmFile ne '' ) {
	open($asmHandle,'>'.$asmFile);
    } else {
	$asmHandle = *STDOUT;
    }

    # group output for every 1024 bytes worth of data
    my $output = '';

    if ( &GetAlwaysOrg() || $org > 0 ) {
	$output .= &Disassemble_Org($org);
    }

    # TODO document for vasm
    my $labelOffset = '';
    if ( $labelEquNeedLabelToOffsetFrom ) {
	my $problem = 0;
	my $labelOrg;
	($problem,$labelOrg) = &LabelGeneric($problem,$org);
	$labelOffset = $labelOrg.'+';
    }

    for ( my $i = 0; $i < $dataLength; $i++ ) {
	if ( ( $i & 0x0003FF ) == 0 && $i > 0 && $output ne '' ) {
	    print $asmHandle $output;
	    $output = '';
	}
	my $addr = $org + $i * $dataValueAlign;
	my $dataUsage = $data[$i];
	if ( exists $comments{$addr} ) {
	    $output .= $comments{$addr};
	}
	if ( $dataUsage & DATA_USAGE_LABEL_MASK ) {
	    if ( exists $labels{$addr} ) {
		my $label = $labels{$addr};
		if ( $dataUsage & DATA_USAGE_CODE_EXTRA ||
		     $dataUsage & DATA_USAGE_DATA_EXTRA ) {
		    # TODO make equate instead of label
		    $output .= &Disassemble_Equ($label,$labelOffset.&Disassemble_Value_Address($addr-$org));
		} else {
		    $output .= sprintf("%s:\n",$label);
		}
	    }
	    for ( my $i = 1; $i < $dataValueAlign; $i++ ) {
		if ( exists $labels{$addr+$i} ) {
		    my $label = $labels{$addr+$i};
		    # TODO make equate instead of label
		    $output .= &Disassemble_Equ($label,$labelOffset.&Disassemble_Value_Address($addr+$i-$org));
		}
	    }
	}
	if ( $dataUsage & DATA_USAGE_CODE_POINT ) {
	    if ( $printAddressCommentsAlways ) {
		$output .= &Disassemble_Comment(&Disassemble_Value_Address($addr));
	    }
	    $output .= $assembly{$addr};
	} elsif ( $dataUsage & DATA_USAGE_DATA_POINT ) {
	    if ( $printAddressCommentsAlways ) {
		$output .= &Disassemble_Comment(&Disassemble_Value_Address($addr));
	    }
	    if ( ! exists $assembly{$addr} ) {
		print STDERR sprintf("addr %sn",&Disassemble_Value_Address($addr));
	    }
	    $output .= $assembly{$addr};
	} elsif ( $dataUsage & DATA_USAGE_CODE_EXTRA ) {
	    # do nothing
	} elsif ( $dataUsage & DATA_USAGE_DATA_EXTRA ) {
	    # do nothing
	} else {
	    if ( $printAddressCommentsAlways ||
		 ! ( $dataUsage & DATA_USAGE_LABEL_MASK ) ) {
		$output .= &Disassemble_Comment(&Disassemble_Value_Address($addr));
	    }
	    $output .= &Disassemble_Data_Align($addr,$dataValueAlign);
	}
    }
    if ( $output ne '' ) {
	print $asmHandle $output;
	$output = '';
    }

    if ( $asmFile ne '' ) {
	close $asmHandle;
    }
}


###############################################################################
# Data conversions
###############################################################################

sub SignExtendByte {
    my $value = $_[0];
    if ( $value & 0x00000080 ) {
	$value = 0xFFFFFF80 | ( $value & 0x0000007F );
    } else {
	$value =              ( $value & 0x0000007F );
    }
    return $value;
}

sub SignExtendWord {
    my $value = $_[0];
    if ( $value & 0x00008000 ) {
	$value = 0xFFFF8000 | ( $value & 0x00007FFF );
    } else {
	$value =              ( $value & 0x00007FFF );
    }
    return $value;
}

sub FlipWord {
    my $value = $_[0];
    my $result = 0;
    for ( my $i = 0; $i <= 15; $i++ ) {
	if ( ( $value >> $i ) & 1 ) {
	    $result |= ( 1 << ( 15 - $i ) );
	}
    }
    return $result;
}


1;
