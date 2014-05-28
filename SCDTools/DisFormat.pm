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
# Disassemble format Support (for scddisassemble)
###############################################################################

package SCDTools::DisFormat;
use strict;
use warnings;

require Exporter;
our @ISA = qw(Exporter);
our @EXPORT = qw(
  SaveCaseUser
  RestoreCaseUser
  SetCase
  SetCaseDir
  SetCaseMnem
  SetCaseReg
  SetCaseImm

  GetCaseDir
  GetCaseMnem
  GetCaseReg
  GetCaseImm

  ApplyCaseDir
  ApplyCaseMnem
  ApplyCaseReg
  ApplyCaseImm

  GetAlwaysOrg
  SetAlwaysOrg
  ChangeComment
  ChangeOrgDirectiveKeyword
  ChangeEquDirectiveKeyword
  ChangeImmediateFormat
  ChangeDataDirectiveKeywords
  ChangeDataStringDirectiveKeyword
  SetDirectiveStringOnlyIfAllPrintable
  ChangeEquUsesColon
  ChangeEquUsesLabel
  SetAddressWidth

  Disassemble_Org
  Disassemble_Equ

  Disassemble_Instruction
  Disassemble_Comment
  Disassemble_EndOfLineComment
  Disassemble_Data_Byte
  Disassemble_Data_Word
  Disassemble_Data_Long
  Disassemble_Data_Byte_Expr
  Disassemble_Data_Word_Expr
  Disassemble_Data_Long_Expr
  Disassemble_Data_String
  Disassemble_Data_Align

  Disassemble_Value_3Bits
  Disassemble_Value_Nibble
  Disassemble_Value_Byte
  Disassemble_Value_Byte_Signed
  Disassemble_Value_Word
  Disassemble_Value_Word_Signed
  Disassemble_Value_24Bits
  Disassemble_Value_Long
  Disassemble_Value_Address
);


###############################################################################
# Spacing enum
###############################################################################

use constant SPACING_NONE        => 0x0000;
use constant SPACING_COUNT_MASK  => 0x00FF;
use constant SPACING_TAB         => 0x0100;
use constant SPACING_SPACE       => 0x0200;
use constant SPACING_FIXED       => 0x0400;
use constant SPACING_SPACE_FIXED => 0x0600;


# upper case by default
my $caseDir  = 1;
my $caseMnem = 1;
my $caseReg  = 1;
my $caseImm  = 1;
my @caseUser = (1,1,1,1);

my @instructionFormat;
# tab format (annoying)
$instructionFormat[0] = "\t%s\n";
$instructionFormat[1] = "\t%s\t%s\n";
$instructionFormat[2] = "\t%s\t%s,%s\n";

# 68kd format
$instructionFormat[0] = "\t%s\n";
$instructionFormat[1] = "\t%-9s%s\n";
$instructionFormat[2] = "\t%-9s%s,%s\n";

# 000000    111   2 3
#       MNEM   OP1 , OP2
my @spacing = (SPACING_TAB|1,SPACING_SPACE_FIXED|9,SPACING_NONE,SPACING_NONE);
#my @spacing = (SPACING_TAB|1,SPACING_SPACE_FIXED|9,SPACING_NONE,SPACING_SPACE_FIXED|20);

my $alwaysOrg = 0;
my $equColon = ':';
my $equLabel = 1;

# 3bits/nibble/byte/word/long formats
my $immediateFormatPrefix = '$';
my $immediateFormatSuffix = '';
my $addressWidth       = 32;
my $valueFormat3Bits   =   "%1.1X";
my $valueFormatNibble  = "\$%1.1X";
my $valueFormatByte    = "\$%2.2X";
my $valueFormatWord    = "\$%4.4X";
my $valueFormat24Bits  = "\$%6.6X";
my $valueFormatLong    = "\$%8.8X";
my $valueFormatAddress = "\$%8.8X";

my $directiveByte   = 'dc.b';
my $directiveWord   = 'dc.w';
my $directiveLong   = 'dc.l';
my $directiveString = 'dc.b';
my $directiveOrg    = 'org';
my $directiveEqu    = 'equ';

my $directiveStringOnlyIfAllPrintable = 0;

my $commentFormat          = "\t;; %s\n";
my $commentFormatEndOfLine =  " ;; %s";     # do not include \n

# old format
# TODO needs to be 6.6 for larger ROMs
# TODO could be smaller for Z80
my $labelGenericFormat = "Label%5.5X";

# vasm 68K needs this to be 1
# gas needs this to be 1
my $labelEquNeedLabelToOffsetFrom = 1;



sub SaveCaseUser {
    @caseUser = ($caseDir,$caseMnem,$caseReg,$caseImm);
}
sub RestoreCaseUser {
    &SetCaseDir( $caseUser[0]);
    &SetCaseMnem($caseUser[1]);
    &SetCaseReg( $caseUser[2]);
    &SetCaseImm( $caseUser[3]);
}
sub SetCase {
    my $case = $_[0];
    &SetCaseDir( $case);
    &SetCaseMnem($case);
    &SetCaseReg( $case);
    &SetCaseImm( $case);
}
sub SetCaseDir {
    my $case = $_[0];
    $caseDir  = $case;
}
sub SetCaseMnem {
    my $case = $_[0];
    $caseMnem = $case;
}
sub SetCaseReg {
    my $case = $_[0];
    $caseReg  = $case;
}
sub SetCaseImm {
    my $case = $_[0];
    $caseImm  = $case;
    &UpdateImmediateFormat();
}

sub GetCaseDir {
    return $caseDir;
}
sub GetCaseMnem {
    return $caseMnem;
}
sub GetCaseReg {
    return $caseReg;
}
sub GetCaseImm {
    return $caseImm;
}

sub ApplyCaseDir {
    return $caseDir  ? uc($_[0]) : lc($_[0]);
}
sub ApplyCaseMnem {
    return $caseMnem ? uc($_[0]) : lc($_[0]);
}
sub ApplyCaseReg {
    return $caseReg  ? uc($_[0]) : lc($_[0]);
}
sub ApplyCaseImm {
    return $caseImm  ? uc($_[0]) : lc($_[0]);
}



sub GetAlwaysOrg {
    return $alwaysOrg;
}
sub SetAlwaysOrg {
    $alwaysOrg = $_[0];
}

sub ChangeComment {
    my $type = $_[0];
    $commentFormat          = "\t$type %s\n";
    $commentFormatEndOfLine =  " $type %s";     # do not include \n
}

sub ChangeOrgDirectiveKeyword {
    $directiveOrg = $_[0];
}

sub ChangeEquDirectiveKeyword {
    $directiveEqu = $_[0];
}

sub UpdateImmediateFormat {
    my $base = $caseImm ? 'X' : 'x';
    $valueFormatNibble  = $immediateFormatPrefix."%1.1".$base.$immediateFormatSuffix;
    $valueFormatByte    = $immediateFormatPrefix."%2.2".$base.$immediateFormatSuffix;
    $valueFormatWord    = $immediateFormatPrefix."%4.4".$base.$immediateFormatSuffix;
    $valueFormat24Bits  = $immediateFormatPrefix."%6.6".$base.$immediateFormatSuffix;
    $valueFormatLong    = $immediateFormatPrefix."%8.8".$base.$immediateFormatSuffix;
    &UpdateValueFormatAddress();
}

sub ChangeImmediateFormat {
    ($immediateFormatPrefix,$immediateFormatSuffix) = @_;
    &UpdateImmediateFormat();
}

sub ChangeDataDirectiveKeywords {
    ($directiveByte,$directiveWord,$directiveLong) = @_;
}

sub ChangeDataStringDirectiveKeyword {
    $directiveString = $_[0];
}

sub SetDirectiveStringOnlyIfAllPrintable {
    $directiveStringOnlyIfAllPrintable = $_[0];
}

sub ChangeEquUsesColon {
    if ( $_[0] ) {
	$equColon = ':';
    } else {
	$equColon = '';
    }
}

sub ChangeEquUsesLabel {
    $equLabel = $_[0];
}

sub SetAddressWidth {
    $addressWidth = $_[0];
    &UpdateValueFormatAddress();
}

sub UpdateValueFormatAddress {
    if ( $addressWidth == 8 ) {
	$valueFormatAddress = $valueFormatByte;
    } elsif ( $addressWidth == 16 ) {
	$valueFormatAddress = $valueFormatWord;
    } elsif ( $addressWidth == 16 ) {
	$valueFormatAddress = $valueFormatWord;
    } elsif ( $addressWidth == 24 ) {
	$valueFormatAddress = $valueFormat24Bits;
    } else {
	$valueFormatAddress = $valueFormatLong;
    }
}

sub Disassemble_Equ {
    my ($label,$value) = @_;
    if ( ! $equLabel ) {
	# gas format: .equ symbol, expression
	return &Disassemble_Directive($directiveEqu,$label,$value);
    }
    # label: equ expression
    # TODO wish I could use directive format:
    return sprintf("%s%s\t%s\t%s\n",$label,$equColon,$directiveEqu,$value);
}

sub Disassemble_Directive {
    # TODO return sprintf($instructionFormat[$#_],@_);
    return &Disassemble_Spacing($caseDir,@_);
}

sub Disassemble_Org {
    my $org = $_[0];
    return &Disassemble_Directive($directiveOrg,&Disassemble_Value_Address($org));
}

sub Disassemble_Spacing {
    my $caseKeyword = $_[0];
    my $keyword = $_[1];
    my $operandCountPlus1 = $#_;
    my $result = '';
    # remember to make default spacing the first checked for each
    # space so that it is the fastest execution
    if ( $spacing[0] & SPACING_TAB ) {
	$result .= "\t" x ( $spacing[0] & SPACING_COUNT_MASK );
    } elsif ( $spacing[0] & SPACING_SPACE ) {
	# fixed spacing does nothing here, so ignore
	$result .= " " x ( $spacing[0] & SPACING_COUNT_MASK );
    }
    $result .= $caseKeyword ? uc($keyword) : lc($keyword);
    if ( $operandCountPlus1 > 1 ) {
	if ( $spacing[1] & SPACING_SPACE ) {
	    my $count = $spacing[1] & SPACING_COUNT_MASK;
	    if ( $spacing[1] & SPACING_FIXED ) {
		$count -= length($keyword);
	    }
	    if ( $count > 0 ) {
		$result .= " " x $count;
	    }
	} elsif ( $spacing[1] & SPACING_TAB ) {
	    $result .= "\t" x ( $spacing[1] & SPACING_COUNT_MASK );
	}
	$result .= $_[2];
	for ( my $i = 3; $i <= $operandCountPlus1; $i++ ) {
	    if ( $spacing[2] ) {
		if ( $spacing[2] & SPACING_SPACE ) {
		    my $count = $spacing[2] & SPACING_COUNT_MASK;
		    if ( $spacing[2] & SPACING_FIXED ) {
			$count -= length($_[$i-1]);
		    }
		    if ( $count > 0 ) {
			$result .= " " x $count;
		    }
		} elsif ( $spacing[2] & SPACING_TAB ) {
		    $result .= "\t" x ( $spacing[2] & SPACING_COUNT_MASK );
		}
	    }
	    $result .= ',';
	    if ( $spacing[3] ) {
		if ( $spacing[3] & SPACING_SPACE ) {
		    my $count = $spacing[3] & SPACING_COUNT_MASK;
		    if ( $spacing[3] & SPACING_FIXED ) {
			# include comma
			# does not include previous spaces or tabs
			# since it doesn't seem like a practical usage
			$count -= length($_[$i-1])-1;
		    }
		    if ( $count > 0 ) {
			$result .= " " x $count;
		    }
		} elsif ( $spacing[3] & SPACING_TAB ) {
		    $result .= "\t" x ( $spacing[3] & SPACING_COUNT_MASK );
		}
	    }
	    $result .= $_[$i];
	}
    }
    return $result."\n";
}

sub Disassemble_Instruction {
    # TODO return sprintf($instructionFormat[$#_],@_);
    return &Disassemble_Spacing($caseMnem,@_);
}

sub Disassemble_Comment {
    my $result = '';
    foreach my $comment (@_) {
	# remove any newlines and beginning whitespace
	my $comment2 = $comment;
	chomp $comment2;
	$comment2 =~ s/^\s+//;
	return sprintf($commentFormat,$comment2);
    }
    return $result;
}

sub Disassemble_EndOfLineComment {
    my ($str,$comment) = @_;
    my $commentFull = sprintf($commentFormatEndOfLine,$comment);
    $str =~ s/\s*(\n\s*)$/$commentFull$1/;
    return $str;
}

sub Disassemble_Data_Byte {
    my ($addr,$width) = @_;
    my $inst = '';
    for ( my $i = 0; $i < $width; $i += 16 ) {
	my $data = '';
	for ( my $j = 0; $j < 16 && $i + $j < $width; $j++ ) {
	    if ( $j > 0 ) {
		$data .= ",";
	    }
	    $data .= &Disassemble_Value_Byte(&SCDTools::DisCommon::GetByte($addr+$i+$j));
	}
	$inst .= &Disassemble_Directive($directiveByte,$data);
    }
    return $inst;
}

sub Disassemble_Data_Word {
    my ($addr,$width) = @_;
    my $inst = '';
    for ( my $i = 0; $i < $width; $i += 16 ) {
	my $data = '';
	for ( my $j = 0; $j < 16 && $i + $j < $width; $j += 2 ) {
	    if ( $j > 0 ) {
		$data .= ",";
	    }
	    $data .= &Disassemble_Value_Word(&SCDTools::DisCommon::GetWord($addr+$i+$j));
	}
	$inst .= &Disassemble_Directive($directiveWord,$data);
    }
    return $inst;
}

sub Disassemble_Data_Long {
    my ($addr,$width) = @_;
    my $inst = '';
    for ( my $i = 0; $i < $width; $i += 16 ) {
	my $data = '';
	for ( my $j = 0; $j < 16 && $i + $j < $width; $j += 4 ) {
	    if ( $j > 0 ) {
		$data .= ",";
	    }
	    $data .= &Disassemble_Value_Long(&SCDTools::DisCommon::GetLong($addr+$i+$j));
	}
	$inst .= &Disassemble_Directive($directiveLong,$data);
    }
    return $inst;
}

sub Disassemble_Data_Byte_Expr {
    return  &Disassemble_Data_List_Expr($directiveByte,@_);
}
sub Disassemble_Data_Word_Expr {
    return  &Disassemble_Data_List_Expr($directiveWord,@_);
}
sub Disassemble_Data_Long_Expr {
    return  &Disassemble_Data_List_Expr($directiveLong,@_);
}

sub Disassemble_Data_List_Expr {
    my $directive = shift @_;
    my $inst = '';
    foreach my $expr (@_) {
	$inst .= &Disassemble_Directive($directive,$expr);
    }
    return $inst;
}

# TODO should we allow strings? gasmri/asl conflict on 'text' versus "text"
my $commentStringInstead = 1;

sub Disassemble_Data_String {
    my ($addr,$width) = @_;
    my $data = '';
    my $inString = 0;
    my $strDelim = '"';
    if ( $commentStringInstead || $directiveStringOnlyIfAllPrintable ) {
	for ( my $i = 0; $i < $width; $i += 1 ) {
	    my $byte = &SCDTools::DisCommon::GetByte($addr+$i);
	    if ( ! ( $byte >= 0x20 && $byte < 0x7F ) ) {
		return &Disassemble_Data_Byte(@_);
	    }
	}
    }
    for ( my $i = 0; $i < $width; $i += 1 ) {
	my $byte = &SCDTools::DisCommon::GetByte($addr+$i);
	if ( $byte >= 0x20 && $byte < 0x7F ) {
	    # printable characters
	    if ( ! $inString ) {
		if ( $i > 0 ) {
		    $data .= ',';
		}
		# open string
		$data .= $strDelim;
	    }
	    if ( chr($byte) eq $strDelim ) {
		# need to escape string delimiter
		$data .= '\\'.$strDelim;
	    } else {
		$data .= sprintf("%c",$byte);
	    }
	    $inString = 1;
	} else {
	    # non-printable characters
	    if ( $inString ) {
		# close string
		$data .= $strDelim;
	    }
	    if ( $i > 0 ) {
		$data .= ',';
	    }
	    $data .= &Disassemble_Value_Byte($byte);
	    $inString = 0;
	}
    }
    if ( $inString ) {
	# close string
	$data .= $strDelim;
    }
    if ( $commentStringInstead ) {
	return &Disassemble_Comment($data).&Disassemble_Data_Byte(@_);
    }
    return &Disassemble_Directive($directiveString,$data);
}

sub Disassemble_Value_3Bits {
    # caller needs to verify bits within lowest nibble
    # since also used to print 1-8 values
    return sprintf($valueFormat3Bits,$_[0] & 0xF);
}
sub Disassemble_Value_Nibble {
    return sprintf($valueFormatNibble,$_[0] & 0xF);
}
sub Disassemble_Value_Byte {
    return sprintf($valueFormatByte,$_[0] & 0xFF);
}
sub Disassemble_Value_Byte_Signed {
    my $data = $_[0] & 0xFF;
    if ( $data & 0x80 ) {
	my $dataNeg = ( $data ^ 0xFF ) + 1;
	return '-'.sprintf($valueFormatByte,$dataNeg);
    }
    return sprintf($valueFormatByte,$data);
}
sub Disassemble_Value_Word {
    return sprintf($valueFormatWord,$_[0] & 0xFFFF);
}
sub Disassemble_Value_Word_Signed {
    my $data = $_[0] & 0xFFFF;
    if ( $data & 0x8000 ) {
	my $dataNeg = ( $data ^ 0xFFFF ) + 1;
	return '-'.sprintf($valueFormatWord,$dataNeg);
    }
    return sprintf($valueFormatWord,$data);
}
sub Disassemble_Value_24Bits {
    return sprintf($valueFormat24Bits,$_[0] & 0xFFFFFF);
}
sub Disassemble_Value_Long {
    return sprintf($valueFormatLong,$_[0] & 0xFFFFFFFF);
}
sub Disassemble_Value_Address {
    return sprintf($valueFormatAddress,$_[0]);
}



1;
