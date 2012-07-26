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
# handle palettes for megadrive/megacd development
###############################################################################

package SCDTools::Palette;
use strict;
use warnings;

my $tmpFile = 'TMP.BIN';
my $tmpFileRemoveAfterUse = 1;
my $constantColorBlack = chr(0x00).chr(0x00);
my $constantColorPink  = chr(0x0E).chr(0x0E);

# [ 0 3 ]
# [ 2 1 ]
my @orderDitherMatrix2x2 = ( 0, 3, 2, 1 );

# [ 0 6 1 7 ]
# [ 4 2 5 3 ]
my @orderDitherMatrix4x2 = ( 0, 6, 1, 7, 4, 2, 5, 3 );

# [ 0 6 ]
# [ 4 2 ]
# [ 1 7 ]
# [ 5 3 ]
my @orderDitherMatrix2x4 = ( 0, 6, 4, 2, 1, 7, 5, 3 );

# [  0 12  3 15 ]
# [  8  4 11  7 ]
# [  2 14  1 13 ]
# [ 10  6  9  5 ]
my @orderDitherMatrix4x4 = ( 0, 12, 3, 15, 8, 4, 11, 7, 2, 14, 1, 13, 10, 6, 9, 5 );

# [  0 48 12 60  3 51 15 63 ]
# [ 32 16 44 28 35 19 47 31 ]
# [  8 56  4 52 11 59  7 55 ]
# [ 40 24 36 20 43 27 39 23 ]
# [  2 50 14 62  1 49 13 61 ]
# [ 34 18 46 30 33 17 45 29 ]
# [ 10 58  6 54  9 57  5 53 ]
# [ 42 26 38 22 41 25 37 21 ]
my @orderDitherMatrix8x8 = ( 0, 48, 12, 60, 3, 51, 15, 63, 32, 16, 44, 28, 35, 19, 47, 31, 8, 56, 4, 52, 11, 59, 7, 55, 40, 24, 36, 20, 43, 27, 39, 23, 2, 50, 14, 62, 1, 49, 13, 61, 34, 18, 46, 30, 33, 17, 45, 29, 10, 58, 6, 54, 9, 57, 5, 53, 42, 26, 38, 22, 41, 25, 37, 21 );

# TODO need a consistent method for color indexing

sub GetHelpString {
   return '[options-palette]
  -asmpal=<file>        output palette to assembly file
  -binpal=<file>        output palette to binary file
  -palfile=<file>       specifies the palette
  -paloffset=<#>        palette offset of the first palette in the palette file
  -palbyteoffset=<#>    byte offset of the first palette in the palette file
  -palcount=<#>         specifies the number of palettes in use
  -magicpink            treat #FF00FF as transparent
  -magicblack           treat #000000 as transparent
  -colorzeronormal      treat color zero of each palette as a normal pixel
  -directcolor          treat palette as 512 colors
  -addcolor=BGR         add VDP hex color to palette
';
}

sub new {
   my($class) = @_;
 
   my $self = bless({}, $class);
 
   $self->{'colors'} = [];
   $self->{'colorsCount'} = 0;
   $self->{'colorsCountRead'} = 0;
   $self->{'colorsMissing'} = {};
   $self->{'entriesPerPalette'} = 16;
   $self->{'colorsPerPalette'} = 15;
   $self->{'paletteCount'} = 1;
   $self->{'fileOutputAsm'} = '';
   $self->{'fileOutputBin'} = '';
   $self->{'fileInput'} = '';
   $self->{'fileInputOffset'} = 0;
   $self->{'magicPink'} = 0;
   $self->{'magicBlack'} = 0;
   $self->{'verbosity'} = 2;
   $self->{'comments'} = 1;
   $self->{'labels'} = 1;
   $self->{'label'} = '';
 
   return $self;
}

sub Reset {
    my ($self) = @_;
    $self->SetMaxColorCount($self->{'colorsCountRead'});
}

sub SetMaxColorCount {
    my ($self,$count) = @_;
    if ( $self->{'colorsCount'} > $count ) {
	splice(@{$self->{'colors'}},$count);
	$self->{'colorsCount'} = $count;
	if ( $count < $self->{'colorsCountRead'} ) {
	    $self->{'colorsCountRead'} = $count;
	}
    }
}

# TODO inconsistent indexing...
sub GetColor {
    my ($self,$index) = @_;
    if ( $index <= $#{$self->{'colors'}} ) {
	return $self->{'colors'}[$index];
    }
    return chr(0x00).chr(0x00);
}


sub GetColorsCountRead {
    my ($self) = @_;
    return $self->{'colorsCountRead'};
}

sub GetColorsCount {
    my ($self) = @_;
    return $self->{'colorsCount'};
}

sub GetColorsPerPalette {
    my ($self) = @_;
    return $self->{'colorsPerPalette'};
}

sub GetPaletteCount {
    my ($self) = @_;
    return $self->{'paletteCount'};
}

sub SetPaletteCount {
    my ($self,$arg) = @_;
    $self->{'paletteCount'} = $arg;
}

sub ParseArg {
    my ($self,$arg) = @_;
    if ( $arg =~ /^-pal(ette)?count=(\d+)$/i ) {
 	$self->{'paletteCount'} = $2;
    } elsif ( $arg =~ /^-asmpal(ette)?=(.*)$/i ) {
 	$self->{'fileOutputAsm'} = $2;
    } elsif ( $arg =~ /^-binpal(ette)?=(.*)$/i ) {
 	$self->{'fileOutputBin'} = $2;
    } elsif ( $arg =~ /^-pal(ette)?file=(.+)$/i ) {
 	$self->{'fileInput'} = $2;
    } elsif ( $arg =~ /^-pal(ette)?byteoffset=(\d+)/i ) {
 	$self->{'fileInputOffset'} = $2;
    } elsif ( $arg =~ /^-pal(ette)?offset=(\d+)$/i ) {
 	$self->{'fileInputOffset'} = 32 * $2;
    } elsif ( $arg =~ /^-pal(ette)?count=(\d+)$/i ) {
 	$self->{'paletteCount'} = $2;
    } elsif ( $arg =~ /^-magicpink$/i ) {
	$self->{'magicPink'} = 1;
    } elsif ( $arg =~ /^-magicblack$/i ) {
	$self->{'magicBlack'} = 1;
    } elsif ( $arg =~ /^-colorzeronormal$/i ) {
	$self->{'colorsPerPalette'} = 16;
	$self->{'entriesPerPalette'} = 16;
    } elsif ( $arg =~ /^-directcolor$/i ) {
	$self->{'colorsPerPalette'} = 512;
	$self->{'entriesPerPalette'} = 512;
    } elsif ( $arg =~ /^-addcolor=([02468ACE]{3})$/i ) {
	my $colorString = $1;
	my $color = chr(hex(substr($colorString,0,1))).chr((hex(substr($colorString,1,1))<<4)|(hex(substr($colorString,2,1))));
	$self->AddColor($color);
	$self->{'colorsCountRead'} = $self->{'colorsCount'};
    } else {
	if ( $arg =~ /^-nocomments$/i ) {
	    $self->{'comments'} = 0;
	} elsif ( $arg =~ /^-nolabels$/i ) {
	    $self->{'labels'} = 0;
	} elsif ( $arg =~ /^-label=(.+)$/i ) {
	    $self->{'label'} = $1;
	} elsif ( $arg =~ /^-v=(\d+)$/i ) {
	    $self->{'verbosity'} = $1;
	}
	return 0;
    }
    return 1;
}

sub ReadPalette {
    my ($self) = @_;
    my $fileInput = $self->{'fileInput'};
    if ( $fileInput eq '' ) {
	return;
    }
    if ( ! -e $fileInput ) {
	die "Bad palette file: ".$fileInput."\n";
    }
    my $tmpFileRemove = 0;
    if ( $fileInput =~ m/\.(asm|68k)$/i ) {
	my $asmFile = $fileInput;
	$fileInput = $tmpFile;
	$tmpFileRemove = $tmpFileRemoveAfterUse;
	system("scdasm -v=".$self->{'verbosity'}." $asmFile ".$fileInput);
	if ( ! -e $fileInput ) {
	    die "Bad scdasm since didn't make: $fileInput";
	}
    }

    if ( $self->{'paletteCount'} <= 0 ) {
	# allow partial palette to count as a whole palette
	$self->{'paletteCount'} = int( ( ( -s $fileInput ) - $self->{'fileInputOffset'} + 2*$self->{'entriesPerPalette'}-1 ) / ( 2 * $self->{'entriesPerPalette'} ) );
    }

    my $paletteCountInBytes = 32 * $self->{'paletteCount'};
    my $paletteData = chr(0x00)x$paletteCountInBytes;

    open( DATA, $fileInput ) or die "Cannot read palette file: $!\n";
    binmode DATA;
    seek(DATA,$self->{'fileInputOffset'},0);
    my $paletteReadCountInBytes = read(DATA,$paletteData,$paletteCountInBytes);
    # for a partial palette, do not add empty colors
    # will add these on output
    for ( my $paletteDataByte = 0; $paletteDataByte < $paletteReadCountInBytes; $paletteDataByte += 2 ) {
	# may add color zero of each palette depending on -colorzeronormal
	if ( ( ( $paletteDataByte >> 1 ) & 0xF ) >= $self->{'entriesPerPalette'} - $self->{'colorsPerPalette'} ) {
	    $self->AddColor(substr($paletteData,$paletteDataByte,2));
	}
    }
    close DATA;

    if ( $tmpFileRemove ) {
	unlink $tmpFile;
    }

    $self->{'colorsCountRead'} = $self->{'colorsCount'};
}

sub OutputPalette {
    my ($self) = @_;
    $self->OutputPaletteForFileAndBinMode($self->{'fileOutputAsm'},0);
    $self->OutputPaletteForFileAndBinMode($self->{'fileOutputBin'},1);
}

sub OutputPaletteForFileAndBinMode {
    my ($self,$file,$bin) = @_;
    if ( $file eq '' ) {
	return;
    }
    my $fileContents = $self->GetOutputPaletteData($bin);
    open(DATA,'>'.$file);
    if ( $bin ) {
	binmode DATA;
    }
    print DATA $fileContents;
    close DATA;
}

sub GetOutputPaletteData {
    my ($self,$bin) = @_;
    if ( $self->{'colorsCount'} <= 0 ) {
	return '';
    }
    my $fileContents = '';
    if ( ! $bin ) {
	if ( $self->{'comments'} ) {
	    $fileContents .= ";; palette\n";
	}
	if ( $self->{'labels'} ) {
	    $fileContents .= "Start".$self->{'label'}."Palette:\n";
	}
    }
    for ( my $paletteIndex = 0; $paletteIndex < $self->{'paletteCount'}; $paletteIndex++ ) {
	for ( my $i = 0; $i < ($self->{'entriesPerPalette'}-$self->{'colorsPerPalette'}); $i++ ) {
	    if ( $bin ) {
		$fileContents .= pack("n",0x0000);
	    } else {   
		$fileContents .= " dc.w \$0000\n";
	    }
	}
	for ( my $i = 0; $i < $self->{'colorsPerPalette'}; $i++ ) {
	    my $colorsIndex = $paletteIndex * $self->{'colorsPerPalette'} + $i;
	    if ( $colorsIndex < $self->{'colorsCount'} ) {
		if ( $bin ) {
		    $fileContents .= $self->{'colors'}[$colorsIndex];
		} else {
		    my $colorf = sprintf("\$%4.4X",unpack("n",$self->{'colors'}[$colorsIndex]));
		    $fileContents .= ' dc.w '.$colorf."\n";
		}
	    } else {
		if ( $bin ) {
		    $fileContents .= pack("n",0x0000);
		} else {
		    $fileContents .= " dc.w \$0000\n";
		}
	    }
	}
    }
    if ( ! $bin && $self->{'labels'} ) {
	$fileContents .= "End".$self->{'label'}."Palette:\n";
    }
    return $fileContents;
}

my %cache;

sub GetColorIndex {
    my ($self,$color) = @_;
    my $index = -1;
    if ( ( $self->{'magicPink'} && $color eq $constantColorPink ) ||
	 ( $self->{'magicBlack'} && $color eq $constantColorBlack ) ) {
	$index = 0;
    } else {
	$index = $self->GetColorIndexWithNoAdding($color);
	if ( $index < 0 ) {
	    $index = $self->AddColor($color);
	}
    }
    return $index;
}

sub GetColorIndexWithNoAdding {
    my ($self,$color) = @_;
    my $index = -1;
    my $colorsCount = $self->{'colorsCount'};
    for ( my $colorsIndex = 0; $colorsIndex < $colorsCount; $colorsIndex++ ) {
	if ( $color eq $self->{'colors'}[$colorsIndex] ) {
	    # account for each palette having unused transparent index
	    $index = ( $colorsIndex % $self->{'colorsPerPalette'} ) + $self->{'entriesPerPalette'} * int($colorsIndex / $self->{'colorsPerPalette'}) + ($self->{'entriesPerPalette'}-$self->{'colorsPerPalette'});
	    last;
	}
    }
    return $index;
}

sub AddColor {
    my ($self,$color) = @_;
    my $index = -1;
    if ( $self->{'colorsCount'} < $self->{'colorsPerPalette'} * $self->{'paletteCount'} ) {
	push @{$self->{'colors'}}, $color;
	my $colorsIndex = $self->{'colorsCount'};
	$index = ( $colorsIndex % $self->{'colorsPerPalette'} ) + $self->{'entriesPerPalette'} * int($colorsIndex / $self->{'colorsPerPalette'}) + ($self->{'entriesPerPalette'}-$self->{'colorsPerPalette'});
	$self->{'colorsCount'}++;
	# debug print
	# printf("Adding color $index : \$%4.4X\n",unpack("n",$color));
    } else {
	$self->{'colorsMissing'}{$color}++;
	$index = -1;
    }
    return $index;
}

sub AddColorsFromImageMagickImage {
    my ($self,$img) = @_;
    use Image::Magick;

    my $width = $img->Get('width');
    my $height = $img->Get('height');
    my $alpha = $img->Get('matte');

    # used to keep track of frequency of colors
    my @colors;

    for ( my $i = 0; $i < 8*8*8; $i++ ) {
	$colors[$i] = 0;
    }

    for ( my $y = 0; $y < $height; $y++ ) {
	for ( my $x = 0; $x < $width; $x++ ) {
	    my $a = 0;
	    if ( $alpha ) {
		$a = $img->GetPixel('channel'=>'Alpha','normalize'=>1,'x'=>$x,'y'=>$y);
		# TODO 95% or 100% or something else?
		if ( defined $a && $a > 0.95 ) {
		    next;
		}
	    }
	    my ($r,$g,$b) = $img->GetPixel('channel'=>'RGB','normalize'=>1,'x'=>$x,'y'=>$y);
	    # 15% performance optimization: use simpler format to store color
	    my $colorInt = &SCDTools::Palette::ConvertRGBNormalizedToVDPColorInt($r,$g,$b);
	    $colors[$colorInt]++;
	}
    }

    # add colors in order of most used to least used
    foreach my $colorInt ( sort { $colors[$b] <=> $colors[$a] || $a <=> $b } (0..8*8*8-1) ) {
	if ( $colors[$colorInt] == 0 ) {
	    last;
	}
	my $color = &SCDTools::Palette::ConvertVDPColorIntToVDPColor($colorInt);
	$self->GetColorIndex($color);
    }
}

sub UpdateImageMagickPalette {
    my ($self,$imgPalette) = @_;
    use Image::Magick;

    my $colorsCount = $self->{'colorsCount'};

    $imgPalette->Set(size=>"${colorsCount}x1");
    my $x = $imgPalette->ReadImage('xc:black');
    warn $x if $x;

    for ( my $colorsIndex = 0; $colorsIndex < $colorsCount; $colorsIndex++ ) {
	my $color = $self->{'colors'}[$colorsIndex];
	my @pixels = &ConvertVDPColorToRGBNormalized($color);
	$imgPalette->SetPixel('channel'=>'RGB','x'=>$colorsIndex,'y'=>0,'color'=>\@pixels);
    }
}

sub Convert {
    my ($self,$type) = @_;
    my $colorsCount = $self->{'colorsCount'};
    for ( my $colorsIndex = 0; $colorsIndex < $colorsCount; $colorsIndex++ ) {
	my $cOld = $self->{'colors'}[$colorsIndex];
	my $cNew;
	if ( $type eq 'greyavg' ) {
	    my ($r,$g,$b) = &ConvertVDPColorToRGBNormalized($cOld);
	    my $avg = ($r+$g+$b)/3.0;
	    $cNew = &ConvertRGBNormalizedToVDPColor($avg,$avg,$avg);
	} elsif ( $type eq 'desaturate50' ) {
	    my ($r,$g,$b) = &ConvertVDPColorToRGBNormalized($cOld);
	    my $avg = ($r+$g+$b)/3.0;
	    $cNew = &ConvertRGBNormalizedToVDPColor(0.5*($r+$avg),0.5*($g+$avg),0.5*($b+$avg));
	} elsif ( $type eq 'invert' ) {
	    my ($r,$g,$b) = &ConvertVDPColorToRGBNormalized($cOld);
	    my $avg = ($r+$g+$b)/3.0;
	    $cNew = &ConvertRGBNormalizedToVDPColor(1.0-$r,1.0-$g,1.0-$b);
	} elsif ( $type eq 'redonly' ) {
	    my ($r,$g,$b) = &ConvertVDPColorToRGBNormalized($cOld);
	    my $avg = ($r+$g+$b)/3.0;
	    $cNew = &ConvertRGBNormalizedToVDPColor($r,0,0);
	} elsif ( $type eq 'greenonly' ) {
	    my ($r,$g,$b) = &ConvertVDPColorToRGBNormalized($cOld);
	    my $avg = ($r+$g+$b)/3.0;
	    $cNew = &ConvertRGBNormalizedToVDPColor(0,$g,0);
	} elsif ( $type eq 'blueonly' ) {
	    my ($r,$g,$b) = &ConvertVDPColorToRGBNormalized($cOld);
	    my $avg = ($r+$g+$b)/3.0;
	    $cNew = &ConvertRGBNormalizedToVDPColor(0,0,$b);
	} else {
	    $cNew = $cOld;
	}
	$self->{'colors'}[$colorsIndex] = $cNew;
    }
}

sub ConvertRGBNormalizedToVDPColorInt {
    my ($r,$g,$b) = @_;
    return (int((255*$b)>>5)<<6)|(int((255*$g)>>5)<<3)|(int((255*$r)>>5));
}

sub ConvertVDPColorIntToVDPColor {
    my ($colorInt) = @_;
    return chr(0x0E & ($colorInt>>5)).chr((0xE0 & ($colorInt<<2))|(0x0E & ($colorInt<<1)));
}

sub ConvertRGBNormalizedToVDPColor {
    my ($r,$g,$b) = @_;
    return chr(int((255*$b)>>5)<<1).chr((int((255*$g)>>5)<<5)|(int((255*$r)>>5)<<1));
}

sub ConvertVDPColorToRGBNormalized {
    my ($c) = @_;
    # TODO inverse it
    # pixel: 0-7
    # (pixel*32/255) is lowest part
    # add 1/16 to get to highlight
    # add 1/32 to get halway between regular and highlight
    return ((ord(substr($c,1,1))&0xE)/15.0,((ord(substr($c,1,1))>>4)&0xE)/15.0,(ord(substr($c,0,1))&0xE)/15.0);
}

sub ConvertVDPColorToRGB24Bit {
    my ($c) = @_;
    return chr(((ord(substr($c,1,1))&0xE)/15.0)*255).chr((((ord(substr($c,1,1))>>4)&0xE)/15.0)*255).chr(((ord(substr($c,0,1))&0xE)/15.0)*255);
}

sub PrintMissingColors {
    my ($self) = @_;
    my @colors = sort {$self->{'colorsMissing'}{$b} <=> $self->{'colorsMissing'}{$a}} keys %{$self->{'colorsMissing'}};
    foreach my $color (@colors) {
	my $count = $self->{'colorsMissing'}{$color};
	my $colorf = sprintf("\$%4.4X",unpack("n",$color));
	print STDERR "Palette does not contain color: $colorf ($count)\n";
    }
}

sub GetOrderedDitherAdd {
    # returns value between 0 and 255 or -1 if invalid format
    my ($format,$x,$y) = @_;
    my $orderedDitherAdd = 0;
    if ( $format eq '8x8' ) {
	$orderedDitherAdd = $orderDitherMatrix8x8[(8*($y & 7)+($x & 7))] << 2;
    } elsif ( $format eq '4x4' ) {
	$orderedDitherAdd = $orderDitherMatrix4x4[(4*($y & 3)+($x & 3))] << 4;
    } elsif ( $format eq '4x2' ) {
	$orderedDitherAdd = $orderDitherMatrix4x2[(4*($y & 1)+($x & 3))] << 5;
    } elsif ( $format eq '2x4' ) {
	$orderedDitherAdd = $orderDitherMatrix2x4[(2*($y & 3)+($x & 1))] << 5;
    } elsif ( $format eq '2x2' ) {
	$orderedDitherAdd = $orderDitherMatrix2x2[(2*($y & 1)+($x & 1))] << 6;
    } else {
	return -1;
    }
    return $orderedDitherAdd;
}

1;

